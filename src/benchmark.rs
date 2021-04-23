use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::Read;
use std::process::{Command, Stdio};
use std::time::Duration;
use wait_timeout::ChildExt;

/// Several outcomes involve running the program before and after migration.
/// Those outcomes have a steps field. The program is expected to terminate
/// in at most the given number of steps, or we have an unexpected outcome.
#[derive(Debug, Serialize, Deserialize)]
struct Outcome {
    #[serde(default, skip_serializing_if = "is_false")]
    assert_unusable: bool,
    #[serde(default, skip_serializing_if = "is_none")]
    expect_compatible: Option<String>,
    result: Option<Expect>,
    #[serde(default, skip_serializing_if = "is_none")]
    migration: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
enum Expect {
    Rejection(Rejection),
    NewRuntimeError,
    Unusable,
    FullyCompatible {
        num_stars: usize,
        #[serde(skip_serializing_if = "is_false", default)]
        manually_verify: bool,
    },
    Disaster,
    Restricted {
        num_stars: usize,
    },
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Rejection {
    stdout: String,
    stderr: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct RuntimeError {
    /// Output from the tool
    message: Option<String>,
    program: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Benchmark {
    file: String,
    #[serde(skip_serializing_if = "is_none")]
    context: Option<String>,
    #[serde(default)]
    results: std::collections::HashMap<String, Outcome>,
    #[serde(default)] // default is zero
    num_stars: usize,
}

#[derive(Debug, Serialize, Deserialize)]
struct MigrationTool {
    title: String,
    command: Vec<String>,
}
#[derive(Debug, Serialize, Deserialize)]
struct Benchmarks {
    tools: Vec<MigrationTool>,
    benchmarks: Vec<Benchmark>,
}

fn is_false(b: &bool) -> bool {
    return !b;
}

fn is_none<T>(v: &Option<T>) -> bool {
    return v.is_none();
}

fn count_stars(e: &super::syntax::Exp) -> usize {
    use super::syntax::{Exp, Typ};
    match e {
        Exp::Lit(..) | Exp::Var(..) => 0,
        Exp::App(e1, e2) | Exp::BinaryOp(_, e1, e2) => count_stars(e1) + count_stars(e2),
        // If we introduce an annotation, we get an extra star! This can produce surprising results
        // For example, the original program `1 + true` has zero stars, but after migration, we get
        // `1 + true as any`, which has 1 star.
        Exp::Ann(e, t) | Exp::Fun(_, t, e) => {
            (match t {
                Typ::Any => 1,
                _ => 0,
            }) + count_stars(e)
        }
        Exp::If(e1, e2, e3) => count_stars(e1) + count_stars(e2) + count_stars(e3),
        _ => panic!("count_stars on {:?}", e),
    }
}

fn get_outcome<'a>(
    tool_name: &str,
    results: &'a mut std::collections::HashMap<String, Outcome>,
) -> &'a mut Outcome {
    if results.contains_key(tool_name) == false {
        results.insert(
            tool_name.to_string(),
            Outcome {
                assert_unusable: false,
                expect_compatible: None,
                result: None,
                migration: None,
            },
        );
    }
    return results.get_mut(tool_name).unwrap();
}

// Run the program after coercion insertion. True means it ran successfully.
// False means a coercion error occurred. Anything else causes a panic.
fn eval(code: String, num_stars: Option<&mut usize>) -> Option<bool> {
    match super::parser::parse(code) {
        Ok(mut ast) => {
            if let Some(num_stars) = num_stars {
                *num_stars = count_stars(&ast);
            }
            super::insert_coercions::insert_coercions(&mut ast).expect("coercion insertion failed");
            Some(super::eval::eval(ast).is_ok())
        }
        Err(_messages) => None,
    }
}

fn benchmark_one(tool: &MigrationTool, benchmark: &mut Benchmark) {
    let mut child = Command::new(&tool.command[0])
        .args(&tool.command[1..])
        .arg(&benchmark.file)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn");
    let migrate_ok = match child.wait_timeout(Duration::from_secs(30)).unwrap() {
        None => {
            child.kill().unwrap();
            eprintln!("Killed");
            false
        }
        Some(code) => code.success(),
    };
    let mut tool_stdout = String::new();
    child
        .stdout
        .unwrap()
        .read_to_string(&mut tool_stdout)
        .unwrap();
    let mut tool_stderr = String::new();
    child
        .stderr
        .unwrap()
        .read_to_string(&mut tool_stderr)
        .unwrap();

    // let output = child.wait_with_output().expect("failed to wait for output");
    // let tool_stdout = String::from_utf8_lossy(&output.stdout).to_string();
    // let tool_stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let mut outcome = get_outcome(&tool.title, &mut benchmark.results);

    if migrate_ok == false {
        outcome.result = Some(Expect::Rejection(Rejection {
            stdout: tool_stdout,
            stderr: tool_stderr,
        }));
        return;
    }

    // For us to manually check the result of migration
    outcome.migration = Some(tool_stdout.clone());
    let original_program = std::fs::read_to_string(&benchmark.file).expect("reading benchmark");
    let original_runs_ok = eval(original_program.clone(), Some(&mut benchmark.num_stars));
    let mut stars_after_migration = 0;
    let migrated_runs_ok = eval(tool_stdout.clone(), Some(&mut stars_after_migration));

    let result_is_known_compatible = outcome
        .expect_compatible
        .as_ref()
        .map(|s| s.trim() == tool_stdout.trim())
        .unwrap_or(false);

    match &benchmark.context {
        None => match (original_runs_ok, migrated_runs_ok) {
            (None, _) => outcome.result = Some(Expect::Disaster),
            (_, None) => outcome.result = Some(Expect::Disaster),
            (Some(true), Some(false)) => {
                outcome.result = Some(Expect::NewRuntimeError);
            }
            (Some(true), Some(true)) => {
                outcome.result = Some(Expect::FullyCompatible {
                    num_stars: stars_after_migration,
                    manually_verify: benchmark.num_stars != stars_after_migration
                        && !result_is_known_compatible,
                });
            }
            (Some(false), Some(false)) => {
                outcome.result = Some(Expect::FullyCompatible {
                    num_stars: stars_after_migration,
                    manually_verify: false,
                });
            }
            (Some(false), Some(true)) => {
                panic!("Migration eliminated an error!");
            }
        },
        Some(context) => {
            let original_in_context = context.replace("HOLE", &original_program);
            let migrated_in_context = context.replace("HOLE", &tool_stdout);
            let original_runs_ok_in_context = eval(original_in_context, None);
            let migrated_runs_ok_in_context = eval(migrated_in_context, None);
            match (
                original_runs_ok,
                migrated_runs_ok,
                original_runs_ok_in_context,
                migrated_runs_ok_in_context,
            ) {
                (Some(true), Some(true), Some(true), Some(false)) => {
                    if outcome.assert_unusable {
                        outcome.result = Some(Expect::Unusable);
                    } else {
                        outcome.result = Some(Expect::Restricted {
                            num_stars: stars_after_migration,
                        });
                    }
                }
                (Some(true), Some(true), Some(true), Some(true)) => {
                    outcome.result = Some(Expect::FullyCompatible {
                        num_stars: stars_after_migration,
                        manually_verify: benchmark.num_stars != stars_after_migration
                            && !result_is_known_compatible,
                    });
                }
                _ => {
                    outcome.result = Some(Expect::Disaster);
                }
            }
        }
    }
}

fn summarize(benchmarks: &Benchmarks) {
    let mut rejected = HashMap::<String, i32>::new();
    let mut new_runtime_err = HashMap::<String, i32>::new();
    let mut unusable = HashMap::<String, i32>::new();
    let mut restricted = HashMap::<String, i32>::new();
    let mut compatible = HashMap::<String, i32>::new();
    let mut num_stars_left = HashMap::<String, i32>::new();
    let mut num_original_stars = HashMap::<String, i32>::new();
    for tool in &benchmarks.tools {
        rejected.insert(tool.title.clone(), 0);
        new_runtime_err.insert(tool.title.clone(), 0);
        unusable.insert(tool.title.clone(), 0);
        restricted.insert(tool.title.clone(), 0);
        compatible.insert(tool.title.clone(), 0);
        num_stars_left.insert(tool.title.clone(), 0);
        num_original_stars.insert(tool.title.clone(), 0);
    }

    for b in &benchmarks.benchmarks {
        for (tool_title, outcome) in &b.results {
            match outcome.result {
                Some(Expect::Rejection(..)) => {
                    *rejected.get_mut(tool_title).unwrap() += 1;
                }
                Some(Expect::NewRuntimeError) => {
                    *new_runtime_err.get_mut(tool_title).unwrap() += 1;
                }
                Some(Expect::Unusable) => {
                    *unusable.get_mut(tool_title).unwrap() += 1;
                }
                Some(Expect::FullyCompatible { num_stars, .. }) => {
                    *num_stars_left.get_mut(tool_title).unwrap() += num_stars as i32;
                    *num_original_stars.get_mut(tool_title).unwrap() += b.num_stars as i32;
                    *compatible.get_mut(tool_title).unwrap() += 1;
                }
                Some(Expect::Restricted { num_stars }) => {
                    *num_stars_left.get_mut(tool_title).unwrap() += num_stars as i32;
                    *num_original_stars.get_mut(tool_title).unwrap() += b.num_stars as i32;
                    *restricted.get_mut(tool_title).unwrap() += 1;
                }
                Some(Expect::Disaster) => {}
                None => {
                    panic!("missing outcome");
                }
            }
        }
    }

    let num_benchmarks = benchmarks.benchmarks.len();

    for tool in &benchmarks.tools {
        let title = &tool.title;
        let rejected = rejected.get(title).unwrap();
        let rejected_denom = num_benchmarks as i32;
        let new_runtime_err = new_runtime_err.get(title).unwrap();
        let new_runtime_err_denom = rejected_denom - rejected;
        let unusable = unusable.get(title).unwrap();
        let unusable_denom = new_runtime_err_denom - new_runtime_err;
        let restricted = restricted.get(title).unwrap();
        let restricted_denom = unusable_denom - unusable;
        let stars = num_stars_left.get(title).unwrap();
        let stars_denom = num_original_stars.get(title).unwrap();
        println!(
            "{} & {} / {} & {} / {} & {} / {} &  {} / {} & {} / {} \\\\ ",
            title,
            rejected,
            rejected_denom,
            new_runtime_err,
            new_runtime_err_denom,
            unusable,
            unusable_denom,
            restricted,
            restricted_denom,
            stars,
            stars_denom
        );
    }
}

pub fn benchmark_main(src_file: impl AsRef<str>) -> Result<(), std::io::Error> {
    let src_text = std::fs::read_to_string(src_file.as_ref())?;
    let mut benchmarks: Benchmarks = serde_yaml::from_str(&src_text).expect("syntax error");
    for mut b in benchmarks.benchmarks.iter_mut() {
        for t in &benchmarks.tools {
            eprintln!("Running {} on {} ...", t.title, b.file);
            benchmark_one(&t, &mut b);
        }
    }

    println!("{}", serde_yaml::to_string(&benchmarks).unwrap());

    summarize(&benchmarks);
    return Ok(());
}

pub fn details_latex(src_file: impl AsRef<str>) -> Result<(), std::io::Error> {
    let src_text = std::fs::read_to_string(src_file.as_ref())?;
    let benchmarks: Benchmarks = serde_yaml::from_str(&src_text).expect("syntax error");
    for b in benchmarks.benchmarks {
        println!("\\subsection*{{{}}}\n", &b.file.replace("_", "-"));
        for t in &benchmarks.tools {
            let result = b.results.get(&t.title).unwrap();
            let migration = result
                .migration
                .as_ref()
                .map(|s| s.replace("⦉", "t").replace("⦊", "").clone())
                .unwrap_or("".to_string());
            let outcome_str = match result.result.as_ref().unwrap() {
                Expect::Disaster => "\\textbf{DISASTER}",
                Expect::FullyCompatible { .. } => "Compatible",
                Expect::NewRuntimeError => "Runtime Error",
                Expect::Rejection { .. } => "Rejected",
                Expect::Unusable => "Unusable",
                Expect::Restricted { .. } => "Restricted",
            };
            println!("\\paragraph{{{}}}: {}", &t.title, outcome_str);
            println!("\\begin{{lstlisting}}");
            println!("{}", migration);
            println!("\\end{{lstlisting}}\n");
        }
    }
    return Ok(());
}

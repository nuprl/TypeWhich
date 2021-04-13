mod benchmark;
mod cgen;
mod eval;
mod grift;
mod ins_and_outs;
mod insert_coercions;
mod parser;
mod pretty;
mod syntax;
mod type_check;
mod z3_state;

use clap::Clap;
use std::io::*;

#[derive(Clap)]
enum Parser {
    Empty,
    Grift,
}

#[derive(Clone, Copy, PartialEq)]
enum Annot {
    Ignore,
    Hard,
}

impl std::str::FromStr for Annot {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "ignore" => Ok(Annot::Ignore),
            "hard" => Ok(Annot::Hard),
            _ => Err("invalid annotation behavior"),
        }
    }
}

impl std::str::FromStr for Parser {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "grift" => Ok(Parser::Grift),
            "empty" => Ok(Parser::Empty),
            _ => Err("invalid parser"),
        }
    }
}

#[derive(Clap)]
#[clap(name = env!("CARGO_PKG_NAME"), version = env!("CARGO_PKG_VERSION"))]
struct TopLevel {
    #[clap(subcommand)]
    sub_command: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Migrate(Opts),
    Eval(EvalOpts),
    Benchmark(BenchmarkOpts),
}

#[derive(Clap)]
struct EvalOpts {
    input: String,
    #[clap(short = 'c', long)]
    show_inserted_coercions: bool,
}

#[derive(Clap)]
struct BenchmarkOpts {
    input: String,
}

#[derive(Clap)]
pub struct Opts {
    /// Input file (defaults to '-', meaning STDIN)
    #[clap(index = 1, default_value = "-")]
    input: String,
    /// Print debugging output
    #[clap(short, long)]
    debug: bool,
    /// Disable the optimizer, which uses 'assert_soft' to reduce the number of
    /// coercions.
    #[clap(long = "no-optimize")]
    disable_optimizer: bool,
    /// Produce an exact type that may not be safe in all contexts
    #[clap(long = "unsafe")]
    unsafe_mode: bool,
    // Select the parser
    #[clap(short, long, default_value = "empty")]
    parser: Parser,
    /// Use a predefined environment; when '-p grift' is set, will default to
    /// 'grift', otherwise it will be 'empty'
    #[clap(
        short,
        long,
        default_value_if("parser", Some("grift"), "grift"),
        default_value("empty")
    )]
    env: Parser,
    /// Specifies behavior on type annotations; when '-p grift' is set, will
    /// default to 'ignore'.
    #[clap(
        short,
        long,
        default_value_if("parser", Some("grift"), "ignore"),
        default_value("ignore")
    )]
    annot: Annot,
    /// Use ins and outs. Lots of features unsupported in this mode.
    #[clap(long)]
    ins_and_outs: bool,
    /// Provide a file and we will ROUGHLY compare our migration to the
    /// provided program's types
    #[clap(long)]
    compare: Option<String>,
}

#[derive(Clone, Copy)]
pub struct Options {
    optimizer: bool,
    context: bool,
    debug: bool,
    annot: Annot,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            optimizer: true,
            context: true,
            debug: false,
            annot: Annot::Hard,
        }
    }
}

fn main() -> Result<()> {
    let top_level = TopLevel::parse();
    match top_level.sub_command {
        SubCommand::Migrate(opts) => migrate_main(opts),
        SubCommand::Eval(opts) => eval_main(opts),
        SubCommand::Benchmark(opts) => benchmark::benchmark_main(&opts.input),
    }
}

fn eval_main(opts: EvalOpts) -> Result<()> {
    let src_txt = std::fs::read_to_string(opts.input)?;
    let mut src_ast = parser::parse(&src_txt).expect("parse error");
    insert_coercions::insert_coercions(&mut src_ast).unwrap();
    if opts.show_inserted_coercions {
        println!("With coercions:\n{}", &src_ast);
    }
    match eval::eval(src_ast) {
        Ok(_) => println!("OK"),
        Err(s) => println!("{}", s),
    };
    Ok(())
}

fn migrate_main(config: Opts) -> Result<()> {
    let options = Options {
        optimizer: !config.disable_optimizer,
        context: !config.unsafe_mode,
        debug: config.debug,
        annot: config.annot,
    };

    let env = match config.env {
        Parser::Grift => grift::env(),
        _ => Default::default(),
    };
    let source = match config.input.as_str() {
        "-" => {
            let mut out = String::new();
            stdin().read_to_string(&mut out)?;
            out
        }
        file => std::fs::read_to_string(file)?,
    };

    let mut parsed = match config.parser {
        Parser::Empty => parser::parse(&source).unwrap(),
        Parser::Grift => grift::parse(&source),
    };

    if options.annot == Annot::Ignore {
        parsed.fresh_types();
    }

    if options.debug {
        eprintln!("Parsed program:");
        eprintln!("{}", parsed);
    }
    let inferred = if config.ins_and_outs {
        parsed.fresh_types();
        ins_and_outs::typeinf_portable(parsed)
    } else {
        cgen::typeinf_options(parsed, &env, options).unwrap()
    };

    if options.debug {
        eprintln!("Annotated program:");
        eprintln!("{}", inferred);
    }

    let typ = type_check::tcheck(&env, &inferred)
        .map_err(|e| Error::new(ErrorKind::Other, format!("{}", e)))?;
    if options.debug {
        eprintln!("Inferred type:");
        eprintln!("{}", typ);
    }

    match config.compare {
        None => {
            println!("{}", &inferred);
            Ok(())
        }
        Some(f) => {
            let compare_to_str = std::fs::read_to_string(f)?;
            let compare_to = grift::parse(&compare_to_str);
            match inferred.matches_roughly(&compare_to) {
                Ok(()) => {
                    println!("MATCHES");
                    Ok(())
                }
                Err(e) => {
                    println!("{}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests_631 {
    use super::cgen::typeinf_options;
    use super::parser::parse;
    use super::syntax::{Coerce, Exp, Typ};
    use super::type_check::type_check;
    use super::Options;
    trait PairOr {
        fn or(&self, other: Self) -> Self;
    }
    impl PairOr for (bool, bool) {
        fn or(&self, other: Self) -> Self {
            (self.0 || other.0, self.1 || other.1)
        }
    }
    fn coerce_contains_coercions(c: Coerce) -> (bool, bool) {
        match c {
            Coerce::Doomed => (false, true),
            Coerce::Id => (false, false),
            Coerce::Seq(a, b) => coerce_contains_coercions(*a).or(coerce_contains_coercions(*b)),
            Coerce::Tag(_) => (true, false),
            Coerce::Untag(_) => (false, true),
            Coerce::Wrap(..) => panic!("wrap shouldn't happen in TypeWhich"),
        }
    }
    // (to_any, from_any)
    pub fn contains_coercions(e: Exp) -> (bool, bool) {
        match e {
            Exp::PrimCoerce(c, e) => contains_coercions(*e).or(coerce_contains_coercions(c)),
            Exp::Coerce(t1, t2, e) => {
                let cts = contains_coercions(*e);
                if t1 == t2 {
                    // this probably shouldn't happen after proper annotation
                    cts
                } else {
                    // a coercion between two non-anys counts as a from_any
                    // because it is possibly unsafe (which is what we really
                    // mean by from_any)
                    (t2 == Typ::Any, t2 != Typ::Any)
                }
            }
            Exp::Lit(..) | Exp::Var(..) | Exp::Empty(..) => (false, false),
            Exp::Fun(_, _, e)
            | Exp::Fix(_, _, e)
            | Exp::Ann(e, _)
            | Exp::Fst(e)
            | Exp::Snd(e)
            | Exp::Head(e)
            | Exp::Tail(e)
            | Exp::Not(e)
            | Exp::Box(e)
            | Exp::Unbox(e)
            | Exp::IsEmpty(e)
            | Exp::IsBool(e)
            | Exp::IsInt(e)
            | Exp::IsString(e)
            | Exp::IsList(e)
            | Exp::IsFun(e)
            | Exp::VectorLen(e) => contains_coercions(*e),
            Exp::App(e1, e2)
            | Exp::Add(e1, e2)
            | Exp::AddOverload(e1, e2)
            | Exp::Mul(e1, e2)
            | Exp::IntEq(e1, e2)
            | Exp::Cons(e1, e2)
            | Exp::Pair(e1, e2)
            | Exp::BoxSet(e1, e2)
            | Exp::Let(.., e1, e2)
            | Exp::Vector(e1, e2)
            | Exp::VectorRef(e1, e2) => contains_coercions(*e1).or(contains_coercions(*e2)),
            Exp::If(e1, e2, e3) | Exp::VectorSet(e1, e2, e3) => contains_coercions(*e1)
                .or(contains_coercions(*e2))
                .or(contains_coercions(*e3)),
            Exp::LetRec(bindings, e) => bindings
                .into_iter()
                .fold(contains_coercions(*e), |cc, (_, _, ei)| {
                    cc.or(contains_coercions(ei))
                }),
        }
    }
    pub fn succeeds(program: &str) -> Typ {
        exp_succeeds(parse(program).unwrap())
    }
    pub fn no_from_any(program: &str) {
        let orig = parse(program).unwrap();
        let (_, e) = compile_verbose(orig);
        let coercions = contains_coercions(e);
        assert!(!coercions.1);
    }
    pub fn coerces(program: &str) -> Typ {
        exp_coerces(parse(program).unwrap())
    }
    fn compile_verbose(mut orig: Exp) -> (Typ, Exp) {
        orig.fresh_types();
        println!("\nOriginal program:\n{}", &orig);
        let mut options = Options::default();
        options.debug = true;
        let e = typeinf_options(orig, &Default::default(), options).unwrap();
        println!("\nAfter type inference:\n{}", e);
        let t = type_check(&e).expect("failed to typecheck");
        println!("\nProgram type:\n{}", t);
        (t, e)
    }
    pub fn exp_succeeds(orig: Exp) -> Typ {
        let (t, e) = compile_verbose(orig);
        let coercions = contains_coercions(e);
        assert!(!coercions.0 && !coercions.1);
        t
    }
    pub fn exp_coerces(orig: Exp) -> Typ {
        let (t, e) = compile_verbose(orig);
        let coercions = contains_coercions(e);
        assert!(coercions.0 || coercions.1);
        t
    }
    #[test]
    fn addition() {
        succeeds("200 + 9101");
    }
    #[test]
    fn num_plus_bool() {
        coerces("1 + true");
    }
    /// this isn't really what the 631 test was saying, but it's added here to
    /// make sure the above isn't a bug
    #[test]
    fn num_plus_bool_janky() {
        coerces("1 +? true");
    }
    #[test]
    fn indir_int_equal_bool() {
        coerces(
            "fun p .
                (fun foo . foo 10 p true)
                    (fun x . fun y . fun z .
                        if true then z
                        else
                            (fun w . w) (if true then y else (fun w0 . w0) x))",
        );
    }
    #[test]
    fn key_is_bool_and_int() {
        // previously ended in else key > 10 which would yield bool and
        // constrain key to int, so we make something similar type-wise without
        // adding comparisons
        coerces(
            "fun key . if true then if true then key else true else
                (fun i . true) (key + 10)",
        );
    }
    #[test]
    fn lots_of_conditionals() {
        coerces(
            "fun x . fun y . fun z .
               (if true then x else y) :: (if true then y else z) ::
                  (if true then z else (fun w . w) 5) :: (if x then empty else empty)",
        );
    }
    #[test]
    fn bool_const() {
        succeeds("true");
    }
    #[test]
    fn list_of_booleans() {
        succeeds("true :: empty");
    }
    #[test]
    fn list_of_numbers() {
        succeeds("100 :: empty");
    }
    #[test]
    fn factorial() {
        // should be if n == 0 instead of if false but it's probably not a
        // particularly important operation to have
        succeeds(
            "let fac = fix fac . fun n . if false then 1 else n * fac (n + -1) in
             fac 50 + fac 100",
        );
    }
    #[test]
    fn extract_list() {
        succeeds("head (2 :: empty) + 5");
    }
    #[test]
    fn identity_polymorphic() {
        coerces(
            "let id = fun x . x in
            let f = fun anid .
                let n = id 10 in
                let b = id true in
                5 in
            f id",
        );
    }
    #[test]
    fn simple_arith() {
        succeeds("(fun x . x + 1) 10");
    }
    #[test]
    fn numeric_const() {
        succeeds("908");
    }
    #[test]
    fn is_empty_number() {
        coerces("is_empty 500");
    }
    #[test]
    fn is_empty_list() {
        succeeds("is_empty (1 :: empty)");
    }
    #[test]
    fn real_map() {
        succeeds(
            "let map = fix map . fun f . fun lst .
               if is_empty(lst) then
                 empty
               else
                 f(head(lst)) :: (map f (tail(lst))) in
               map (fun n . n + 1) (1 :: 2 :: 3 :: empty)",
        );
    }
    #[test]
    fn bogus_map() {
        succeeds(
            "let map = fun f . fun lst .
               f(head(lst)) :: f(head(tail(lst))) :: empty in
                   map (fun n . n + 1) (1 :: 2 :: 3 :: empty)",
        );
    }
    // = not yet supported: extract a value from a record =
    // = not yet supported: extract a value from a non-record =
    #[test]
    fn double() {
        succeeds(
            "let square = fun n . if false then 0 else n + n in
            square 10 + square 5",
        );
    }
    #[test]
    fn tail_wag() {
        succeeds("12 :: (tail (12 :: empty))");
    }
    #[test]
    fn tail_toggle() {
        succeeds("tail (1 :: empty)");
    }
    // = not yet supported: arrays are homogenous =
    #[test]
    fn dyn_list_single_level() {
        coerces("1 :: (false :: empty)");
    }
    #[test]
    fn dyn_list_nested() {
        coerces("1 :: (false :: ((2 :: (true :: empty)) :: empty))");
    }
    #[test]
    fn flatten_body() {
        coerces(
            "let flatten = fun append . fun f . fun x .
               if is_list x then append (f (head x)) (f (tail x)) else x :: empty in
               let l = 1 :: (false :: ((2 :: (true :: empty)) :: empty)) in
               flatten (fun x . fun y. x) (fun x. x) l",
        );
    }
}

#[cfg(test)]
mod tests_migeed_and_parsberg {
    use super::cgen::typeinf;
    use super::parser::parse;
    use super::tests_631::coerces;
    use super::type_check::type_check;

    // TODO(arjun): _maximal in the name is not accurate. Alternative name:
    // assert_ti_ok
    fn assert_maximal(program: &str, annotated: &str) {
        let mut orig = parse(program).unwrap();
        orig.fresh_types();
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(orig).expect("type inference failed on the original program");
        println!("\nAfter type inference:\n{}", e);
        let correct = typeinf(parse(annotated).unwrap())
            .expect("type inference failed on the expected program");
        println!(
            "\nProgram type:\n{}",
            type_check(&e).expect("failed to typecheck")
        );
        println!("\nCorrect:\n{}", correct);
        assert_eq!(e, correct);
    }
    #[test]
    #[ignore]
    fn apply_add() {
        assert_maximal("fun x . x (x + 1)", "fun x: any . x (x + 1)");
    }

    #[test]
    #[ignore]
    fn add_applied() {
        // TODO(arjun): We get a different result. Worth discussing.
        assert_maximal(
            "fun x             . x ((x true) + 1)",
            "fun x: any -> int . x ((x true) + 1)",
        );
    }

    #[test]
    #[ignore]
    fn add_two_applies() {
        // TODO(arjun): We get a different result. Worth discussing.
        assert_maximal(
            "fun x             . x 4 + x true",
            "fun x: any -> int . x 4 + x true",
        );
    }
    #[test]
    fn identity_four() {
        assert_maximal("(fun x . x) 4", "(fun x: int . x) 4");
    }

    #[test]
    #[ignore]
    fn succ_id_id() {
        // TODO(luna): We get a different result, in part because we don't
        // allow from_any coercions on arguments
        assert_maximal(
            "1 + ((fun y    .y) ((fun x    .x) true))",
            "1 + ((fun y:int.y) (from_any ((fun x:any.x) true)))",
        );
    }
    #[test]
    fn identity() {
        assert_maximal("fun x.x", "fun x: any . x");
    }

    #[test]
    #[ignore]
    fn apply2() {
        // TODO(arjun): We get any -> any -> any as the type on the arrow, which
        // results in just as few coercions.
        assert_maximal(
            "fun x    .fun y                    .y x x",
            "fun x:int.fun y:(int -> int -> int).y x x",
        );
    }
    #[test]
    #[ignore]
    fn indirect_apply_self() {
        // TODO(luna): We get a different result, in part because we don't
        // allow from_any coercions on arguments
        assert_maximal(
            "fun x    .(fun y    .x)           x  x",
            "fun x:any.(fun y:int.x) (from_any x) x",
        );
    }
    #[test]
    #[ignore]
    fn the_long_one() {
        // TODO(luna): We get a different result, in part because we don't
        // allow from_any coercions on arguments
        assert_maximal(
            "fun x    .(fun f    .(fun x    .fun y    .x)          f (from_any (f x)))(fun z    .1)",
            "fun x:int.(fun f:any.(fun x:int.fun y:int.x)(from_any f)(from_any (f x)))(fun z:int.1)",
        );
    }
    /// this benchmark has no maximal migration, which means that x could be
    /// given an infinity recursive arrow type (t -> t -> t -> ...). we will
    /// give it... something
    #[test]
    fn apply_self() {
        coerces("fun x.x x");
    }
    /// this benchmark has an unknown maximal migration. because Migeed's
    /// algorithm is incomplete, it sometimes does not report whether a maximal
    /// solution exists. in practice, this probably means that there is no maximal
    /// migration. we still give it some migration
    #[test]
    fn untypable_in_sys_f() {
        coerces("(fun x.fun y.y(x(fun x.x))(x(fun b.fun c.b)))(fun d.d d)");
    }
    /// unknown to Migeed and Parsberg. self interpreter for the lambda calculus
    #[test]
    fn self_interpreter() {
        coerces(
            "(fun h.(fun x.h(x x))(fun x.h x x))
             (fun e.fun m.m(fun x.x)(fun m.fun n.(e m)(e n))(fun m.fun v.e (m v)))",
        );
    }
}

#[cfg(test)]
mod tests_misc {
    use super::tests_631::coerces;

    #[test]
    fn fact_church() {
        coerces(
            "
            let add1  =
                  fun x. 1 + x in
            let one  =
                  fun f. fun x. f x in
            let five  =
                  fun f. fun x. f (f (f (f (f x)))) in
            let pred  =
                  fun n.
                    (fun f.
                      (fun x.
                        (((n (fun g. fun h. h (g f)))
                          (fun u. x))
                         (fun u. u)))) in
            let mult  =
                  fun m.
                    (fun n.
                      (fun f. m (n f))) in
            let _true   =
                   fun a. fun b. a in
            let _false  =
                   fun a. fun b. b in
            let is0   =
                  fun n. n (fun x. _false) _true in
            let fact  =
                  fix fact. fun n.
                    ((     (is0 n) // if
                           (fun x. one))
                           (fun x. (mult n) (fact (pred n)))) in
            let realize = fun n . n add1 0 in // : (int -> int) -> (int -> int)
            let n = fact five in
            realize n",
        );
    }
    #[test]
    fn fact_dyn() {
        coerces(
            "
            let f = fun f.fun n.
                if n = 0
                    then 1
                    else n * (f f (n + -1)) in
            f f 6",
        );
    }
}

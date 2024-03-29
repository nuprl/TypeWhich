# Introduction

<span class="smallcaps">TypeWhich</span> is a type migration tool for
the gradually-typed lambda calculus with several extensions. Its
distinguishing characteristics are the following:

1.  <span class="smallcaps">TypeWhich</span> formulates type migration
    as a MaxSMT problem.

2.  <span class="smallcaps">TypeWhich</span> always produces a
    migration, as long as the input program is well-scoped.

3.  <span class="smallcaps">TypeWhich</span> can optimize for different
    properties: it can produce the most informative types, or types that
    ensure compatibility with un-migrated code.

Before you read the read of this guide or try to use
<span class="smallcaps">TypeWhich</span>, we strongly recommend reading
Phipps-Costin et al. (2021), which describes
<span class="smallcaps">TypeWhich</span> in depth.

This repository contains the source code for
<span class="smallcaps">TypeWhich</span>. In addition to the core type
migration algorithm, the <span class="smallcaps">TypeWhich</span>
executable has several auxiliary features:

1.  It has a parser for the Grift programming language, which we use to
    infer types for the Grift benchmarks from Kuhlenschmidt,
    Almahallawi, and Siek (2019);

2.  It has an interpreter for the GTLC, which we use in validation;

3.  It has an implementation of the gradual type inference algorithm
    from Rastogi, Chaudhuri, and Hosmer (2012); and

4.  It includes a framework for evaluating type migration algorithms,
    which we use to compare <span class="smallcaps">TypeWhich</span> to
    several algorithms from the literature (Rastogi, Chaudhuri, and
    Hosmer 2012; Campora et al. 2018; Migeed and Palsberg 2020; Siek and
    Vachharajani 2008).

Finally, this repository contains several gradual typing benchmarks:

1.  The “challenge set” from Phipps-Costin et al. (2021);

2.  The benchmarks from Migeed and Palsberg (2020); and

3.  The benchmarks from Kuhlenschmidt, Almahallawi, and Siek (2019).

This document will guide you though building
<span class="smallcaps">TypeWhich</span>, using it on example programs,
and using the evaluation framework to reproduce our experimental
results.

## Building and Testing <span class="smallcaps">TypeWhich</span>

**For artifact evaluation, we strongly recommend using the
<span class="smallcaps">TypeWhich</span> Virtual Machine and skipping
this section.**

##### 

<span class="smallcaps">TypeWhich</span> is built in Rust and uses Z3
under the hood. In principle, it should work on macOS, Linux or Windows,
though we have only tried it on macOS and Linux. *However*, our
evaluation uses the implementation from Siek and Vachharajani (2008),
which is an old piece of software that is difficult to build on a modern
platform. We have managed to compile it a Docker container and produce a
32-bit Linux binary. It should be possible to build it for other
platforms, but it will require additional effort. Therefore, **we
strongly recommend using Linux to evaluate
<span class="smallcaps">TypeWhich</span>**.

##### Installing <span class="smallcaps">TypeWhich</span> Dependencies

To build <span class="smallcaps">TypeWhich</span> from source, you will
need:

1.  The [Rust language toolchain](https://rustup.rs/).

2.  The Z3 build dependencies and the “usual” build toolchain. On Ubuntu
    Linux, you can run the following command to get them:
    
        sudo apt-get install libz3-dev build-essential

3.  Python 3 and PyYAML to run the integration tests. These are
    installed by default on most platforms. If you can run the following
    command successfully then you already have them installed:
    
        python3 -c "import yaml"

##### Installing Other Type Migration Tools

<span class="smallcaps">TypeWhich</span> does not require these
dependencies, but they are necessary to reproduce our evaluation.

1.  Migeed and Palsberg (2020) is implemented in Haskell. We have
    written a parser and printer for their tool that is compatible with
    <span class="smallcaps">TypeWhich</span>. This modified
    implementation is available at the following URL:
    
    <https://github.com/arjunguha/migeed-palsberg-popl2020>
    
    Build the tool as described in the repository, and then copy (or
    symlink) the `MaxMigrate` program to `bin/MaxMigrate` in the
    <span class="smallcaps">TypeWhich</span> directory. On Linux, the
    executable is at:
    
        migeed-palsberg-popl2020/.stack-work/install/x86_64-linux-tinfo6/
        lts-13.25/8.6.5/bin/MaxMigrate

2.  Siek and Vachharajani (2008) is implemented in OCaml 3.12 (which is
    quite old). The following repository has an implementation of the
    tool, with a modified parser and printer that is compatible with
    <span class="smallcaps">TypeWhich</span>:
    
    <https://github.com/arjunguha/siek-vachharajani-dls2008>
    
    Build the tool as described in the repository, and then copy (or
    symlink) the `gtlc` program to `bin/gtubi` in the
    <span class="smallcaps">TypeWhich</span> directory.
    
    **Warning:** The repository builds a 32-bit Linux executable. You
    will need to ensure that your Linux system has the libraries needed
    to run 32-bit code.

3.  Campora et al. (2018) The following repository has our
    implementation of the algorithm from Campora et al. (2018):
    
    <https://github.com/arjunguha/mgt>
    
    Build the tool as described in the repository and then copy (or
    symlink) the the `mgt` program to `bin/mgt` in the
    <span class="smallcaps">TypeWhich</span> directory.
    
    **Note:** The original implementation by the authors of Campora et
    al. (2018) does not produce an ordinary migrated program as output.
    Instead, it produces a BDD that can be interpreted as a family of
    programs. Our implementation of their algorithm produces programs as
    output.

##### Building and Testing

Use `cargo` to build <span class="smallcaps">TypeWhich</span>:

    cargo build

Run the unit tests:

    cargo test

You may see a few ignored tests, but *no tests should fail*.

Test <span class="smallcaps">TypeWhich</span> using the Grift
benchmarks:

    ./test-runner.sh grift grift

*No tests should fail.*

Finally, run the GTLC benchmarks without any third-party tools:

    cargo run -- benchmark benchmarks.yaml \
      --ignore Gtubi MGT MaxMigrate > test.results.yaml

You will see debugging output (on standard error), but the results will
be saved to the YAML file. Compare these results to known good results:

    ./bin/yamldiff test.expected.yaml test.results.yaml

*You should see no output, which indicates that there are no
differences.*

Build <span class="smallcaps">TypeWhich</span> in release mode (only
needed for performance evaluation):

    cargo build --release

# Artifact Evaluation: Getting Started

Before starting this chapter, we must either:

  - Use the <span class="smallcaps">TypeWhich</span> Virtual Machine, or

  - Install <span class="smallcaps">TypeWhich</span> manually, along
    with all the third party tools we use for evaluation.

**Warning:** <span class="smallcaps">TypeWhich</span> uses the Z3 SMT
solver under the hood, and different versions of Z3 can produce slightly
different results. The expected outputs that we document in this guide
were produced on the <span class="smallcaps">TypeWhich</span> Virtual
Machine.

If the following steps are successful, then we can be quite confident
that <span class="smallcaps">TypeWhich</span> and all third-party tools
are working as expected.

1.  From a terminal window, enter the
    <span class="smallcaps">TypeWhich</span> directory:
    
        cd ~/typewhich

2.  Run the <span class="smallcaps">TypeWhich</span> benchmarks and
    output results to `results.yaml`:
    
        ./bin/TypeWhich benchmark benchmarks.yaml > results.yaml
    
    This will take less than five minutes to complete. This command runs
    the GTLC benchmarks using all five tools, including
    <span class="smallcaps">TypeWhich</span> in two modes. Therefore,
    for each benchmark, we will see six lines of output (on standard
    error). For example:
    
        Running Gtubi on adversarial/01-farg-mismatch.gtlc ...
        Running InsAndOuts on adversarial/01-farg-mismatch.gtlc ...
        Running MGT on adversarial/01-farg-mismatch.gtlc ...
        Running MaxMigrate on adversarial/01-farg-mismatch.gtlc ...
        Running TypeWhich2 on adversarial/01-farg-mismatch.gtlc ...
        Running TypeWhich on adversarial/01-farg-mismatch.gtlc ...
    
    There are three runs of third-party tools that take longer than 30
    seconds, so you will `Killed` appear three times. These are known
    shortcomings that are described in the paper.

3.  Run the following command to ensure that the results are identical
    to known good results:
    
        ./bin/yamldiff expected.yaml results.yaml
    
    There should be no output printed, which indicates that there are no
    differences.

4.  Run the following command to run
    <span class="smallcaps">TypeWhich</span> on the Grift benchmarks:
    
        ./grift_inference.sh
    
    We expect to see `MATCHES` print several times, which indicates that
    <span class="smallcaps">TypeWhich</span> inferred exactly the same
    types that were written by the Grift authors on that benchmark.
    However, we also expect to see a `Warning`, and two mismatches on
    `n_body` and `sieve`.

At this point, we can investigate the artifact in more depth, which is
the subject of the next chapter.

# Artifact Evaluation: Step by Step Guide

**This chapter assumes you have completed the steps in
Chapter [2](#getting-started).**

## Claims To Validate

The paper makes the following claims that we validate in this chapter:

1.  Figure 15 summarizes the performance of several type migration tools
    on a a suite of benchmarks. This artifact generates that figure, and
    we can validate the data and benchmarking scripts in as much depth
    as desired.

2.  Section 6.5 runs <span class="smallcaps">TypeWhich</span> on
    benchmarks written in Grift. These benchmarks have two versions: one
    that has no type annotations, and the other that has human-written
    type annotations. When run on the unannotated Grift benchmarks,
    <span class="smallcaps">TypeWhich</span> recovers the human-written
    annotations on all but two of the Grift benchmarks. This artifact
    includes a script that produces this result.

3.  Section 6.6 reports that our full suite of benchmarks is 892 LOC,
    and <span class="smallcaps">TypeWhich</span> takes three seconds to
    run on all of them. This artifact includes the script we use for the
    performance evaluation. It will take longer in a virtual machine,
    but should be roughly the same. i.e., it will be significantly less
    than 30 seconds.

The rest of this section will is a step-by-step guide through repeating
and validating these claims.

### GTLC Benchmarks on Multiple Tools (Figure 15)

In the previous chapter, we generated `results.yaml`. That ran
<span class="smallcaps">TypeWhich</span> and all other tools on two
suites of benchmarks:

1.  All the benchmarks from Migeed and Palsberg (2020), which are in the
    `migeed` directory.

2.  The “challenge set”’ from the paper, which are in the `adversarial`
    directory.

The file `benchmarks.yaml` drives the benchmarking framework. The top of
the file lists the type migration tool, and is followed by a list of
benchmark files, and some additional information that needed to produce
results. The entire benchmarking procedure is implemented in
`src/benchmark.rs`, which does performs the following steps on each
benchmark:

1.  It checks that the tool produces valid program, to verify that the
    tool did not reject the program.

2.  It runs the original program and the output of the tool and checks
    that they produce the same result, to verify that the tool did not
    introduce a runtime error.

3.  In a gradually typed language, increasing type precision can make a
    program incompatible with certain contexts. To check if this is the
    case, every benchmark in the <span class="smallcaps">yaml</span>
    file *may* be accompanied by a context that witnesses the
    incompatibility: the framework runs the original and migrated
    program in the context, to check if they produce different results.

4.  The framework counts the number of `any`s that are eliminated by the
    migration tool. Every eliminated `any` improves precision, but *may
    or may not* introduce an incompatibility, but this requires human
    judgement. For example, in the program `fun x . x + 1`, annotating
    “x” with `int` does not introduce an incompatibility. However, in
    `fun x . x`, annotating “x” with `int` is an incompatibility. The
    framework flags these results for manual verification. However, it
    allows the input <span class="smallcaps">yaml</span> to specify
    expected outputs to suppress these warnings when desired.

The file `results.yaml` is a copy of `benchmarks.yaml` with output data
added by the benchmarking framework. We use this file to generate
Figure 15 in the paper. You should validate that table as follows:

1.  Check that `results.yaml` does not have any errors: look for the
    string “Disaster” in that file. It should not occur\!

2.  Regenerate the LaTeX snippet for the table with the following
    command:
    
    ``` 
    ./bin/TypeWhich latex-benchmark-summary results.yaml     
    ```
    
    The output that you will see is roughly the LaTeX code for
    Figure 15, with two small differences:
    
    1.  It prints `TypeWhich2` instead of `TypeWhichC`, and
    
    2.  `TypeWhich` instead of `TypeWhichP`.
    
    However, the order of rows and columns is exactly the same as the
    table in the paper. It should be straightforward to check that the
    fractions in this output are exactly the fractions reported in the
    table.

### Grift Benchmarks with <span class="smallcaps">TypeWhich</span>

The Grift evaluation script (grift\_inference.sh) uses the –compare flag
of <span class="smallcaps">TypeWhich</span>, which corresponds migrated
types to the provided file’s type annotations and reports whether they
match, ignoring annotations, coercions, and unannotated identifiers.

On benchmarks for which it reports MATCHES,
<span class="smallcaps">TypeWhich</span> produced exactly the same type
annotations as the hand-typed versions.

On `n_body`, verify that grift-suite/benchmarks/src/dyn/n\_body.grift
and  
grift-suite/benchmarks/src/dyn/n\_body\_no\_unused\_funs.grift differ
only by the removal of unused getters and setters near the top of the
program. Note that <span class="smallcaps">TypeWhich</span>’s types on
the adjusted benchmark with no unused functions matches the hand-typed
version.

On sieve, the warning refers to a lack of parsing support for recursive
types. As a result the mismatch message is less informative than
inspection. To verify exactly which types fail to migrate, run
<span class="smallcaps">TypeWhich</span> to migrate the types of the
program:

    ./bin/TypeWhich migrate grift-suite/benchmarks/src/dyn/sieve.grift

Each bound identifier (excluding lets) will be printed, with its type.
Keeping that input open, manually inspect the hand-typed version at
grift-suite/benchmarks/src/static/sieve/single/sieve.grift. Consider,
for example, the first identifier, `stream-first`. The annotated program
declares stream-first to accept (Rec s (Tuple Int (-\> s))) and return
Int, while <span class="smallcaps">TypeWhich</span>’s output accepts any
and returns any. Inspecting each function remaining, you will see that
every (Rec s (Tuple Int (-\> s))) is replaced with the dynamic type.
Also, some (but not all) integer types are migrated as the dynamic type
(because they hold values from projections out of tuples of any). Note
that the unit-terminated pair representation of tuples is visible in
stream-unfold, which otherwise has the expected type.

### Performance

From the <span class="smallcaps">TypeWhich</span> directory, run the
following command:

    time ./performance.sh

The script will take roughly three seconds to complete. You can read the
script to verify that it runs <span class="smallcaps">TypeWhich</span>
on three suites of benchmarks:

1.  `migeed/*.gtlc`: the benchmarks from Migeed and Palsberg (2020),

2.  `adversarial/*.gtlc`: the “challenge set” from our paper, and

3.  `grift-suite/benchmarks/src/dyn/*.grift`: the benchmarks from
    Kuhlenschmidt, Almahallawi, and Siek (2019).

## Exploring Type Migrations

Our artifact includes several type migration tools, in addition to
<span class="smallcaps">TypeWhich</span>, and we have hacked their
parsers to work with the same concrete syntax, so that it is easy to use
any tool on the same program. We encourage you to try some out, and to
modify the benchmarks as well. Here are the available tools:

  - To run Migeed and Palsberg (2020):
    
        ./bin/MaxMigrate FILENAME.gtlc

  - To run Campora et al. (2018):
    
        ./bin/mgt FILENAME.gtlc

  - To run Siek and Vachharajani (2008):
    
        ./bin/gtubi FILENAME.gtlc

  - To run Rastogi, Chaudhuri, and Hosmer (2012):
    
        ./bin/TypeWhich migrate --ins-and-outs FILENAME.gtlc

  - To run <span class="smallcaps">TypeWhich</span> and produce types
    that are safe in all contexts:
    
        ./bin/TypeWhich migrate FILENAME.gtlc

  - To run <span class="smallcaps">TypeWhich</span> and produce precise
    types that may not work in all contexts:
    
        ./bin/TypeWhich migrate --precise FILENAME.gtlc

##### Example

Create a file called `input.gtlc` with the following contents:

    (fun f. (fun y. f) (f 5)) (fun x. 10 + x)

This program omits all type annotations:
<span class="smallcaps">TypeWhich</span> assumes that omitted
annotations are all **`any`**.

We can migrate the the program using
<span class="smallcaps">TypeWhich</span> in two modes:

1.  In *compatibility mode*, <span class="smallcaps">TypeWhich</span>
    infers types but maintains compatibility with un-migrated code:
    
        $ ./bin/TypeWhich migrate input.gtlc
        (fun f:any -> int. (fun y:int. f) (f 5)) (fun x:any. 10 + x)

2.  In *precise mode*, <span class="smallcaps">TypeWhich</span> infers
    the most precise type that it can, though that may come at the
    expense of compatibility:
    
        $ ./bin/TypeWhich migrate --precise inpuy.gtlc
        (fun f:int -> int. (fun y:int. f) (f 5)) (fun x:int. 10 + x)

## Input Language

<span class="smallcaps">TypeWhich</span> supports a superset of the
GTLC, written in the following syntax. Note that the other tools do not
support all the extensions documented below.

|     |    |                                                                                |                                               |
| --: | :-: | :----------------------------------------------------------------------------- | :-------------------------------------------- |
| *b* | := | **`true`** | **`false`**                                                       | Boolean literal                               |
| *n* | := | ... | \(-1\) | 0 | 1 | ...                                                     | Integer literals                              |
| *s* | := | `"..."`                                                                        | String literals                               |
| *c* | := | b | n | s                                                                      | Literals                                      |
| *T* | := | **`any`**                                                                      | The unknown type                              |
|     | |  | **`int`**                                                                      | Integer type                                  |
|     | |  | **`bool`**                                                                     | Boolean type                                  |
|     | |  | *T*<sub>1</sub> **`->`** *T*<sub>2</sub>                                       | Function type                                 |
|     | |  | **`(`** *T* **`)`**                                                            |                                               |
| *e* | := | *x*                                                                            | Bound identifier                              |
|     | |  | *c*                                                                            | Literal                                       |
|     | |  | e **`:`** T                                                                    | Type ascription                               |
|     | |  | **`(`** *e* **`)`**                                                            | Parenthesis                                   |
|     | |  | **`fun`** *x* **`.`** *e*                                                      | Function                                      |
|     | |  | *e*<sub>1</sub> *e*<sub>2</sub>                                                | Application                                   |
|     | |  | *e*<sub>1</sub> **`+`** *e*<sub>2</sub>                                        | Addition                                      |
|     | |  | *e*<sub>1</sub> **`*`** *e*<sub>2</sub>                                        | Multiplication                                |
|     | |  | *e*<sub>1</sub> **`=`** *e*<sub>2</sub>                                        | Integer equality                              |
|     | |  | *e*<sub>1</sub> **`+?`** *e*<sub>2</sub>                                       | Addition or string concatenation (overloaded) |
|     | |  | **`(`***e*<sub>1</sub>**`,`***e*<sub>2</sub>**`)`**                            | Pair                                          |
|     | |  | **`fix`** *f* **`.`***e*                                                       | Fixpoint                                      |
|     | |  | **`if`** *e*<sub>1</sub> **`then`** *e*<sub>2</sub> **`else`** *e*<sub>3</sub> | Conditional                                   |
|     | |  | **`let`** *x* **`=`** *e*<sub>1</sub> **`in`** *e*<sub>2</sub>                 | Let binding                                   |
|     | |  | **`let rec`** *x* **`=`** *e*<sub>1</sub> **`in`** *e*<sub>2</sub>             | Recursive let binding                         |

# Guide to Source Code

The root TypeWhich directory includes a number of utilities, programs,
and source code (though most of <span class="smallcaps">TypeWhich</span>
is provided in src/):

1.  `adversarial/, grift-suite/benchmarks/, migeed/`: The three
    components of the <span class="smallcaps">TypeWhich</span> benchmark
    suite, adversarial/ being original, and grift-suite/ and migeed/
    adapted from the referenced research

2.  `doc/`: Source and render of this documentation

3.  `benchmarks.yaml`: This is the test harness configuration and data
    for the <span class="smallcaps">TypeWhich</span> benchmarks
    framework, specifying to run the benchmarks and tools presented in
    the paper

4.  `expected.yaml`: Provides the expected behavior of the tool when
    configured with benchmarks.yaml

5.  `test.expected.yaml`: Provides the expected behavior of only
    <span class="smallcaps">TypeWhich</span>/ Rastogi, Chaudhuri, and
    Hosmer (2012) for testing the implementations

6.  `bin/`: Provides (and expects user to provide) symbolic links to
    tools

7.  `build.rs, Cargo.lock, Cargo.toml, target/`: Required build files
    for <span class="smallcaps">TypeWhich</span>. Binaries are placed in
    target/

8.  `other-examples/`: Provides additional programs that are not
    interesting enough to be in the
    <span class="smallcaps">TypeWhich</span> benchmark suite

9.  `grift_inference.sh`: Evaluation tool for Grift benchmarks which
    compares if types produced are exactly the same as the static types
    provided in the suite

10. `performance.sh, test-runner.sh, run_tool.sh`: Tools that run
    <span class="smallcaps">TypeWhich</span> on more programs or in
    release mode

11. **`src/`**: The <span class="smallcaps">TypeWhich</span>
    implementation, including implemention of Rastogi, Chaudhuri, and
    Hosmer (2012)

Within `src/`, the following files are found:

1.  **`benchmark.rs`, `precision.rs`**: Provides the
    <span class="smallcaps">TypeWhich</span> benchmarking framework

2.  **`cgen.rs`**: Generates the documented constraints of the
    <span class="smallcaps">TypeWhich</span> algorithm and performs type
    migration

3.  `eval.rs`: An interpreter for the GTLC with explicit coercions

4.  `insert_coercions.rs`: Type-directed coercion insertion for the
    GTLC, used for the interpreter. Not related to type migration

5.  `grift.l, grift.y, grift.rs, lexer.l, parser.y, pretty.rs`: Parsers
    and printers for Grift and the unified concrete syntax used by all
    tools

6.  `ins_and_outs/`: Our implementation of Rastogi, Chaudhuri, and
    Hosmer (2012).

7.  `main.rs`: Entry point; options parsing

8.  `syntax.rs`: The language supported by
    <span class="smallcaps">TypeWhich</span>. Also includes the –compare
    tool used for Grift evaluation

9.  `type_check.rs`: Type-checking for programs with explicit coercions

10. `z3_state.rs`: Abstraction for the Z3 solver used for type inference
    in <span class="smallcaps">TypeWhich</span>

The core of the <span class="smallcaps">TypeWhich</span> algorithm is
found in cgen.rs. The constraints specified in the paper are implemented
in State::cgen (\~line 52), with comments resembling the notation from
the paper. Of note are references to `strengthen` and `weaken`, which
are simply macros for
\((t1 = t2 \land w) \lor (t1 = * \land \texttt{ground(t2)} \land \neg w)\),
w fresh; and
\((t1 = t2 \land w) \lor (t2 = * \land \texttt{ground(t1)} \land \neg w)\),
w fresh respectively. They are not to be confused with the
<span class="smallcaps">Weaken</span> function from the paper.

State::negative\_any (\~line 400) implements the
<span class="smallcaps">Weaken</span> algorithm from the paper.
typeinf\_options (\~line 624) implements the
<span class="smallcaps">Migrate</span> algorithm in full.

<div id="refs" class="references">

<div id="ref-campora:migrating">

Campora, John Peter, Sheng Chen, Martin Erwig, and Eric Walkingshaw.
2018. “Migrating Gradual Types.” *Proceedings of the ACM on Programming
Languages (PACMPL)* 2 (POPL).

</div>

<div id="ref-kuhlenschmidt:grift">

Kuhlenschmidt, Andre, Deyaaeldeen Almahallawi, and Jeremy G. Siek. 2019.
“Toward Efficient Gradual Typing for Structural Types via Coercions.” In
*ACM Sigplan Conference on Programming Language Design and
Implementation (Pldi)*.

</div>

<div id="ref-migeed:decidable">

Migeed, Zeina, and Jens Palsberg. 2020. “What Is Decidable About Gradual
Types?” *Proceedings of the ACM on Programming Languages (PACMPL)* 4
(POPL).

</div>

<div id="ref-typewhich">

Phipps-Costin, Luna, Carolyn Jane Anderson, Michael Greenberg, and Arjun
Guha. 2021. “Solver-Based Gradual Type Migration.” *Proceedings of the
ACM on Programming Languages (PACMPL)* 5 (OOPSLA).

</div>

<div id="ref-rastogi:gti">

Rastogi, Aseem, Avik Chaudhuri, and Basil Hosmer. 2012. “The Ins and
Outs of Gradual Type Inference.” In *ACM Sigplan-Sigact Symposium on
Principles of Programming Languages (Popl)*.

</div>

<div id="ref-siek:gti">

Siek, Jeremy G., and Manish Vachharajani. 2008. “Gradual Typing with
Unification-Based Inference.” In *Dynamic Languages Symposium (Dls)*.

</div>

</div>

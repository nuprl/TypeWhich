# Introduction

<span class="smallcaps">TypeWhich</span> is a type migration tool for
the gradually-typed lambda calculus and the Grift programming language.
Its distinguishing characteristics are the following:

1.  <span class="smallcaps">TypeWhich</span> formulates type migration
    as a MaxSMT problem.

2.  <span class="smallcaps">TypeWhich</span> always produces a
    migration, as long as the input program is well-scoped.

3.  <span class="smallcaps">TypeWhich</span> can optimize for different
    properties: it can produce the most informative types, or types that
    ensure compatibility with un-migrated code.

For more information on <span class="smallcaps">TypeWhich</span>, see
Phipps-Costin et al. (2021).

# Building and Testing <span class="smallcaps">TypeWhich</span>

## Dependencies

To build <span class="smallcaps">TypeWhich</span> from source, you will
need:

1.  The [Rust language toolchain](https://rustup.rs/).

2.  The Z3 build dependencies. On Ubuntu Linux, you can run the
    following command to get them:
    
        sudo apt-get install libz3-dev

3.  Python 3 and PyYAML to run the integration tests. These are
    installed by default on most platforms. If you can run the following
    command, then you already have them installed:
    
        python3 -c "import yaml"

## Other Type Migration Tools

The <span class="smallcaps">TypeWhich</span> benchmarking suite is setup
to compare <span class="smallcaps">TypeWhich</span> to several other
type migration tools, some of these tools are in other repositories. You
do not need these other tools to use
<span class="smallcaps">TypeWhich</span>, but you do need them to
reproduce the evaluation from Phipps-Costin et al. (2021).

1.  Rastogi, Chaudhuri, and Hosmer (2012): the
    <span class="smallcaps">TypeWhich</span> code includes an
    implementation of this algorithm, and it has no external
    dependencies.

2.  Migeed and Palsberg (2020) is implemented in Haskell. We have
    written a parser and printer for their tool that is compatible with
    <span class="smallcaps">TypeWhich</span>. This modified
    implementation is available at the following URL:
    
    <https://github.com/arjunguha/migeed-palsberg-popl2020>
    
    Build the tool as described in the repository, and then copy (or
    symlink) the `MaxMigrate` program to `bin/MaxMigrate` in the
    <span class="smallcaps">TypeWhich</span> directory.. On Linux, the
    executable is at:
    
        migeed-palsberg-popl2020/.stack-work/install/x86_64-linux-tinfo6/
        lts-13.25/8.6.5/bin/MaxMigrate

3.  Siek and Vachharajani (2008) is implemented in OCaml 3.12 (which is
    quite old). The following repository has an implementation of the
    tool, with a modified parser and printer that is compatible with
    <span class="smallcaps">TypeWhich</span>:
    
    <https://github.com/arjunguha/siek-vachharajani-dls2008>
    
    Build the tool as described in the repository, and then copy (or
    symlink) the `gtlc` program to `bin/gtubi` in the
    <span class="smallcaps">TypeWhich</span> directory.
    
    **Warning:** It is quite hard to build OCaml 3.12 on a modern Linux
    system. The repository is configured to build a 32-bit Linux
    executable.

4.  Campora et al. (2018) \[FILL\]
    
    <https://github.com/arjunguha/mgt>

## Building and Testing

To build <span class="smallcaps">TypeWhich</span>(and our implementation
of Rastogi, Chaudhuri, and Hosmer (2012)), run the following command:

    cargo build

Run the unit tests:

    cargo test

Test <span class="smallcaps">TypeWhich</span> using the Grift
benchmarks:

    ./test-runner.sh grift grift

Finally, run the GTLC benchmarks without any third-party tools:

    cargo run -- benchmark benchmarks.yaml \
      --ignore Gtubi MGT MaxMigrate > test.results.yaml
    ./bin/yamldiff test.expected.yaml test.results.yaml

# Running <span class="smallcaps">TypeWhich</span>

The <span class="smallcaps">TypeWhich</span> executable is symlinked to
`bin/TypeWhich`. <span class="smallcaps">TypeWhich</span> expects its
input program to be in a single file, and written in either Grift
(extension `.grift`) or in a superset of the gradually typed lambda
calculus (extension `.gtlc`), shown in Section [4](#input-lang-gtlc).

#### Example

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
    
        $ ./bin/TypeWhich migrate --unsafe inpuy.gtlc
        (fun f:int -> int. (fun y:int. f) (f 5)) (fun x:int. 10 + x)

The <span class="smallcaps">TypeWhich</span> executable supports several
other sub-commands and flags. Run `./bin/TypeWhich –help` for more
complete documentation.

# Input Language

**[./doc/doc.pdf](./doc/doc.pdf) has the same content as this file, but
with slightly better formatting.**

<span class="smallcaps">TypeWhich</span> supports a superset of the
GTLC, written in the following syntax:

\[FILL\] A few cases missing

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

# Experiments

*To run the full suite of experiments, you will need to install the
third-party type migration tools.*

To run the experiments, use the following command:

    ./bin/TypeWhich benchmark benchmarks.yaml > RESULTS.yaml

It prints progress on standard error. The output is a YAML file of
results.

## Validation

1.  The benchmarking script does a lot of validation itself.

2.  In `RESULTS.yaml`, look for the string “Disaster”. It should not
    appear\!

3.  In `RESULTS.yaml`, look for the string `manually_verify`. These are
    results from experiments where (1) we could not crash the migrated
    program, and (2) the migrated program has fewer ‘any‘s than the
    original. So, the table of results counts this migration as one that
    is 100% compatible with untyped contexts. But, it requires a manual
    check.

4.  Finally, you can compare `RESULTS.yaml` with a known good output
    from benchmarking:
    
        ./bin/yamldiff RESULTS.yaml expected.yaml

## Results

To generate the summary table found in Phipps-Costin et al. (2021), use
the following command:

    ./bin/TypeWhich latex-benchmark-summary RESULTS.yaml 

To generate the appendix of results:

    ./bin/TypeWhich latex-benchmarks RESULTS.yaml 

# Benchmarks

The <span class="smallcaps">TypeWhich</span> repository has several
benchmarks:

1.  The `migeed` directory contains the benchmarks from Migeed et al.,
    written in the concrete syntax of
    <span class="smallcaps">TypeWhich</span>.

2.  The `adversarial` directory contains the “challenge set” from the
    <span class="smallcaps">TypeWhich</span> paper.

3.  The `grift-suite` directory contains tests from
    [Grift](https://github.com/Gradual-Typing/Grift/tree/master/tests/suite).
    The `mu/` directory has been modified to use Dyn where it originally
    used recursive types.

4.  The `grift-suite/benchmarks` contains benchmarks from
    <https://github.com/Gradual-Typing/benchmarks> with the following
    adjustments:
    
    1.  The getters and setters in n-body have been removed. They were
        neither used nor exported we opted to remove these functions
        from the benchmark. This is discussed in the paper.
    
    2.  We have changed where in the program some benchmarks print a
        terminating newline for consistency between the static and
        dynamic versions.
    
    3.  Benchmarks that rely on modules are removed

<div id="refs" class="references">

<div id="ref-campora:migrating">

Campora, John Peter, Sheng Chen, Martin Erwig, and Eric Walkingshaw.
2018. “Migrating Gradual Types.” *Proceedings of the ACM on Programming
Languages (PACMPL)* 2 (POPL).

</div>

<div id="ref-migeed:decidable">

Migeed, Zeina, and Jens Palsberg. 2020. “What Is Decidable About Gradual
Types?” *Proceedings of the ACM on Programming Languages (PACMPL)* 4
(POPL).

</div>

<div id="ref-typewhich">

Phipps-Costin, Luna, Carolyn Jane Anderson, Michael Greenberg, and Arjun
Guha. 2021. “Solver-Based Gradual Type Migration.”
<https://khoury.northeastern.edu/~arjunguha/main/papers/2021-typewhich.html>.

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

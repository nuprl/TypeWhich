---
author:
- |
    Luna Phipps-Costin, Carolyn Jane Anderson, Michael Greenberg, and Arjun
    Guha
bibliography: 'main.bib'
title: '[TypeWhich]{.smallcaps} Guide'
---

Introduction
============

[TypeWhich]{.smallcaps} is a type migration tool for the gradually-typed
lambda calculus and the Grift programming language. Its distinguishing
characteristics are the following:

1.  [TypeWhich]{.smallcaps} formulates type migration as a MaxSMT
    problem.

2.  [TypeWhich]{.smallcaps} always produces a migration, as long as the
    input program is well-scoped.

3.  [TypeWhich]{.smallcaps} can optimize for different properties: it
    can produce the most informative types, or types that ensure
    compatibility with un-migrated code.

For more information on [TypeWhich]{.smallcaps}, see @typewhich.

Building and Testing [TypeWhich]{.smallcaps}
============================================

Dependencies
------------

To build [TypeWhich]{.smallcaps} from source, you will need:

1.  The [Rust language toolchain](https://rustup.rs/).

2.  The Z3 build dependencies. On Ubuntu Linux, you can run the
    following command to get them:

        sudo apt-get install libz3-dev

3.  Python 3 and PyYAML to run the integration tests. These are
    installed by default on most platforms. If you can run the following
    command, then you already have them installed:

        python3 -c "import yaml"

Other Type Migration Tools
--------------------------

The [TypeWhich]{.smallcaps} benchmarking suite is setup to compare
[TypeWhich]{.smallcaps} to several other type migration tools, some of
these tools are in other repositories. You do not need these other tools
to use [TypeWhich]{.smallcaps}, but you do need them to reproduce the
evaluation from @typewhich.

1.  @rastogi:gti: the [TypeWhich]{.smallcaps} code includes an
    implementation of this algorithm, and it has no external
    dependencies.

2.  @migeed:decidable is implemented in Haskell. We have written a
    parser and printer for their tool that is compatible with
    [TypeWhich]{.smallcaps}. This modified implementation is available
    at the following URL:

    <https://github.com/arjunguha/migeed-palsberg-popl2020>

    Build the tool as described in the repository, and then copy (or
    symlink) the `MaxMigrate` program to `bin/Migrate` in the
    [TypeWhich]{.smallcaps} directory.. On Linux, the executable is at:

        migeed-palsberg-popl2020/.stack-work/install/x86_64-linux-tinfo6/
        lts-13.25/8.6.5/bin/MaxMigrate

3.  @siek:gti is implemented in OCaml 3.12 (which is quite old). The
    following repository has an implementation of the tool, with a
    modified parser and printer that is compatible with
    [TypeWhich]{.smallcaps}:

    <https://github.com/arjunguha/siek-vachharajani-dls2008>

    Build the tool as described in the repository, and then copy (or
    symlink) the `gtlc` program to `bin/gtubi` in the
    [TypeWhich]{.smallcaps} directory.

    **Warning:** It is quite hard to build OCaml 3.12 on a modern Linux
    system. The repository is configured to build a 32-bit Linux
    executable.

4.  @campora:migrating \[FILL\]

    <https://github.com/arjunguha/mgt>

Building and Testing
--------------------

To build [TypeWhich]{.smallcaps}(and our implementation of
@rastogi:gti), run the following command:

    cargo build

Run the unit tests:

    cargo test

Test [TypeWhich]{.smallcaps} using the Grift benchmarks:

    ./test-runner.sh grift grift

Finally, run the GTLC benchmarks without any third-party tools:

    cargo run -- benchmark benchmarks.yaml \
      --ignore Gtubi MGT MaxMigrate > test.results.yaml
    ./bin/yamldiff test.expected.yaml test.results.yaml

Running [TypeWhich]{.smallcaps}
===============================

The [TypeWhich]{.smallcaps} executable is symlinked to `bin/TypeWhich`.
[TypeWhich]{.smallcaps} expects its input program to be in a single
file, and written in either Grift (extension `.grift`) or in a superset
of the gradually typed lambda calculus (extension `.gtlc`), shown in
Section [4](#input-lang-gtlc){reference-type="ref"
reference="input-lang-gtlc"}.

#### Example

Create a file called `input.gtlc` with the following contents:

    (fun f. (fun y. f) (f 5)) (fun x. 10 + x)

This program omits all type annotations: [TypeWhich]{.smallcaps} assumes
that omitted annotations are all **`any`**.

We can migrate the the program using [TypeWhich]{.smallcaps} in two
modes:

1.  In *compatibility mode*, [TypeWhich]{.smallcaps} infers types but
    maintains compatibility with un-migrated code:

        $ ./bin/TypeWhich migrate input.gtlc
        (fun f:any -> int. (fun y:int. f) (f 5)) (fun x:any. 10 + x)

2.  In *precise mode*, [TypeWhich]{.smallcaps} infers the most precise
    type that it can, though that may come at the expense of
    compatibility:

        $ ./bin/TypeWhich migrate --unsafe inpuy.gtlc
        (fun f:int -> int. (fun y:int. f) (f 5)) (fun x:int. 10 + x)

The [TypeWhich]{.smallcaps} executable supports several other
sub-commands and flags. Run `./bin/TypeWhich –help` for more complete
documentation.

Input Language {#input-lang-gtlc}
==============

[TypeWhich]{.smallcaps} supports a superset of the GTLC, written in the
following syntax:

\[FILL\] A few cases missing

  ----- ---- ----------------------------------------------------- -----------------------------------------------
    *b*  :=  **`true`** \| **`false`**                             Boolean literal
    *n*  :=  \... \| $-1$ \| 0 \| 1 \| \...                        Integer literals
    *s*  :=  `"..."`                                               String literals
    *c*  :=  b \| n \| s                                           Literals
    *T*  :=  **`any`**                                             The unknown type
         \|  **`int`**                                             Integer type
         \|  **`bool`**                                            Boolean type
         \|  *T*~1~ **`->`** *T*~2~                                Function type
         \|  **`(`** *T* **`)`**                                   
    *e*  :=  *x*                                                   Bound identifier
         \|  *c*                                                   Literal
         \|  e **`:`** T                                           Type ascription
         \|  **`(`** *e* **`)`**                                   Parenthesis
         \|  **`fun`** *x* **`.`** *e*                             Function
         \|  *e*~1~ *e*~2~                                         Application
         \|  *e*~1~ **`+`** *e*~2~                                 Addition
         \|  *e*~1~ **`*`** *e*~2~                                 Multiplication
         \|  *e*~1~ **`=`** *e*~2~                                 Integer equality
         \|  *e*~1~ **`+?`** *e*~2~                                Addition or string concatenation (overloaded)
         \|  **`(`***e*~1~**`,`***e*~2~**`)`**                     Pair
         \|  **`fix`** *f* **`.`***e*                              Fixpoint
         \|  **`if`** *e*~1~ **`then`** *e*~2~ **`else`** *e*~3~   Conditional
         \|  **`let`** *x* **`=`** *e*~1~ **`in`** *e*~2~          Let binding
         \|  **`let rec`** *x* **`=`** *e*~1~ **`in`** *e*~2~      Recursive let binding
  ----- ---- ----------------------------------------------------- -----------------------------------------------

Running Experiments
===================

Run this command:

    ./bin/TypeWhich benchmark benchmarks.yaml > RESULTS.yaml

It prints progress on standard error. The output is a YAML file of
results, followed by a table summary.

Validation
==========

1.  The benchmarking script does a lot of validation itself.

2.  In RESULTS.yaml, look for the string \"Disaster\". It should not
    appear!

3.  In RESULTS.yaml, look for the string `manually_verify`. These are
    results from experiments where (1) we could not crash the migrated
    program, and (2) the migrated program has fewer 'any's than the
    original. So, the table of results counts this migration as one that
    is 100% compatible with untyped contexts. But, it requires a manual
    check.

Benchmarks
==========

The [TypeWhich]{.smallcaps} repository has several benchmarks:

1.  The `migeed` directory contains the benchmarks from Migeed et al.,
    written in the concrete syntax of [TypeWhich]{.smallcaps}.

2.  The `adversarial` directory contains the "challenge set" from the
    [TypeWhich]{.smallcaps} paper.

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

# TypeWhich

TypeWhich is a type migration tool for the gradually-typed lambda calculus and
the Grift programming language. Its principle features are that:

1. TypeWhich formulates type migration as a MaxSMT problem.

2. TypeWhich always produces a migration, as long as the input program is
   well-scoped.

3. TypeWhich can optimize for different properties: it can produce the most
   informative types, or types that ensure compatibility with unmigrated code.

For more information, see our draft paper:

Luna Phipps-Costin, Carolyn Jane Anderson, Michael Greenberg, and Arjun Guha.
[Solver-based Gradual Type Migration](https://khoury.northeastern.edu/~arjunguha/main/papers/2021-typewhich.html) 
*In submission*.


## Dependencies

TypeWhich requires:

1. The [Rust toolchain](https://rustup.rs/)

2. The Z3 build dependencies. On Ubuntu Linux, you can run
   `sudo apt-get install libz3-dev`.

2. Python 3 and PyYAML to run the integration tests. These are installed by
   default on most platforms. If you can run `python3 -c "import yaml"`, then
   you have this already.

The TypeWhich benchmarking suite is setup to compare TypeWhich to several
other type migration tools:

1. Aseem Rastogi, Avik Chaudhuri, and Basil Hosmer.
   The Ins and Outs of Gradual Type Inference. In *POPL* 2012.

   The TypeWhich codebase includes an implementation of this algorithm.


3. Zeina Migeed and Jens Palsberg. What is Decidable About Gradual Typing?
   In *POPL* 2020.

   The following repository is a fork of the paper's artifact, to which we've
   added a parser that is compatible with ours:

   https://github.com/arjunguha/migeed-palsberg-popl2020

   Build the tool as described in the repository, and then copy (or symlink)
   the *MaxMigrate* program to `bin/MaxMigrate`. On Linux, the executable is at:

   `migeed-palsberg-popl2020/.stack-work/install/x86_64-linux-tinfo6/lts-13.25/8.6.5/bin/MaxMigrate`

4. Jeremy G. Siek and Manish Vachharajani. Gradual Typing with Unification-based
   Inference. In *DLS* 2008.

   The following repository is a fork of the paper's artifact, to which we've
   added a parser that is compatible with ours:

   https://github.com/arjunguha/siek-vachharajani-dls2008

   Build the tool as described in the repository, and then copy (or symlink)
   the *gtlc* program to `bin/gtubi`.

   **Warning**: The tool builds a 32-bit Linux executable. Good luck!

5. **TODO Instructions missing.** Migrating Gradual Types:
   
   https://github.com/arjunguha/mgt

## Building

```
cargo build
ln -s $PWD/target/debug/typeinf-playground bin/TypeWhich
```

## Experiments

Run this command:

```
./bin/TypeWhich benchmark benchmarks.yaml > RESULTS.yaml
```

It prints progress on standard error. The output is a YAML file of results,
followed by a table summary. 

### Validating Results

1. The benchmarking script does a lot of validation itself.

2. In RESULTS.yaml, look for the string "Disaster". It should not appear!

3. In RESULTS.yaml, look for the string "manually_verify". These are results
   from experiments where (1) we could not crash the migrated program, and
   (2) the migrated program has fewer `any`s than the original. So, the
   table of results counts this migration as one that is 100% compatible with
   untyped contexts. But, it requires a manual check.

## Testing

Take a look in `.github/workflows/build.yml` to see our CI
testing. The gist of it is:

```
cargo build
cargo test -- --nocapture
cargo run -- benchmark benchmarks.yaml --ignore Gtubi MGT MaxMigrate > test.results.yaml
./bin/yamldiff test.expeceted.yaml test.results.yaml
./test-runner.sh grift grift
```

## Rastogi et al

In the directory [ins-and-outs](ins-and-outs/) is an implementation of Rastogi
et al's paper, The Ins and Outs of Gradual Type Inference. The implementation
has a problem with not terminating on recursive programs that may not originate
in the paper.

## Benchmarks and test suites

### [migeed](migeed)

These benchmarks were included in Migeed et al's paper and have been written in
our concrete syntax.

These examples are duplicated in our distribution of [Siek and Vachharajani's
tool](https://github.com/arjunguha/siek-vachharajani-dls2008) with their
concrete syntax and needed environments.

### [adversarial](adversarial)

These benchmarks are presented in the paper as the "Challenge set." They are in
our concrete syntax.

### [grift-suite](grift-suite)

This is a clone of [the test suite from
Grift](https://github.com/Gradual-Typing/Grift/tree/master/tests/suite). The
mu/ directory has been modified to use Dyn where it originally used recursive
types.

### [grift-suite/benchmarks](grift-suite/benchmarks)

This is a clone of the [benchmarks from the Grift
paper](https://github.com/Gradual-Typing/benchmarks) with the following
adjustments:

- The getters and setters in n-body have been removed. They were neither used
nor exported we opted to remove these functions from the benchmark. This is
discussed in the paper.
- We have changed where in the program some benchmarks print a terminating
newline for consistency between the static and dynamic versions.
- Benchmarks that rely on modules are removed

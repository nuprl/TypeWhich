
# Dependencies

```
sudo apt-get install libz3-dev
```

1. Zeina Migeed and Jens Palsberg. What is Decidable About Gradual Typing.
   POPL 2020.

   The following repository is a fork of the paper's artifact, to which we've
   added a parser that is compatible with ours:

   https://github.com/arjunguha/migeed-palsberg-popl2020

   Build the tool as described in the repository, and then copy (or symlink)
   the *MaxMigrate* program to `bin/Migrate`. On Linux, the executable is at:

   `migeed-palsberg-popl2020/.stack-work/install/x86_64-linux-tinfo6/lts-13.25/8.6.5/bin/MaxMigrate`

# Testing

Take a look in `.github/workflows/build.yml` to see our CI
testing. The gist of it is:

```
cargo build
cargo test -- --nocapture
./test-runner.sh grift grift
./test-runner.sh ins-and-outs migeed
./test-runner.sh no-context migeed
./test-runner.sh smt migeed
```

# Rastogi et al

In the directory [ins-and-outs](ins-and-outs/) is an implementation of Rastogi
et al's paper, The Ins and Outs of Gradual Type Inference. The implementation
has a number of problems, such as not dealing with context appropriately.

# Benchmarks and test suites

## [migeed](migeed)

These benchmarks were included in Migeed et al's paper and have been written in
the concrete syntax used by our tool, the Rastogi et al implementation, and our
parser for Migeed et al's tool.

These examples are duplicated in our distribution of [Siek and Vachharajani's
tool](https://github.com/arjunguha/siek-vachharajani-dls2008) with their
concrete syntax and needed environments.

## [adversarial](adversarial)

These benchmarks are presented in the paper as the "Challenge set." They are in
the concrete syntax used by our tool, the Rastogi et al implementation, and our
parser for Migeed et al's tool.

These examples are also duplicated in our Siek and Vachharajani distribution.

## [grift-suite](grift-suite)

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

# TODO

- [x] expose grift on CLI
- [x] letrec
- [x] a way to automatically reset annotations (for grift tests)
- [x] get grift benchmarks running
  + [x] https://github.com/Gradual-Typing/Grift/blob/master/src/language/forms.rkt for operations
  + [x] floating point https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/n-body.grift
  + [x] top-level (define, etc.) https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/insertion-sort-5.grift

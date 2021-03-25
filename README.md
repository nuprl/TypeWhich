
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

```
cargo test -- --nocapture
./test-runner.sh grift
./test-runner.sh migeed-ins-and-outs
./test-runner.sh migeed-context
./test-runner.sh migeed-smt
```

# TODO

- [x] expose grift on CLI
- [x] letrec
- [x] a way to automatically reset annotations (for grift tests)
- [x] get grift benchmarks running
  + [x] https://github.com/Gradual-Typing/Grift/blob/master/src/language/forms.rkt for operations
  + [x] floating point https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/n-body.grift
  + [x] top-level (define, etc.) https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/insertion-sort-5.grift

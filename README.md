
# Dependencies

```
sudo apt-get install libz3-dev
```

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

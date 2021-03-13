
# Dependencies

```
sudo apt-get install libz3-dev
```

# Testing

```
cargo test -- --nocapture
```

# TODO

- [x] expose grift on CLI
- [x] letrec
- [ ] get grift benchmarks running
  + [ ] https://github.com/Gradual-Typing/Grift/blob/master/src/language/forms.rkt for operations
  + [ ] floating point https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/n-body.grift
  + [x] top-level (define, etc.) https://github.com/Gradual-Typing/Grift/blob/master/tests/suite/program/insertion-sort-5.grift
        desugar into letrec?

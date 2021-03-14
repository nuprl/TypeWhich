use crate::syntax::*;

lrlex::lrlex_mod!("grift.l"); // effectively mod `grift_l`
lrpar::lrpar_mod!("grift.y"); // effectively mod `grift_y`

pub fn toplevel_exp(tls: Vec<Toplevel>) -> Exp {
    let mut bindings = Vec::new();
    let mut exprs = Vec::new();

    let mut saw_expr = false;
    let mut warnings = Vec::new();
    for tl in tls.into_iter() {
        match tl {
            Toplevel::Define(x, t, e) => {
                if saw_expr {
                    warnings.push(x.clone());
                }

                bindings.push((x, t, e));
            }
            Toplevel::Exp(e) => {
                saw_expr = true;
                exprs.push(e);
            }
        }
    }

    if !warnings.is_empty() {
        eprintln!(
            "The following top-level definitions have been unsoundly reordered: {}.",
            warnings.join(", ")
        );
    }

    let e = Exp::begin(exprs);
    if bindings.is_empty() {
        e
    } else {
        Exp::LetRec(bindings, Box::new(e))
    }
}

pub fn parse_toplevel(input: impl AsRef<str>) -> Vec<Toplevel> {
    let input = input.as_ref();
    let lexerdef = grift_l::lexerdef();
    let lexer = lexerdef.lexer(input);
    let (res, errs) = grift_y::parse(&lexer);
    if errs.is_empty() {
        return res.unwrap();
    }
    for err in errs.into_iter() {
        eprintln!("{}", err.pp(&lexer, &|t| grift_y::token_epp(t)));
    }
    panic!("Error parsing expressions");
}

pub fn parse(input: impl AsRef<str>) -> Exp {
    toplevel_exp(parse_toplevel(input))
}

#[cfg(test)]
mod test {
    use super::parse;
    use crate::syntax::*;
    use crate::tests_631::*;

    #[test]
    #[should_panic]
    fn bad_things() {
        parse("(if 5 6 7 8)");
    }
    #[test]
    fn parse_int() {
        assert_eq!(parse("5"), Exp::Lit(Lit::Int(5)))
    }
    #[test]
    fn let_once() {
        assert_eq!(
            parse("(let ((x 5)) x)"),
            Exp::Let(
                "x".to_string(),
                Box::new(Exp::Lit(Lit::Int(5))),
                Box::new(Exp::Var("x".to_string()))
            )
        )
    }
    #[test]
    fn lambda() {
        parse("(lambda (x) x)");
    }
    #[test]
    fn app() {
        parse("((lambda (x) x) 5)");
    }
    #[test]
    fn cond() {
        parse("(if 5 6 7)");
    }
    #[test]
    fn fact_grift_concrete() {
        exp_coerces(parse(
            "(let ((f (lambda (f n)
                (if (= n 0)
                    1
                    ; - was replaced with + because meh
                    (* n (f f (+ n 1)))))))
              ; this was : but again, meh
              (f f 6))",
        ));
    }
    #[test]
    fn ack_no_rec() {
        // this is supposed to be letrec but meh
        exp_succeeds(parse(
            "(letrec ([ack (lambda ([m : Int] [n : Int]) : Int
                    (if (= m 0)
                        (+ n 1)
                        (if (= n 0)
                            (ack (+ m -1) 1)
                            (ack (+ m -1) (ack m (+ n -1))))))])
                  (ack 1 2))",
        ));
    }
    #[test]
    fn ack() {
        exp_succeeds(parse(
            "(letrec ([ack (lambda (m n) ; this should have : Dyn but we don't annotate returns yet
                             (if (= m 0)
                                 (+ n 1)
                                 (if (= n 0)
                                     (ack (+ m -1) 1)
                                     (ack (+ m -1) (ack m (+ n -1))))))])
               (ack 3 10)) ; should be : / ann",
        ));
    }
    #[test]
    fn box_int() {
        exp_succeeds(parse(
            "(let ((my_box (box 5)))
                (let ((i_set (box-set! my_box 10)))
                  (unbox my_box)))",
        ));
    }
    #[test]
    fn box_any() {
        exp_coerces(parse(
            "(let ((my_box (box 5)))
                (let ((i_set (box-set! my_box #t)))
                  (unbox my_box)))",
        ));
    }
    #[test]
    fn box_context() {
        exp_coerces(parse("(box 5)"));
    }
    #[test]
    fn box_identities() {
        assert_eq!(
            exp_coerces(parse(
                "(let ((id (lambda (x) x)))
                (let ((h (id (box 5))))
                (id (unbox h))))"
            )),
            Typ::Any
        );
    }
    #[test]
    fn box_weakens_box_any() {
        assert_eq!(
            exp_coerces(parse(
                "(let ((my_box (box #t)))
                (let ((h ((lambda (x) x) my_box)))
                ((lambda (x) (+ 1 (unbox x))) my_box)))"
            )),
            Typ::Int
        );
    }
    #[test]
    fn box_stay_strong() {
        assert_eq!(
            exp_succeeds(parse(
                "(let ((id (lambda (x) x))) (let ((h (id (box 5)))) 5))"
            )),
            Typ::Int
        );
    }
    #[test]
    fn multi_arg_lam() {
        parse("(lambda (f n) (if (= n 0) 1 (f n)))");
    }
    #[test]
    fn basic_toplevel() {
        assert_eq!(
            exp_succeeds(parse("(define x 5) (define y 10) (+ x y)")),
            Typ::Int
        );
        assert_eq!(
            exp_succeeds(parse(
                "(define (f) 10) (define x 5) (define y 10) (* (f) (+ x y))"
            )),
            Typ::Int
        );
    }
    #[test]
    fn float_constants() {
        assert_eq!(
            exp_succeeds(parse(
                "(define x #i8.34336671824457987) (define y #i2.30417297573763929e-5) (define z 0.1) y"
            )),
            Typ::Float
        )
    }
    #[test]
    fn scheme_varnames() {
        assert_eq!(
            exp_succeeds(parse("(define days-per-year : Float #i365.24) (define *saturn* #i-4.03523417114321381e-1) *saturn*")),
            Typ::Float
        );
    }
    #[test]
    fn tuples() {
        assert_eq!(
            exp_succeeds(parse("(tuple 1 #f \"hi\")")),
            Typ::tuples(vec![Typ::Int, Typ::Bool, Typ::Str])
        );
        assert_eq!(
            exp_succeeds(parse("(tuple-proj (tuple 1 #f \"hi\") 2)")),
            Typ::Str
        );
    }
}

use crate::syntax::*;

lrlex::lrlex_mod!("grift.l"); // effectively mod `grift_l`
lrpar::lrpar_mod!("grift.y"); // effectively mod `grift_y`

pub fn parse(input: impl AsRef<str>) -> Vec<Toplevel> {
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

pub fn parse_exp(input: impl AsRef<str>) -> Exp {
    let tl = parse(input);

    assert_eq!(tl.len(), 1);
    let e = tl.into_iter().next().unwrap();

    match e {
        Toplevel::Exp(e) => e,
        _ => panic!("Expected exp, got '{}'", e),
    }
}

#[cfg(test)]
mod test {
    use super::parse_exp;
    use crate::syntax::*;
    use crate::tests_631::*;

    #[test]
    #[should_panic]
    fn bad_things() {
        parse_exp("(if 5 6 7 8)");
    }
    #[test]
    fn parse_int() {
        assert_eq!(parse_exp("5"), Exp::Lit(Lit::Int(5)))
    }
    #[test]
    fn let_once() {
        assert_eq!(
            parse_exp("(let ((x 5)) x)"),
            Exp::Let(
                "x".to_string(),
                Box::new(Exp::Lit(Lit::Int(5))),
                Box::new(Exp::Var("x".to_string()))
            )
        )
    }
    #[test]
    fn lambda() {
        parse_exp("(lambda (x) x)");
    }
    #[test]
    fn app() {
        parse_exp("((lambda (x) x) 5)");
    }
    #[test]
    fn cond() {
        parse_exp("(if 5 6 7)");
    }
    #[test]
    fn fact_grift_concrete() {
        exp_coerces(parse_exp(
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
        exp_succeeds(parse_exp(
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
        exp_succeeds(parse_exp(
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
        exp_succeeds(parse_exp(
            "(let ((my_box (box 5)))
                (let ((i_set (box-set! my_box 10)))
                  (unbox my_box)))",
        ));
    }
    #[test]
    fn box_any() {
        exp_coerces(parse_exp(
            "(let ((my_box (box 5)))
                (let ((i_set (box-set! my_box #t)))
                  (unbox my_box)))",
        ));
    }
    #[test]
    fn box_context() {
        exp_coerces(parse_exp("(box 5)"));
    }
    #[test]
    fn box_identities() {
        assert_eq!(
            exp_coerces(parse_exp(
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
            exp_coerces(parse_exp(
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
            exp_succeeds(parse_exp(
                "(let ((id (lambda (x) x))) (let ((h (id (box 5)))) 5))"
            )),
            Typ::Int
        );
    }
    #[test]
    fn multi_arg_lam() {
        parse_exp("(lambda (f n) (if (= n 0) 1 (f n)))");
    }
}

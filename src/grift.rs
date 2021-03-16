use crate::syntax::*;
use im_rc::HashMap;

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

type Env = HashMap<String, Typ>;
/// Types from [Grift primitives](https://github.com/Gradual-Typing/Grift/blob/master/src/language/primitives.rkt)
pub fn env() -> Env {
    let mut env = HashMap::new();

    add_ops(
        &mut env,
        &["<", ">", "=", ">=", "<="],
        &Typ::arrs(vec![Typ::Int, Typ::Int, Typ::Bool]),
    );

    env.insert(
        "binary-not".to_string(),
        Typ::arrs(vec![Typ::Int, Typ::Int]),
    );
    add_ops(
        &mut env,
        &[
            "+",
            "-",
            "*",
            "%/",
            "%>>",
            "%<<",
            "%%",
            "quotient",
            "binary-and",
            "binary-or",
            "binary-xor",
        ],
        &Typ::arrs(vec![Typ::Int, Typ::Int, Typ::Int]),
    );

    env.insert("not".to_string(), Typ::arrs(vec![Typ::Bool, Typ::Bool]));
    add_read_print(&mut env, "int", &Typ::Int);
    add_read_print(&mut env, "bool", &Typ::Bool);

    add_ops(
        &mut env,
        &["fl<", "fl>", "fl=", "fl>=", "fl<="],
        &Typ::arrs(vec![Typ::Float, Typ::Float, Typ::Bool]),
    );
    add_ops(
        &mut env,
        &[
            "fl+",
            "fl-",
            "fl*",
            "fl/",
            "flmodulo",
            "flexpt",
            "flmin",
            "flmax",
            "flquotient",
        ],
        &Typ::arrs(vec![Typ::Float, Typ::Float, Typ::Float]),
    );
    add_ops(
        &mut env,
        &[
            "flabs",
            "flround",
            "flfloor",
            "flceiling",
            "fltruncate",
            "flsin",
            "flcos",
            "fltan",
            "flasin",
            "flacos",
            "flatan",
            "fllog",
            "flep",
            "flsqrt",
            "flnegate",
        ],
        &Typ::arrs(vec![Typ::Float, Typ::Float]),
    );
    env.insert(
        "print-float".to_string(),
        Typ::arrs(vec![Typ::Float, Typ::Int, Typ::Unit]),
    );
    env.insert("read-float".to_string(), Typ::arrs(vec![Typ::Float]));

    env.insert(
        "float->int".to_string(),
        Typ::arrs(vec![Typ::Float, Typ::Int]),
    );
    env.insert(
        "int->float".to_string(),
        Typ::arrs(vec![Typ::Int, Typ::Float]),
    );

    
    env.insert(
        "char->int".to_string(),
        Typ::arrs(vec![Typ::Char, Typ::Int]),
    );
    env.insert(
        "int->char".to_string(),
        Typ::arrs(vec![Typ::Int, Typ::Char]),
    );
    add_read_print(&mut env, "char", &Typ::Char);
    env.insert("display-char".to_string(), Typ::arrs(vec![Typ::Char, Typ::Unit]));

    add_ops(
        &mut env,
        &["timer-start", "timer-stop", "timer-report"],
        &Typ::arrs(vec![Typ::Unit]),
    );

    env.insert("print".to_string(), Typ::arrs(vec![Typ::Str, Typ::Unit]));
    env.insert(
        "printf".to_string(),
        Typ::arrs(vec![Typ::Str, Typ::List(Box::new(Typ::Any)), Typ::Unit]),
    );
    env.insert("exit".to_string(), Typ::arrs(vec![Typ::Int, Typ::Any])); // TODO(mmg): return type should be Bottom

    env
}

fn add_read_print(env: &mut Env, name: &str, typ: &Typ) {
    env.insert(format!("read-{}", name), Typ::arrs(vec![typ.clone()]));
    env.insert(
        format!("print-{}", name),
        Typ::arrs(vec![typ.clone(), Typ::Unit]),
    );
}

fn add_ops(env: &mut Env, ops: &[&str], typ: &Typ) {
    for name in ops {
        env.insert(name.to_string(), typ.clone());
    }
}

#[cfg(test)]
mod test {
    use super::parse;
    use crate::syntax::*;
    use crate::tests_631::contains_coercions;
    use crate::type_check::tcheck;
    use crate::cgen::typeinf_options;

    fn compile_verbose(orig: Exp) -> (Typ, Exp) {
        let env = super::env();
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf_options(orig, &env, Default::default()).unwrap();
        println!("\nAfter type inference:\n{}", e);
        let t = tcheck(&env, &e).expect("failed to typecheck");
        println!("\nProgram type:\n{}", t);
        (t, e)
    }
    pub fn exp_succeeds(orig: Exp) -> Typ {
        let (t, e) = compile_verbose(orig);
        let coercions = contains_coercions(e);
        assert!(!coercions.0 && !coercions.1);
        t
    }
    pub fn exp_coerces(orig: Exp) -> Typ {
        let (t, e) = compile_verbose(orig);
        let coercions = contains_coercions(e);
        assert!(coercions.0 || coercions.1);
        t
    }

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
    #[test]
    fn int_ops() {
        assert_eq!(exp_succeeds(parse("(< (* 1 2) (+ 3 4))")), Typ::Bool);
    }
}

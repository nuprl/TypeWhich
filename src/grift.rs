use crate::parser::next_metavar;
use crate::syntax::*;
use lexpr::Value;

#[allow(unused)]
pub fn parse(program: &str) -> Exp {
    *parse_sexp(&lexpr::from_str(program).unwrap())
}

fn parse_sexp(e: &Value) -> Box<Exp> {
    Box::new(match e {
        Value::Symbol(s) => Exp::Var(s.to_string()),
        // TODO(luna): grift uses i61, so we should use i64
        Value::Number(n) => Exp::Lit(Lit::Int(n.as_i64().unwrap() as i32)),
        Value::Bool(b) => Exp::Lit(Lit::Bool(*b)),
        Value::Cons(atoms) => {
            let (first, rest) = atoms.as_pair();
            match first {
                Value::Symbol(s) => match &s[..] {
                    "if" => {
                        let mut it = rest.list_iter().unwrap();
                        let rv = Exp::If(
                            parse_sexp(&it.next().unwrap()),
                            parse_sexp(&it.next().unwrap()),
                            parse_sexp(&it.next().unwrap()),
                        );
                        assert_eq!(it.next(), None);
                        rv
                    }
                    "let" => {
                        let (bindings, bodies) = rest.as_pair().unwrap();
                        // these could probably be "curried" as well
                        // TODO(luna): multiple bindings
                        let binding = &bindings[0];
                        let id = expect_string(&binding[0]);
                        // TODO(luna): allow type
                        let exp = parse_sexp(&binding[1]);
                        let body = &bodies.as_pair().unwrap().0;
                        let body = parse_sexp(&body);
                        Exp::Let(id, exp, body)
                    }
                    "letrec" => {
                        let (bindings, bodies) = rest.as_pair().unwrap();
                        let bindings = bindings
                            .as_cons()
                            .unwrap()
                            .iter()
                            .map(|c| {
                                let binding = c.car();
                                let id = expect_string(&binding[0]);
                                // TODO(luna): allow type
                                let exp = *parse_sexp(&binding[1]);
                                (id, next_metavar(), exp)
                            })
                            .collect();
                        // TODO(luna): multiple bodies(?)
                        let body = &bodies.as_pair().unwrap().0;
                        let body = parse_sexp(&body);
                        Exp::LetRec(bindings, body)
                    }
                    "lambda" => {
                        let (bindings, list_body) = rest.as_pair().unwrap();
                        let ids_typs = bindings
                            .as_cons()
                            .unwrap()
                            .iter()
                            .map(|b| match b.car() {
                                Value::Symbol(s) => (s.to_string(), next_metavar()),
                                Value::Cons(l) => {
                                    let mut l = l.iter();
                                    let id = expect_string(&l.next().unwrap().car());
                                    assert_eq!(
                                        expect_string(&l.next().unwrap().car()),
                                        ":",
                                        "failed to parse id with type"
                                    );
                                    (id, parse_typ(&l.next().unwrap().car()))
                                }
                                _ => panic!("bad id, ty pair"),
                            })
                            .collect::<Vec<(String, Typ)>>();
                        let (typ, body) = match list_body.get(0).unwrap() {
                            Value::Symbol(s) if &**s == ":" => (
                                parse_typ(&list_body.get(1).unwrap()),
                                list_body.get(2).unwrap(),
                            ),
                            got => (next_metavar(), got),
                        };
                        // TODO(luna): do something with that return type
                        let body = parse_sexp(body);
                        *curry_lambda(&ids_typs, body)
                    }
                    "tuple" => {
                        // these could probably be "curried" as well
                        Exp::Pair(parse_sexp(&rest[0]), parse_sexp(&rest[1]))
                    }
                    "box" => Exp::Box(parse_sexp(&rest[0])),
                    "unbox" => Exp::Unbox(parse_sexp(&rest[0])),
                    "box-set!" => Exp::BoxSet(parse_sexp(&rest[0]), parse_sexp(&rest[1])),
                    "+" => Exp::Add(parse_sexp(&rest[0]), parse_sexp(&rest[1])),
                    "-" => {
                        let a = parse_sexp(&rest[0]);
                        let b = parse_sexp(&rest[1]);
                        if let Exp::Lit(Lit::Int(i)) = *b {
                            Exp::Add(a, Box::new(Exp::Lit(Lit::Int(-i))))
                        } else {
                            todo!("actual support for -");
                        }
                    }
                    "*" => Exp::Mul(parse_sexp(&rest[0]), parse_sexp(&rest[1])),
                    "=" => Exp::IntEq(parse_sexp(&rest[0]), parse_sexp(&rest[1])),
                    // TODO(luna): actual support
                    ":" => *parse_sexp(&rest[0]),
                    _ => *parse_left_rec(first, rest),
                },
                _ => *parse_left_rec(first, rest),
            }
        }
        _ => todo!("{}", e),
    })
}

fn parse_typ(se: &Value) -> Typ {
    match se {
        Value::Symbol(s) => match &s[..] {
            "Dyn" => Typ::Any,
            "Bool" => Typ::Bool,
            "Int" => Typ::Int,
            _ => todo!("{}", s),
        },
        Value::Cons(atoms) => {
            let (first, rest) = atoms.as_pair();
            match first {
                Value::Symbol(s) => match &s[..] {
                    "->" => curry_arr(rest),
                    "Tuple" => {
                        // TODO(luna): non-pair tuples
                        Typ::Pair(Box::new(parse_typ(&rest[0])), Box::new(parse_typ(&rest[1])))
                    }
                    _ => todo!("{}", s),
                },
                _ => panic!("not a type"),
            }
        }
        _ => panic!("not a type"),
    }
}

fn parse_left_rec(first: &Value, rest: &Value) -> Box<Exp> {
    let f = parse_sexp(first);
    let args: Vec<Box<Exp>> = rest
        .as_cons()
        .unwrap()
        .iter()
        .map(|a| parse_sexp(&a.car()))
        .collect();
    curry_app(f, &args)
}
fn curry_lambda(ids: &[(String, Typ)], body: Box<Exp>) -> Box<Exp> {
    if ids.is_empty() {
        body
    } else {
        let (id, rest) = ids.split_first().unwrap();
        let body = curry_lambda(rest, body);
        Box::new(Exp::Fun(id.0.clone(), id.1.clone(), body))
    }
}
fn curry_app(f: Box<Exp>, args: &[Box<Exp>]) -> Box<Exp> {
    if args.is_empty() {
        f
    } else {
        let (arg, rest) = args.split_first().unwrap();
        curry_app(Box::new(Exp::App(f, arg.clone())), rest)
    }
}
fn curry_arr(def: &Value) -> Typ {
    let (t, rest) = def.as_pair().unwrap();
    if rest == &Value::Null {
        parse_typ(t)
    } else {
        Typ::Arr(Box::new(parse_typ(t)), Box::new(curry_arr(rest)))
    }
}

fn expect_string(se: &Value) -> String {
    if let Value::Symbol(s) = se {
        s.to_string()
    } else {
        panic!("expected string");
    }
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
    fn fact_church() {
        coerces(
            "
            let add1  =
                  fun x. 1 + x in
            let one  =
                  fun f. fun x. f x in
            let five  =
                  fun f. fun x. f (f (f (f (f x)))) in
            let pred  =
                  fun n.
                    (fun f.
                      (fun x.
                        (((n (fun g. fun h. h (g f)))
                          (fun u. x))
                         (fun u. u)))) in
            let mult  =
                  fun m.
                    (fun n.
                      (fun f. m (n f))) in
            let _true   =
                   fun a. fun b. a in
            let _false  =
                   fun a. fun b. b in
            let is0   =
                  fun n. n (fun x. _false) _true in
            let fact  =
                  fix fact. fun n.
                    ((     (is0 n) // if
                           (fun x. one))
                           (fun x. (mult n) (fact (pred n)))) in
            let realize = fun n . n add1 0 in // : (int -> int) -> (int -> int)
            let n = fact five in
            realize n",
        );
    }
    #[test]
    fn fact_dyn() {
        coerces(
            "
            let f = fun f.fun n.
                if n = 0
                    then 1
                    else n * (f f (n + -1)) in
            f f 6",
        );
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
            "(let ((lol_no_rec (lambda ([m : Int] [n : Int]) : Int 5)))
                (let ([ack (lambda ([m : Int] [n : Int]) : Int
                    (if (= m 0)
                        (+ n 1)
                        (if (= n 0)
                            (lol_no_rec (+ m -1) 1)
                            (lol_no_rec (+ m -1) (lol_no_rec m (+ n -1))))))])
                  (ack 1 2)))",
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
}

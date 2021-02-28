use crate::parser::next_metavar;
use crate::syntax::*;
use sexp::{Atom, Sexp};

#[allow(unused)]
pub fn parse(program: &str) -> Exp {
    *parse_sexp(&sexp::parse(program).unwrap())
}

fn parse_sexp(e: &Sexp) -> Box<Exp> {
    Box::new(match e {
        Sexp::Atom(atom) => match atom {
            Atom::S(s) => Exp::Var(s.clone()),
            // TODO(luna): grift uses i61, so we should use i64
            Atom::I(i) => Exp::Lit(Lit::Int(*i as i32)),
            Atom::F(_) => todo!("floats"),
        },
        Sexp::List(atoms) => {
            let (first, rest) = atoms.split_first().unwrap();
            match first {
                Sexp::Atom(a) => match a {
                    Atom::S(s) => match &s[..] {
                        "if" => {
                            assert_eq!(rest.len(), 3, "too many expressions on if");
                            Exp::If(
                                parse_sexp(&rest[0]),
                                parse_sexp(&rest[1]),
                                parse_sexp(&rest[2]),
                            )
                        }
                        "let" => {
                            let (bindings, bodies) = rest.split_first().unwrap();
                            let bindings = expect_list(bindings);
                            // these could probably be "curried" as well
                            assert_eq!(bindings.len(), 1, "TODO(luna): multiple bindings");
                            let binding = expect_list(&bindings[0]);
                            assert_eq!(binding.len(), 2);
                            let id = expect_string(&binding[0]);
                            // TODO(luna): allow type
                            let exp = parse_sexp(&binding[1]);
                            assert_eq!(bodies.len(), 1, "TODO(luna): multiple bodies");
                            let body = &bodies[0];
                            let body = parse_sexp(&body);
                            Exp::Let(id, exp, body)
                        }
                        "lambda" => {
                            let (bindings, list_body) = rest.split_first().unwrap();
                            let bindings = expect_list(bindings);
                            let ids = bindings
                                .iter()
                                .map(|b| expect_string(b))
                                .collect::<Vec<String>>();
                            // TODO(luna): allow type
                            assert_eq!(list_body.len(), 1, "function had multiple bodies");
                            let body = parse_sexp(&list_body[0]);
                            *curry_lambda(&ids, body)
                        }
                        "tuple" => {
                            // these could probably be "curried" as well
                            assert_eq!(rest.len(), 2, "TODO(luna): non-pair tuples");
                            Exp::Pair(parse_sexp(&rest[0]), parse_sexp(&rest[1]))
                        }
                        "+" => {
                            assert_eq!(rest.len(), 2, "too many arguments to +");
                            Exp::Add(parse_sexp(&rest[0]), parse_sexp(&rest[1]))
                        }
                        "*" => {
                            assert_eq!(rest.len(), 2, "too many arguments to *");
                            Exp::Mul(parse_sexp(&rest[0]), parse_sexp(&rest[1]))
                        }
                        "=" => {
                            assert_eq!(rest.len(), 2, "too many arguments to =");
                            Exp::IntEq(parse_sexp(&rest[0]), parse_sexp(&rest[1]))
                        }
                        _ => *parse_left_rec(first, rest),
                    },
                    _ => panic!("begining a list with an integer/float"),
                },
                _ => *parse_left_rec(first, rest),
            }
        }
    })
}

fn parse_typ(se: &Sexp) -> Typ {
    match se {
        Sexp::Atom(a) => match a {
            Atom::S(s) => match &s[..] {
                "Dyn" => Typ::Any,
                "Bool" => Typ::Bool,
                "Int" => Typ::Int,
                _ => todo!("{}", s),
            },
            _ => panic!("not a type"),
        },
        Sexp::List(atoms) => {
            let (first, rest) = atoms.split_first().unwrap();
            match first {
                Sexp::Atom(a) => match a {
                    Atom::S(s) => match &s[..] {
                        "->" => {
                            Typ::Arr(Box::new(parse_typ(&rest[0])), Box::new(parse_typ(&rest[1])))
                        }
                        "Tuple" => {
                            assert_eq!(rest.len(), 2, "TODO(luna): non-pair tuples");
                            Typ::Pair(Box::new(parse_typ(&rest[0])), Box::new(parse_typ(&rest[1])))
                        }
                        _ => todo!("{}", s),
                    },
                    _ => panic!("not a type"),
                },
                Sexp::List(a) => panic!("not a type"),
            }
        }
    }
}

fn parse_left_rec(first: &Sexp, rest: &[Sexp]) -> Box<Exp> {
    let f = parse_sexp(first);
    let args: Vec<Box<Exp>> = rest.iter().map(|a| parse_sexp(a)).collect();
    curry_app(f, &args)
}
fn curry_lambda(ids: &[String], body: Box<Exp>) -> Box<Exp> {
    if ids.len() == 0 {
        body
    } else {
        let (id, rest) = ids.split_first().unwrap();
        let body = curry_lambda(rest, body);
        Box::new(Exp::Fun(id.clone(), next_metavar(), body))
    }
}
fn curry_app(f: Box<Exp>, args: &[Box<Exp>]) -> Box<Exp> {
    if args.len() == 0 {
        f
    } else {
        let (arg, rest) = args.split_first().unwrap();
        curry_app(Box::new(Exp::App(f, arg.clone())), rest)
    }
}
fn curry_arr(first: Typ, rest: &[Typ]) -> Typ {
    if rest.len() == 0 {
        first
    } else {
        let (t, rest) = rest.split_first().unwrap();
        curry_arr(Typ::Arr(Box::new(first), Box::new(t.clone())), rest)
    }
}

fn expect_string(se: &Sexp) -> String {
    if let Sexp::Atom(a) = se {
        if let Atom::S(s) = a {
            s.clone()
        } else {
            panic!("expected string");
        }
    } else {
        panic!("expected atom");
    }
}
/// actually gives back a Vec because this library is a bit annoying
fn expect_list(se: &Sexp) -> &Vec<Sexp> {
    if let Sexp::List(l) = se {
        l
    } else {
        panic!("expected list");
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
        ))
    }
}

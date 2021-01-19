use super::parser::next_metavar;
use super::syntax::*;
use im_rc::HashMap;
use std::cell::RefCell;
use z3;
use z3::ast::Ast;
use z3::{ast, ast::Dynamic, SatResult, Sort};

type Env = HashMap<String, Typ>;

struct State<'a> {
    cxt: &'a z3::Context,
    int_ctor: &'a z3::FuncDecl<'a>,
    bool_ctor: &'a z3::FuncDecl<'a>,
    arr_ctor: &'a z3::FuncDecl<'a>,
    solver: z3::Solver<'a>,
    vars: RefCell<HashMap<u32, Dynamic<'a>>>,
    typ_sort: &'a z3::Sort<'a>,
}

impl<'a> State<'a> {
    fn typ_to_z3ast(&self, typ: &Typ) -> Dynamic<'a> {
        match typ {
            Typ::Int => self.int_ctor.apply(&[]),
            Typ::Bool => self.bool_ctor.apply(&[]),
            Typ::Arr(t1, t2) => self
                .arr_ctor
                .apply(&[&self.typ_to_z3ast(t1), &self.typ_to_z3ast(t2)]),
            Typ::Metavar(n) => {
                let mut vars = self.vars.borrow_mut();
                match vars.get(n) {
                    Some(ast) => ast.clone(),
                    None => {
                        let t = z3::ast::Datatype::fresh_const(&self.cxt, "metavar", self.typ_sort);
                        let x = Dynamic::from_ast(&t);
                        vars.insert(*n, x.clone());
                        x
                    }
                }
            }
        }
    }

    fn z3_true(&self) -> ast::Bool<'_> {
        ast::Bool::from_bool(self.cxt, true)
    }

    fn cgen(&self, env: &Env, exp: &Exp) -> (Typ, z3::ast::Bool<'_>) {
        match exp {
            Exp::Lit(lit) => (lit.typ(), self.z3_true()),
            Exp::Var(x) => (
                env.get(x).expect("unbound identifier").clone(),
                self.z3_true(),
            ),
            Exp::Fun(x, t, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t.clone());
                let (t_body, phi) = self.cgen(&env, body);
                (Typ::Arr(Box::new(t.clone()), Box::new(t_body)), phi)
            }
            Exp::App(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar();
                let t = Typ::Arr(Box::new(t2), Box::new(alpha.clone()));
                // In ordinary HM, we would create a new metavariable 'alpha' and produce the constraint
                // 't1 = t2 -> alpha'. However, we cannot express this equality in propositional
                // logic + uninterpreted functions.
                // create the con
                let phi = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&t));
                (alpha, ast::Bool::and(self.cxt, &[&phi1, &phi2, &phi]))
            }
            Exp::Add(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let phi3 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Int));
                let phi4 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Int));
                (
                    Typ::Int,
                    ast::Bool::and(self.cxt, &[&phi1, &phi2, &phi3, &phi4]),
                )
            }
        }
    }
}

fn typeinf(exp: &Exp) -> Result<(), ()> {
    let cfg = z3::Config::new();
    let cxt = z3::Context::new(&cfg);

    let typ = z3::DatatypeBuilder::new(&cxt, "Typ")
        .variant("Int", vec![])
        .variant("Bool", vec![])
        .variant(
            "Arr",
            vec![
                ("arg", z3::DatatypeAccessor::Datatype("Typ".into())),
                ("ret", z3::DatatypeAccessor::Datatype("Typ".into())),
            ],
        )
        .finish();

    let s = State {
        cxt: &cxt,
        int_ctor: &typ.variants[0].constructor,
        bool_ctor: &typ.variants[1].constructor,
        arr_ctor: &typ.variants[2].constructor,
        solver: z3::Solver::new(&cxt),
        vars: Default::default(),
        typ_sort: &typ.sort,
    };

    let solver = z3::Solver::new(&cxt);
    let (_, phi) = s.cgen(&Default::default(), exp);
    solver.assert(&phi);
    match solver.check() {
        SatResult::Unsat => return Err(()),
        SatResult::Unknown => panic!("unknown from Z3 -- very bad"),
        SatResult::Sat => (),
    }
    let model = solver.get_model().expect("model not available");
    println!("{}", model);
    // TODO(arjun): Read the model out and produce a map from metavars to types
    return Ok(());
}

#[cfg(test)]
mod test {
    use super::super::parser::parse;
    use super::typeinf;

    #[test]
    fn test_typeinf() {
        typeinf(&parse("(fun x . x) 10 ")).unwrap();
    }

    #[test]
    fn test_typeinf_err() {
        // In HM, this would be an occurs-check failure
        typeinf(&parse("(fun f . f f)")).unwrap_err();
    }

    #[test]
    fn test_typeinf_add() {
        typeinf(&parse("(fun x . x + 20) 10 ")).unwrap();
    }

    #[test]
    fn test_typeinf_err_add() {
        typeinf(&parse("(fun x . fun y . x + y) 10 false")).unwrap_err();
    }

    #[test]
    fn test_typeinf_err_add_app() {
        typeinf(&parse("fun f . f 10 + f true")).unwrap_err();
    }
}

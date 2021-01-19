use super::parser::next_metavar;
use super::syntax::*;
use im_rc::HashMap;
use std::cell::RefCell;
use z3;
use z3::ast::Ast;
use z3::{ast, ast::Dynamic, Model, SatResult, Sort};

type Env = HashMap<String, Typ>;

struct State<'a> {
    cxt: &'a z3::Context,
    int_ctor: &'a z3::FuncDecl<'a>,
    bool_ctor: &'a z3::FuncDecl<'a>,
    str_ctor: &'a z3::FuncDecl<'a>,
    arr_ctor: &'a z3::FuncDecl<'a>,
    typ: &'a z3::DatatypeSort<'a>,
    solver: z3::Solver<'a>,
    vars: RefCell<HashMap<u32, Dynamic<'a>>>,
    typ_sort: &'a Sort<'a>,
}

impl<'a> State<'a> {
    fn typ_to_z3ast(&self, typ: &Typ) -> Dynamic<'a> {
        match typ {
            Typ::Int => self.int_ctor.apply(&[]),
            Typ::Bool => self.bool_ctor.apply(&[]),
            Typ::Str => self.str_ctor.apply(&[]),
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
            Exp::Add(op_t, e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let phi3 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Int));
                let phi4 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Int));
                let int_case = ast::Bool::and(
                    self.cxt,
                    &[
                        &phi3,
                        &phi4,
                        &self.typ_to_z3ast(&op_t)._eq(&self.typ_to_z3ast(&Typ::Int)),
                    ],
                );
                let phi5 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Str));
                let phi6 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Str));
                let str_case = ast::Bool::and(
                    self.cxt,
                    &[
                        &phi5,
                        &phi6,
                        &self.typ_to_z3ast(&op_t)._eq(&self.typ_to_z3ast(&Typ::Str)),
                    ],
                );
                let add_constraints = ast::Bool::or(self.cxt, &[&int_case, &str_case]);
                (
                    Typ::Int,
                    ast::Bool::and(self.cxt, &[&phi1, &phi2, &add_constraints]),
                )
            }
        }
    }

    fn is_int<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Bool
    where
        'a: 'b,
    {
        self.typ.variants[0].tester.apply(&[&e]).as_bool().unwrap()
    }

    fn is_bool<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Bool
    where
        'a: 'b,
    {
        self.typ.variants[1].tester.apply(&[e]).as_bool().unwrap()
    }

    fn is_str<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Bool
    where
        'a: 'b,
    {
        self.typ.variants[2].tester.apply(&[e]).as_bool().unwrap()
    }

    fn is_arr<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Bool
    where
        'a: 'b,
    {
        self.typ.variants[3].tester.apply(&[e]).as_bool().unwrap()
    }

    fn arr_arg<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Dynamic
    where
        'a: 'b,
    {
        self.typ.variants[3].accessors[0].apply(&[e])
    }

    fn arr_ret<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Dynamic
    where
        'a: 'b,
    {
        self.typ.variants[3].accessors[1].apply(&[e])
    }

    fn z3_to_typ<'b>(&'b self, model: &'b Model, e: ast::Dynamic) -> Typ
    where
        'a: 'b,
    {
        if model.eval(&self.is_int(&e)).unwrap().as_bool().unwrap() {
            Typ::Int
        } else if model.eval(&self.is_bool(&e)).unwrap().as_bool().unwrap() {
            Typ::Bool
        } else if model.eval(&self.is_str(&e)).unwrap().as_bool().unwrap() {
            Typ::Str
        } else {
            let arg = self.arr_arg(&e);
            let arg = model.eval(&arg).unwrap();
            let ret = self.arr_ret(&e);
            let ret = model.eval(&ret).unwrap();
            let t1 = self.z3_to_typ(model, arg);
            let t2 = self.z3_to_typ(model, ret);
            Typ::Arr(Box::new(t1), Box::new(t2))
        }
    }
}

fn annotate(env: &HashMap<u32, Typ>, exp: &mut Exp) {
    match &mut *exp {
        Exp::Lit(_) => {}
        Exp::Var(_) => {}
        Exp::Fun(_, t, e) => {
            *t = env.get(&t.expect_metavar()).unwrap().clone();
            annotate(env, e);
        }
        Exp::Add(t, e1, e2) => {
            *t = env.get(&t.expect_metavar()).unwrap().clone();
            annotate(env, e1);
            annotate(env, e2);
        }
        Exp::App(e1, e2) => {
            annotate(env, e1);
            annotate(env, e2);
        }
    }
}

pub fn typeinf(exp: &Exp) -> Result<Exp, ()> {
    let cfg = z3::Config::new();
    let cxt = z3::Context::new(&cfg);

    let typ = z3::DatatypeBuilder::new(&cxt, "Typ")
        .variant("Int", vec![])
        .variant("Bool", vec![])
        .variant("Str", vec![])
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
        str_ctor: &typ.variants[2].constructor,
        arr_ctor: &typ.variants[3].constructor,
        solver: z3::Solver::new(&cxt),
        vars: Default::default(),
        typ_sort: &typ.sort,
        typ: &typ,
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

    let mut result = HashMap::new();
    for (x, x_ast) in s.vars.borrow().iter() {
        let x_val_ast = model.eval(x_ast).expect("evaluating metavar");
        // let x_val_ast = x_val_ast.as_datatype().expect("expected datatype");
        result.insert(*x, s.z3_to_typ(&model, x_val_ast));
    }
    let mut e = exp.clone();
    annotate(&result, &mut e);
    return Ok(e);
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

    #[test]
    fn str_add() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . x + x) "everything is ""#)).unwrap()
        );
    }

    #[test]
    fn cannot_add_str_int() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . fun y . x + y) "everything is " 10"#)).unwrap_err()
        );
    }

    #[test]
    fn infer_arr() {
        println!("{:?}", typeinf(&parse("fun f . f 200")).unwrap());
    }

    #[test]
    fn ambiguous_add() {
        println!("{:?}", typeinf(&parse("fun x . x + x")).unwrap());
    }
}

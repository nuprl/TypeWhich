use super::parser::next_metavar_typ;
use super::syntax::*;
use im_rc::HashMap;
use std::cell::RefCell;
use z3;
use z3::ast::{Ast, Bool};
use z3::{ast, ast::Dynamic, Model, SatResult, Sort};

type Env = HashMap<String, Typ>;

struct State<'a> {
    cxt: &'a z3::Context,
    int_ctor: &'a z3::FuncDecl<'a>,
    bool_ctor: &'a z3::FuncDecl<'a>,
    str_ctor: &'a z3::FuncDecl<'a>,
    arr_ctor: &'a z3::FuncDecl<'a>,
    any_ctor: &'a z3::FuncDecl<'a>,
    typ: &'a z3::DatatypeSort<'a>,
    solver: z3::Solver<'a>,
    vars: RefCell<HashMap<u32, Dynamic<'a>>>,
    coercions: RefCell<HashMap<u32, Bool<'a>>>,
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
            Typ::Any => self.any_ctor.apply(&[]),
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

    fn coercion_to_z3(&self, coercion: u32) -> Bool<'a> {
        let mut coercions = self.coercions.borrow_mut();
        match coercions.get(&coercion) {
            Some(ast) => ast.clone(),
            None => {
                let t = z3::ast::Bool::fresh_const(&self.cxt, "coercion-metavar");
                coercions.insert(coercion, t.clone());
                t
            }
        }
    }

    fn z3_true(&self) -> ast::Bool<'_> {
        ast::Bool::from_bool(self.cxt, true)
    }

    fn cgen(&self, env: &Env, exp: &Exp) -> (Typ, z3::ast::Bool<'_>) {
        match exp {
            // ---------------------------
            // Γ ⊢ lit : (lit.typ(), true)
            Exp::Lit(lit) => (lit.typ(), self.z3_true()),
            // ---------------------------
            // Γ ⊢ x : (Γ(x), true)
            Exp::Var(x) => (
                env.get(x).expect("unbound identifier").clone(),
                self.z3_true(),
            ),
            // Γ,x:T ⊢ e : (T_2, φ)
            // ---------------------------------------
            // Γ ⊢ fun (x : T_1) . e : (T_1 -> T_2, φ)
            Exp::Fun(x, t, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t.clone());
                let (t_body, phi) = self.cgen(&env, body);
                (Typ::Arr(Box::new(t.clone()), Box::new(t_body)), phi)
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 e_2 : (α, φ_1 && φ_2 && T_1 -> α = T_2)
            Exp::App(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar_typ();
                let t = Typ::Arr(Box::new(t2), Box::new(alpha.clone()));
                let phi = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&t));
                (alpha, ast::Bool::and(self.cxt, &[&phi1, &phi2, &phi]))
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 + e_2 : (α, φ_1 && φ_2 && (T_1 = T_2 = α = int ||
            //                                    T_1 = T_2 = α = str ||
            //                                    T_1 = T_2 = α = any))
            Exp::Add(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let phi3 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Int));
                let phi4 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Int));
                let int_case = ast::Bool::and(self.cxt, &[&phi3, &phi4]);
                let phi5 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Str));
                let phi6 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Str));
                let str_case = ast::Bool::and(self.cxt, &[&phi5, &phi6]);
                let phi7 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Any));
                let phi8 = self.typ_to_z3ast(&t2)._eq(&self.typ_to_z3ast(&Typ::Any));
                let any_case = ast::Bool::and(self.cxt, &[&phi7, &phi8]);
                let add_constraints = ast::Bool::or(self.cxt, &[&int_case, &str_case, &any_case]);
                (
                    Typ::Int,
                    ast::Bool::and(self.cxt, &[&phi1, &phi2, &add_constraints]),
                )
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ MaybeToAny (cα, e) : (α, φ && ((cα = false && α = T) ||
            //                                    (cα = true && α = any && T != any)))
            Exp::MaybeToAny(calpha, e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar_typ();
                let phi2 = Bool::or(
                    self.cxt,
                    &[
                        &Bool::and(
                            self.cxt,
                            &[
                                &Bool::not(&self.coercion_to_z3(*calpha)),
                                &self.typ_to_z3ast(&alpha)._eq(&self.typ_to_z3ast(&t)),
                            ],
                        ),
                        &Bool::and(
                            self.cxt,
                            &[
                                &self.coercion_to_z3(*calpha),
                                &self.typ_to_z3ast(&alpha)._eq(&self.typ_to_z3ast(&Typ::Any)),
                                &Bool::not(
                                    &self.typ_to_z3ast(&t)._eq(&self.typ_to_z3ast(&Typ::Any)),
                                ),
                            ],
                        ),
                    ],
                );
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ MaybeFromAny (cα, e) : (α, φ && ((cα = false && α = T) ||
            //                                      (cα = true && α != any && T = any)))
            Exp::MaybeFromAny(calpha, e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar_typ();
                let phi2 = Bool::or(
                    self.cxt,
                    &[
                        &Bool::and(
                            self.cxt,
                            &[
                                &Bool::not(&self.coercion_to_z3(*calpha)),
                                &self.typ_to_z3ast(&alpha)._eq(&self.typ_to_z3ast(&t)),
                            ],
                        ),
                        &Bool::and(
                            self.cxt,
                            &[
                                &self.coercion_to_z3(*calpha),
                                &Bool::not(
                                    &self.typ_to_z3ast(&alpha)._eq(&self.typ_to_z3ast(&Typ::Any)),
                                ),
                                &self.typ_to_z3ast(&t)._eq(&self.typ_to_z3ast(&Typ::Any)),
                            ],
                        ),
                    ],
                );
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ ToAny (e) : (any, φ && T != any)
            Exp::ToAny(e) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = Bool::not(&self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Any)));
                (Typ::Any, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ ToAny (e) : (α, φ && T = any)
            Exp::FromAny(e) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = self.typ_to_z3ast(&t1)._eq(&self.typ_to_z3ast(&Typ::Any));
                let alpha = next_metavar_typ();
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
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

    fn is_any<'b>(&'b self, e: &'b ast::Dynamic) -> Bool
    where
        'a: 'b,
    {
        self.typ.variants[4].tester.apply(&[e]).as_bool().unwrap()
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
        } else if model.eval(&self.is_arr(&e)).unwrap().as_bool().unwrap() {
            let arg = self.arr_arg(&e);
            let arg = model.eval(&arg).unwrap();
            let ret = self.arr_ret(&e);
            let ret = model.eval(&ret).unwrap();
            let t1 = self.z3_to_typ(model, arg);
            let t2 = self.z3_to_typ(model, ret);
            Typ::Arr(Box::new(t1), Box::new(t2))
        } else if model.eval(&self.is_any(&e)).unwrap().as_bool().unwrap() {
            Typ::Any
        } else {
            panic!("missing case in z3_to_typ");
        }
    }
}

fn annotate<'a>(env: &HashMap<u32, Typ>, coercions: &HashMap<u32, bool>, exp: &mut Exp) {
    match &mut *exp {
        Exp::Lit(_) => {}
        Exp::Var(_) => {}
        Exp::Fun(_, t, e) => {
            *t = env.get(&t.expect_metavar()).unwrap().clone();
            annotate(env, coercions, e);
        }
        Exp::MaybeToAny(coercion, e) => {
            annotate(env, coercions, e);
            if *coercions.get(&coercion).unwrap() {
                *exp = Exp::ToAny(Box::new(e.take()));
            } else {
                *exp = e.take();
            }
        }
        Exp::MaybeFromAny(coercion, e) => {
            annotate(env, coercions, e);
            if *coercions.get(&coercion).unwrap() {
                *exp = Exp::FromAny(Box::new(e.take()));
            } else {
                *exp = e.take();
            }
        }
        Exp::App(e1, e2) | Exp::Add(e1, e2) => {
            annotate(env, coercions, e1);
            annotate(env, coercions, e2);
        }
        Exp::ToAny(e) | Exp::FromAny(e) => {
            annotate(env, coercions, e);
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
        .variant("Any", vec![])
        .finish();

    let s = State {
        cxt: &cxt,
        int_ctor: &typ.variants[0].constructor,
        bool_ctor: &typ.variants[1].constructor,
        str_ctor: &typ.variants[2].constructor,
        arr_ctor: &typ.variants[3].constructor,
        any_ctor: &typ.variants[4].constructor,
        solver: z3::Solver::new(&cxt),
        vars: Default::default(),
        coercions: Default::default(),
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
    eprintln!("{}", model);
    let mut result = HashMap::new();
    for (x, x_ast) in s.vars.borrow().iter() {
        let x_val_ast = model.eval(x_ast).expect("evaluating metavar");
        result.insert(*x, s.z3_to_typ(&model, x_val_ast));
    }
    let mut coercions = HashMap::new();
    for (x, x_ast) in s.coercions.borrow().iter() {
        let x_val_ast = model.eval(x_ast).expect("evaluating coercion-metavar");
        coercions.insert(
            *x,
            x_val_ast.as_bool().expect("didn't resolve coercion value"),
        );
    }
    let mut e = exp.clone();
    annotate(&result, &coercions, &mut e);
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
    fn occurs_check_fun_any() {
        // In HM, this would be an occurs-check failure
        println!("{:?}", typeinf(&parse("fun f . f f")).unwrap())
    }

    #[test]
    fn test_typeinf_add() {
        typeinf(&parse("(fun x . x + 20) 10 ")).unwrap();
    }

    #[test]
    fn str_add() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . x + x) "everything is ""#)).unwrap()
        );
    }

    #[test]
    fn add_str_int_any() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . fun y . x + y) "everything is " 10"#)).unwrap()
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

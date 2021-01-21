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
    int_z3: &'a Dynamic<'a>,
    bool_z3: &'a Dynamic<'a>,
    str_z3: &'a Dynamic<'a>,
    arr_ctor: &'a z3::FuncDecl<'a>,
    any_z3: &'a Dynamic<'a>,
    typ: &'a z3::DatatypeSort<'a>,
    vars: RefCell<HashMap<u32, Dynamic<'a>>>,
    coercions: RefCell<HashMap<u32, Bool<'a>>>,
    typ_sort: &'a Sort<'a>,
}

impl<'a> State<'a> {
    fn t2z3(&self, typ: &Typ) -> Dynamic<'a> {
        match typ {
            Typ::Int => self.int_z3.clone(),
            Typ::Bool => self.bool_z3.clone(),
            Typ::Str => self.str_z3.clone(),
            Typ::Arr(t1, t2) => self.arr_ctor.apply(&[&self.t2z3(t1), &self.t2z3(t2)]),
            Typ::Any => self.any_z3.clone(),
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

    fn c2z3(&self, coercion: u32) -> Bool<'a> {
        let mut coercions = self.coercions.borrow_mut();
        match coercions.get(&coercion) {
            Some(ast) => ast.clone(),
            None => {
                let t = Bool::fresh_const(&self.cxt, "coercion-metavar");
                coercions.insert(coercion, t.clone());
                t
            }
        }
    }

    fn z3_true(&self) -> Bool<'_> {
        Bool::from_bool(self.cxt, true)
    }

    fn cgen(&self, env: &Env, exp: &Exp) -> (Typ, Bool<'_>) {
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
                let phi = self.t2z3(&t1)._eq(&self.t2z3(&t));
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2, &phi]))
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
                let t1 = self.t2z3(&t1);
                let t2 = self.t2z3(&t2);
                let int_case = Bool::and(self.cxt, &[&t1._eq(self.int_z3), &t2._eq(self.int_z3)]);
                let str_case = Bool::and(self.cxt, &[&t1._eq(self.str_z3), &t2._eq(self.str_z3)]);
                let any_case = Bool::and(self.cxt, &[&t1._eq(self.any_z3), &t2._eq(self.any_z3)]);
                let add_constraints = Bool::or(self.cxt, &[&int_case, &str_case, &any_case]);
                (
                    Typ::Int,
                    Bool::and(self.cxt, &[&phi1, &phi2, &add_constraints]),
                )
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // Γ ⊢ e_3 : (T_3, φ_3)
            // ----------------------------------------------
            // Γ ⊢ e_1 + e_2 : (α, φ_1 && φ_2 && φ_3 && T_1 = bool &&
            //                                          T_2 = T_3
            Exp::If(e1, e2, e3) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let (t3, phi3) = self.cgen(&env, e3);
                let phi4 = self.t2z3(&t1)._eq(self.bool_z3);
                let phi5 = self.t2z3(&t2)._eq(&self.t2z3(&t3));
                (
                    t2,
                    Bool::and(self.cxt, &[&phi1, &phi2, &phi3, &phi4, &phi5]),
                )
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ MaybeToAny (cα, e) : (α, φ && ((cα = false && α = T) ||
            //                                    (cα = true && α = any && T != any)))
            Exp::MaybeToAny(calpha, e) => {
                let calpha = self.c2z3(*calpha);
                let (t, phi1) = self.cgen(env, e);
                let t = self.t2z3(&t);
                let alpha = next_metavar_typ();
                let dont_coerce_case =
                    Bool::and(self.cxt, &[&Bool::not(&calpha), &self.t2z3(&alpha)._eq(&t)]);
                let do_coerce_case = Bool::and(
                    self.cxt,
                    &[
                        &calpha,
                        &self.t2z3(&alpha)._eq(self.any_z3),
                        &Bool::not(&t._eq(&self.any_z3)),
                    ],
                );
                let phi2 = Bool::or(self.cxt, &[&dont_coerce_case, &do_coerce_case]);
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ MaybeFromAny (cα, e) : (α, φ && ((cα = false && α = T) ||
            //                                      (cα = true && α != any && T = any)))
            Exp::MaybeFromAny(calpha, e) => {
                let calpha = self.c2z3(*calpha);
                let (t, phi1) = self.cgen(env, e);
                let t = self.t2z3(&t);
                let alpha = next_metavar_typ();
                let dont_coerce_case =
                    Bool::and(self.cxt, &[&Bool::not(&calpha), &self.t2z3(&alpha)._eq(&t)]);
                let do_coerce_case = Bool::and(
                    self.cxt,
                    &[
                        &calpha,
                        &Bool::not(&self.t2z3(&alpha)._eq(self.any_z3)),
                        &t._eq(self.any_z3),
                    ],
                );
                let phi2 = Bool::or(self.cxt, &[&dont_coerce_case, &do_coerce_case]);
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ ToAny (e) : (any, φ && T != any)
            Exp::ToAny(e) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = Bool::not(&self.t2z3(&t1)._eq(self.any_z3));
                (Typ::Any, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ ToAny (e) : (α, φ && T = any)
            Exp::FromAny(e) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = self.t2z3(&t1)._eq(self.any_z3);
                let alpha = next_metavar_typ();
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
        }
    }

    fn is_variant(&self, i: usize, model: &Model, e: &ast::Dynamic) -> bool {
        model
            .eval(&self.typ.variants[i].tester.apply(&[&e]).as_bool().unwrap())
            .unwrap()
            .as_bool()
            .unwrap()
    }
    fn is_int(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(0, model, e)
    }
    fn is_bool(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(1, model, e)
    }
    fn is_str(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(2, model, e)
    }
    fn is_arr(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(3, model, e)
    }
    fn is_any(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(4, model, e)
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
        if self.is_int(model, &e) {
            Typ::Int
        } else if self.is_bool(model, &e) {
            Typ::Bool
        } else if self.is_str(model, &e) {
            Typ::Str
        } else if self.is_arr(model, &e) {
            let arg = self.arr_arg(&e);
            let arg = model.eval(&arg).unwrap();
            let ret = self.arr_ret(&e);
            let ret = model.eval(&ret).unwrap();
            let t1 = self.z3_to_typ(model, arg);
            let t2 = self.z3_to_typ(model, ret);
            Typ::Arr(Box::new(t1), Box::new(t2))
        } else if self.is_any(model, &e) {
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
        Exp::ToAny(e) | Exp::FromAny(e) => {
            annotate(env, coercions, e);
        }
        Exp::App(e1, e2) | Exp::Add(e1, e2) => {
            annotate(env, coercions, e1);
            annotate(env, coercions, e2);
        }
        Exp::If(e1, e2, e3) => {
            annotate(env, coercions, e1);
            annotate(env, coercions, e2);
            annotate(env, coercions, e3);
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
        int_z3: &typ.variants[0].constructor.apply(&[]),
        bool_z3: &typ.variants[1].constructor.apply(&[]),
        str_z3: &typ.variants[2].constructor.apply(&[]),
        arr_ctor: &typ.variants[3].constructor,
        any_z3: &typ.variants[4].constructor.apply(&[]),
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

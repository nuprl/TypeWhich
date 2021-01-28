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
    list_ctor: &'a z3::FuncDecl<'a>,
    pair_ctor: &'a z3::FuncDecl<'a>,
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
            Typ::List(t) => self.list_ctor.apply(&[&self.t2z3(t)]),
            Typ::Pair(t1, t2) => self.pair_ctor.apply(&[&self.t2z3(t1), &self.t2z3(t2)]),
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
                env.get(x)
                    .unwrap_or_else(|| panic!("unbound identifier {}", x))
                    .clone(),
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
            // Γ,x:T_1 ⊢ e : (T_2, φ)
            // ---------------------------------------
            // Γ ⊢ fix (x : T_1) . e : (T_1, φ && T_1 = T_2)
            Exp::Fix(x, t1, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t1.clone());
                let (t2, phi1) = self.cgen(&env, body);
                let phi2 = self.t2z3(t1)._eq(&self.t2z3(&t2));
                (t1.clone(), Bool::and(self.cxt, &[&phi1, &phi2]))
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
            // Γ ⊢ e1 : (T_1, φ_1)
            // Γ,x:T_1 ⊢ e2 : (T_2, φ_2)
            // ---------------------------------------
            // Γ ⊢ let x: α = e1 in e2 : (T_2, φ_1 && φ_2 && α == T_1)
            Exp::Let(x, alpha, e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let phi3 = self.t2z3(alpha)._eq(&self.t2z3(&t1));
                let mut env = env.clone();
                env.insert(x.clone(), t1);
                let (t2, phi2) = self.cgen(&env, e2);
                (t2, Bool::and(self.cxt, &[&phi1, &phi2, &phi3]))
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 [+*] e_2 : (int, φ_1 && φ_2 && T_1 = T_2 = int)
            Exp::Add(e1, e2) | Exp::Mul(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let t1 = self.t2z3(&t1);
                let t2 = self.t2z3(&t2);
                let int_case = Bool::and(self.cxt, &[&t1._eq(self.int_z3), &t2._eq(self.int_z3)]);
                (Typ::Int, Bool::and(self.cxt, &[&phi1, &phi2, &int_case]))
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 = e_2 : (bool, φ_1 && φ_2 && T_1 = T_2 = int)
            Exp::IntEq(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let t1 = self.t2z3(&t1);
                let t2 = self.t2z3(&t2);
                let int_case = Bool::and(self.cxt, &[&t1._eq(self.int_z3), &t2._eq(self.int_z3)]);
                (Typ::Bool, Bool::and(self.cxt, &[&phi1, &phi2, &int_case]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ not e : (bool, φ && T = bool)
            Exp::Not(e) => {
                let (t, phi1) = self.cgen(&env, e);
                let phi2 = self.t2z3(&t)._eq(self.bool_z3);
                (Typ::Bool, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 +? e_2 : (α, φ_1 && φ_2 && (T_1 = T_2 = α = int ||
            //                                     T_1 = T_2 = α = str ||
            //                                     T_1 = T_2 = α = any))
            Exp::AddOverload(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar_typ();
                let t1 = self.t2z3(&t1);
                let t2 = self.t2z3(&t2);
                let a_z3 = self.t2z3(&alpha);
                let int_case = Bool::and(
                    self.cxt,
                    &[
                        &t1._eq(self.int_z3),
                        &t2._eq(self.int_z3),
                        &a_z3._eq(self.int_z3),
                    ],
                );
                let str_case = Bool::and(
                    self.cxt,
                    &[
                        &t1._eq(self.str_z3),
                        &t2._eq(self.str_z3),
                        &a_z3._eq(self.str_z3),
                    ],
                );
                let any_case = Bool::and(
                    self.cxt,
                    &[
                        &t1._eq(self.any_z3),
                        &t2._eq(self.any_z3),
                        &a_z3._eq(self.any_z3),
                    ],
                );
                let add_constraints = Bool::or(self.cxt, &[&int_case, &str_case, &any_case]);
                (
                    alpha,
                    Bool::and(self.cxt, &[&phi1, &phi2, &add_constraints]),
                )
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // Γ ⊢ e_3 : (T_3, φ_3)
            // ----------------------------------------------
            // Γ ⊢ if e_1 then e_2 else e_3 : (α, φ_1 && φ_2 && φ_3 &&
            //                                    T_1 = bool && T_2 = T_3)
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
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1, e_2 : ((T_1, T_2), φ_1 && φ_2)
            Exp::Pair(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                (
                    Typ::Pair(Box::new(t1), Box::new(t2)),
                    Bool::and(self.cxt, &[&phi1, &phi2]),
                )
            }
            // Γ ⊢ e_1 : (T_1, φ_1)
            // Γ ⊢ e_2 : (T_2, φ_2)
            // ----------------------------------------------
            // Γ ⊢ e_1 :: e_2 : (T_2, φ_1 && φ_2 && List(T_1) = T_2)
            Exp::Cons(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let list_t1 = self.t2z3(&Typ::List(Box::new(t1)));
                let phi3 = list_t1._eq(&self.t2z3(&t2));
                (t2, Bool::and(self.cxt, &[&phi1, &phi2, &phi3]))
            }
            // ----------------------------------------------
            // Γ ⊢ empty α : (List(α), true)
            Exp::Empty(alpha) => (Typ::List(Box::new(alpha.clone())), self.z3_true()),
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ head e : (α, φ && List(α) = T)
            Exp::Head(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar_typ();
                let phi2 = self
                    .t2z3(&Typ::List(Box::new(alpha.clone())))
                    ._eq(&self.t2z3(&t));
                (alpha, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ tail e : (T, φ && List(α) = T)
            Exp::Tail(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar_typ();
                let phi2 = self
                    .t2z3(&Typ::List(Box::new(alpha.clone())))
                    ._eq(&self.t2z3(&t));
                (t, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ is_empty e : (bool, φ && List(α) = T)
            Exp::IsEmpty(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar_typ();
                let phi2 = self
                    .t2z3(&Typ::List(Box::new(alpha.clone())))
                    ._eq(&self.t2z3(&t));
                (Typ::Bool, Bool::and(self.cxt, &[&phi1, &phi2]))
            }
            // Γ ⊢ e : (T, φ)
            // ----------------------------------------------
            // Γ ⊢ is_GROUND e : (bool, φ)
            Exp::IsBool(e) | Exp::IsInt(e) | Exp::IsString(e) | Exp::IsList(e) | Exp::IsFun(e) => {
                let (t1, phi1) = self.cgen(env, e);
                (
                    Typ::Bool,
                    Bool::and(self.cxt, &[&phi1, &self.t2z3(&t1)._eq(self.any_z3)]),
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
            // Γ ⊢ MaybeFromAny (cα, α, e) : (α, φ && ((cα = false && α = T) ||
            //                                      (cα = true &&
            //                                       T = any &&
            //                                       α != any &&
            //                                       is_fun α => α = any -> any)))
            Exp::MaybeFromAny(calpha, alpha, e) => {
                let calpha = self.c2z3(*calpha);
                let (t, phi1) = self.cgen(env, e);
                let t = self.t2z3(&t);
                let dont_coerce_case =
                    Bool::and(self.cxt, &[&Bool::not(&calpha), &self.t2z3(&alpha)._eq(&t)]);
                let any_to_any = Typ::Arr(Box::new(Typ::Any), Box::new(Typ::Any));
                let do_coerce_case = Bool::and(
                    self.cxt,
                    &[
                        &calpha,
                        &t._eq(self.any_z3),
                        &Bool::not(&self.t2z3(&alpha)._eq(self.any_z3)),
                        &self
                            .z3_is_arr(&self.t2z3(&alpha))
                            .implies(&self.t2z3(&alpha)._eq(&self.t2z3(&any_to_any))),
                    ],
                );
                let phi2 = Bool::or(self.cxt, &[&dont_coerce_case, &do_coerce_case]);
                (alpha.clone(), Bool::and(self.cxt, &[&phi1, &phi2]))
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
            Exp::FromAny(t, e) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = self.t2z3(&t1)._eq(self.any_z3);
                (t.clone(), Bool::and(self.cxt, &[&phi1, &phi2]))
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

    fn z3_is_arr<'b>(&'b self, e: &ast::Dynamic<'b>) -> ast::Bool<'b> {
        self.typ.variants[3].tester.apply(&[&e]).as_bool().unwrap()
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
    fn is_list(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(4, model, e)
    }
    fn is_pair(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(5, model, e)
    }
    fn is_any(&self, model: &Model, e: &ast::Dynamic) -> bool {
        self.is_variant(6, model, e)
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
    fn list_typ<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Dynamic
    where
        'a: 'b,
    {
        self.typ.variants[4].accessors[0].apply(&[e])
    }
    fn pair1<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Dynamic
    where
        'a: 'b,
    {
        self.typ.variants[5].accessors[0].apply(&[e])
    }
    fn pair2<'b>(&'b self, e: &'b ast::Dynamic) -> ast::Dynamic
    where
        'a: 'b,
    {
        self.typ.variants[5].accessors[1].apply(&[e])
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
        } else if self.is_list(model, &e) {
            let t = self.list_typ(&e);
            let t = model.eval(&t).unwrap();
            let t = self.z3_to_typ(model, t);
            Typ::List(Box::new(t))
        } else if self.is_pair(model, &e) {
            let t1 = model.eval(&self.pair1(&e)).unwrap();
            let t2 = model.eval(&self.pair2(&e)).unwrap();
            let t1 = self.z3_to_typ(model, t1);
            let t2 = self.z3_to_typ(model, t2);
            Typ::Pair(Box::new(t1), Box::new(t2))
        } else if self.is_any(model, &e) {
            Typ::Any
        } else {
            panic!("missing case in z3_to_typ");
        }
    }
}

fn annotate_typ<'a>(env: &HashMap<u32, Typ>, t: &mut Typ) {
    // if type already exists, nothing to do
    match t {
        Typ::Metavar(i) => {
            *t = match env.get(i) {
                Some(t) => t.clone(),
                // there is no constraint whatsoever on what this type
                // can be. Migeed and Parsberg seem to choose Int in this
                // case, though i haven't read enough to know if they
                // explicitly mention that
                None => Typ::Int,
            }
        }
        _ => (),
    }
}

fn annotate<'a>(env: &HashMap<u32, Typ>, coercions: &HashMap<u32, bool>, exp: &mut Exp) {
    match &mut *exp {
        Exp::Lit(..) | Exp::Var(..) => {}
        Exp::Empty(t) => annotate_typ(env, t),
        Exp::Let(_, t, e1, e2) => {
            annotate_typ(env, t);
            annotate(env, coercions, e1);
            annotate(env, coercions, e2);
        }
        Exp::Fun(_, t, e) | Exp::Fix(_, t, e) => {
            annotate_typ(env, t);
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
        Exp::MaybeFromAny(coercion, t, e) => {
            annotate(env, coercions, e);
            if *coercions.get(&coercion).unwrap() {
                annotate_typ(env, t);
                *exp = Exp::FromAny(t.clone(), Box::new(e.take()));
            } else {
                *exp = e.take();
            }
        }
        Exp::ToAny(e)
        | Exp::FromAny(_, e)
        | Exp::Head(e)
        | Exp::Tail(e)
        | Exp::Not(e)
        | Exp::IsEmpty(e)
        | Exp::IsBool(e)
        | Exp::IsInt(e)
        | Exp::IsString(e)
        | Exp::IsList(e)
        | Exp::IsFun(e) => {
            annotate(env, coercions, e);
        }
        Exp::App(e1, e2)
        | Exp::Add(e1, e2)
        | Exp::AddOverload(e1, e2)
        | Exp::IntEq(e1, e2)
        | Exp::Cons(e1, e2)
        | Exp::Pair(e1, e2)
        | Exp::Mul(e1, e2) => {
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
        .variant(
            "List",
            vec![("t", z3::DatatypeAccessor::Datatype("Typ".into()))],
        )
        .variant(
            "Pair",
            vec![
                ("t1", z3::DatatypeAccessor::Datatype("Typ".into())),
                ("t2", z3::DatatypeAccessor::Datatype("Typ".into())),
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
        list_ctor: &typ.variants[4].constructor,
        pair_ctor: &typ.variants[5].constructor,
        any_z3: &typ.variants[6].constructor.apply(&[]),
        vars: Default::default(),
        coercions: Default::default(),
        typ_sort: &typ.sort,
        typ: &typ,
    };

    let solver = z3::Optimize::new(&cxt);
    let (_, phi) = s.cgen(&Default::default(), exp);
    solver.assert(&phi);
    for (_, x) in s.coercions.borrow().iter() {
        solver.assert_soft(&Bool::not(x), 1, None);
    }
    match solver.check(&[]) {
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
    use crate::tests_631::*;

    #[test]
    fn test_typeinf() {
        typeinf(&parse("(fun x . x) 10 ")).unwrap();
    }

    #[test]
    fn identity_alone() {
        println!("{:?}", typeinf(&parse("fun x . x")).unwrap())
    }

    #[test]
    fn occurs_check_fun_any() {
        // In HM, this would be an occurs-check failure
        println!("{:?}", typeinf(&parse("fun f . f f")).unwrap())
    }

    #[test]
    fn test_typeinf_add() {
        typeinf(&parse("(fun x . x +? 20) 10 ")).unwrap();
    }

    #[test]
    fn str_add() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . x +? x) "everything is ""#)).unwrap()
        );
    }

    #[test]
    fn add_str_int_any() {
        println!(
            "{:?}",
            typeinf(&parse(r#"(fun x . fun y . x +? y) "everything is " 10"#)).unwrap()
        );
    }

    #[test]
    fn infer_arr() {
        println!("{:?}", typeinf(&parse("fun f . f 200")).unwrap());
    }

    #[test]
    fn ambiguous_add() {
        println!("{:?}", typeinf(&parse("fun x . x +? x")).unwrap());
    }

    #[test]
    fn heterogenous_list() {
        println!("{:?}", typeinf(&parse("true :: 10 :: empty")).unwrap());
    }

    #[test]
    fn make_pair() {
        succeeds("(fun x . fun y . x, y) 5 true");
    }

    #[test]
    fn over_optimized() {
        no_from_any(
            "// this should be (any -> int)
             // but it gets mislabeled as (int -> int)
             let accepts_any = fun x . 5 in
             // this is used to get the optimizer to mislabel accepts_any
             accepts_any 5 + accepts_any 5 + accepts_any 5 +
             // now this was correct before our inference, but now is incorrect
             // a runtime error will be thrown as false is from_any_to_any'd, when it could
             // have stayed any just fine
             // the conditional is here to allow the to_any
             accepts_any (if true then true else false)",
        );
    }

    #[test]
    fn cond_int_bool() {
        println!(
            "{}",
            typeinf(&parse(
                "let f = fun b.fun x. if b then x + 1 else not x in
                 let y = f true 5 in
                 f false false"
            ))
            .unwrap()
        );
    }
}

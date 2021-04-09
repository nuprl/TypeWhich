use super::parser::next_metavar;
use super::syntax::*;
use super::z3_state::Z3State;
use super::Options;
use im_rc::HashMap;
use std::cell::RefCell;
use z3::ast::{Ast, Bool, Dynamic};
use z3::{Optimize, SatResult};

type Env = HashMap<String, Typ>;

struct State<'a> {
    vars: RefCell<HashMap<u32, Dynamic<'a>>>,
    z3: Z3State<'a>,
    solver: Optimize<'a>,
    options: Options,
}

impl<'a> State<'a> {
    fn t2z3(&self, typ: &Typ) -> Dynamic<'a> {
        match typ {
            Typ::Unit => self.z3.unit_z3.clone(),
            Typ::Int => self.z3.int_z3.clone(),
            Typ::Float => self.z3.float_z3.clone(),
            Typ::Bool => self.z3.bool_z3.clone(),
            Typ::Str => self.z3.str_z3.clone(),
            Typ::Char => self.z3.char_z3.clone(),
            Typ::Arr(t1, t2) => self.z3.arr_ctor.apply(&[&self.t2z3(t1), &self.t2z3(t2)]),
            Typ::List(t) => self.z3.list_ctor.apply(&[&self.t2z3(t)]),
            Typ::Pair(t1, t2) => self.z3.pair_ctor.apply(&[&self.t2z3(t1), &self.t2z3(t2)]),
            Typ::Box(t) => self.z3.box_ctor.apply(&[&self.t2z3(t)]),
            Typ::Vect(t) => self.z3.vect_ctor.apply(&[&self.t2z3(t)]),
            Typ::Any => self.z3.any_z3.clone(),
            Typ::Metavar(n) => {
                let mut vars = self.vars.borrow_mut();
                match vars.get(n) {
                    Some(ast) => ast.clone(),
                    None => {
                        let t = z3::ast::Datatype::fresh_const(
                            self.z3.cxt,
                            &typ.to_string(),
                            self.z3.typ_sort,
                        );
                        let x = Dynamic::from_ast(&t);
                        vars.insert(*n, x.clone());
                        x
                    }
                }
            }
        }
    }

    fn cgen(&self, env: &Env, exp: &mut Exp) -> (Typ, Bool<'_>) {
        match exp {
            Exp::PrimCoerce(..) => panic!("PrimCoerce should not appear in source"),
            // ---------------------------
            // Γ ⊢ lit => lit.typ(), true
            Exp::Lit(lit) => (lit.typ(), self.z3.true_z3()),
            // ---------------------------
            // Γ ⊢ x => Γ(x), true
            Exp::Var(x) => (
                env.get(x)
                    .unwrap_or_else(|| panic!("unbound identifier {}", x))
                    .clone(),
                self.z3.true_z3(),
            ),
            // Γ,x:T_1 ⊢ e => T_2, φ
            // ---------------------------------------
            // Γ ⊢ fun x : T_1 . e => T_1 -> α, φ && weaken(T_2, α)
            Exp::Fun(x, t, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t.clone());
                let (t_body, phi) = self.cgen(&env, body);
                let alpha = next_metavar();
                let phi2 = self.weaken(t_body, alpha.clone(), body);
                (Typ::Arr(Box::new(t.clone()), Box::new(alpha)), phi & phi2)
            }
            // Γ,x:T_1 ⊢ e => T_2, φ
            // ---------------------------------------
            // Γ ⊢ fix x : T_1 . e => T_1, φ && T_1 = T_2
            Exp::Fix(x, t1, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t1.clone());
                let (t2, phi1) = self.cgen(&env, body);
                let phi2 = self.t2z3(t1)._eq(&self.t2z3(&t2));
                (t1.clone(), phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 e_2 => coerce(T_1, α -> β) e_1 coerce(T_2, α) e_2, β,
            //                φ_1 && φ_2 && strengthen(T_1, α -> β) && weaken(T_2, α)
            Exp::App(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar();
                let beta = next_metavar();
                let arr = Typ::Arr(Box::new(alpha.clone()), Box::new(beta.clone()));
                let phi3 = self.strengthen(t1.clone(), arr, e1);
                // If the thing was annotated by the user, we allow strengthening
                let (phi3, phi4) = match t1 {
                    Typ::Metavar(..) => (phi3, self.weaken(t2, alpha, e2)),
                    // dynamic consistency
                    Typ::Arr(arg_box, _) => match *arg_box {
                        Typ::Metavar(..) => (phi3, self.weaken(t2, alpha, e2)),
                        arg => {
                            // rather than say strengthen(t2, arg), we say, if
                            // it's annotated, it's on the user to make sure that
                            // annotation is correct. That is, we will either infer
                            // the annotation (covered by weaken) or we will infer
                            // any, and the annotation needs to be right
                            // Generates a constraint that t1 is dynamic consistent with t2, as well
                            // as that t1 is a reasonable migration *assuming t2 is correct*. This
                            // means that if t1 weakens to t2, weak_negative_any is true, but if t1
                            // strengthens to t2, all bets are off
                            (
                                phi3,
                                self.t2z3(&t2)._eq(&self.z3.any_z3) | self.weaken(t2, arg, e2),
                            )
                        }
                    },
                    _ => {
                        eprintln!("applied non-arrow. will create failing coercion.");
                        (self.z3.true_z3(), self.weaken(t2, alpha, e2))
                    }
                };
                (beta, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ,x:T_1 ⊢ e2 => T_2, φ_2
            // ---------------------------------------
            // Γ ⊢ let x = e1 in e2 => let x = e1 in e2, T_2, φ_1 && φ_2
            Exp::Let(x, e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let mut env = env.clone();
                env.insert(x.clone(), t1);
                let (t2, phi2) = self.cgen(&env, e2);
                (t2, phi1 & phi2)
            }
            // Γ,x1:T_1,...,xn:T_n ⊢ ei => T_i, φ_i
            // Γ,x1:T_1,...,xn:T_n ⊢ e => T, φ
            // ---------------------------------------
            // Γ ⊢ letrec x1 : T_1 = e1 ... xn : T_n = en in e => letrec x1 : T_1 = e1 ... xn : T_n = en in e , T, φ_1 && ... & φ_n && φ
            Exp::LetRec(es, e) => {
                let mut env = env.clone();
                for (xi, ti, _) in es.iter() {
                    env.insert(xi.clone(), ti.clone());
                }
                let phis = es.iter_mut().fold(self.z3.true_z3(), |acc, (_, ti, ei)| {
                    let (si, phii) = self.cgen(&env, ei);
                    acc & self.t2z3(ti)._eq(&self.t2z3(&si)) & phii
                });
                let (t, phi) = self.cgen(&env, e);
                (t, phi & phis)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // -------------------
            // Γ ⊢ e1 : T => coerce(T_1, T, e), T, φ_1 && (T_1 = any || weaken(T_1, T))
            Exp::Ann(e, typ) => {
                let (t1, phi1) = self.cgen(env, e);
                // Generates a constraint that t1 is dynamic consistent with t2, as well
                // as that t1 is a reasonable migration *assuming t2 is correct*. This
                // means that if t1 weakens to t2, weak_negative_any is true, but if t1
                // strengthens to t2, all bets are off
                let phi2 = self.t2z3(&t1)._eq(&self.z3.any_z3) | self.weaken(t1, typ.clone(), e);
                (typ.clone(), phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 [+*] e_2 => coerce(T_1, int) e_1 [+*] coerce(T_2, int) e_2, int,
            //                     φ_1 && φ_2 && strengthen(T_1, int) && strengthen(T_2, int)
            Exp::Add(e1, e2) | Exp::Mul(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let phi3 = self.strengthen(t1, Typ::Int, &mut *e1)
                    & self.strengthen(t2, Typ::Int, &mut *e2);
                (Typ::Int, phi1 & phi2 & phi3)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 = e_2 => coerce(T_1, int) e_1 = coerce(T_2, int) e_2, bool,
            //                  φ_1 && φ_2 && strengthen(T_1, int) && strengthen(T_2, int)
            Exp::IntEq(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let s1 = self.strengthen(t1, Typ::Int, e1);
                let s2 = self.strengthen(t2, Typ::Int, e2);
                (Typ::Bool, phi1 & phi2 & s1 & s2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ not e => coerce(T, bool) e, bool, φ && weaken(T, bool)
            Exp::Not(e) => {
                let (t, phi1) = self.cgen(&env, e);
                let phi2 = self.strengthen(t, Typ::Bool, e);
                (Typ::Bool, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 +? e_2 => coerce(T_1, α) e_1 +? coerce(T_2, α) e_2, α,
            //                   φ_1 && φ_2 && (α = int ||
            //                                  α = str ||
            //                                  α = any) &&
            //                                  weaken(T_1, α) && weaken(T_2, α)
            Exp::AddOverload(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar();
                let a_z3 = self.t2z3(&alpha);
                let weakens =
                    self.weaken(t1, alpha.clone(), e1) & self.weaken(t2, alpha.clone(), e2);
                let valid_type = a_z3._eq(&self.z3.int_z3)
                    | a_z3._eq(&self.z3.str_z3)
                    | a_z3._eq(&self.z3.any_z3);
                (alpha, phi1 & phi2 & valid_type & weakens)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // Γ ⊢ e_3 => T_3, φ_3
            // ----------------------------------------------
            // Γ ⊢ if e_1 then e_2 else e_3 =>
            //         if coerce(T_1, bool) e_1 then coerce(T_2, α) else coerce(T_3, α), α,
            //                                 φ_1 && φ_2 && φ_3 && strengthen(T_1, bool) &&
            //                                 weaken(T_2, α) && weaken(T_3, α)
            Exp::If(e1, e2, e3) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let (t3, phi3) = self.cgen(&env, e3);
                let alpha = next_metavar();
                let phi4 = self.strengthen(t1, Typ::Bool, e1)
                    & self.weaken(t2, alpha.clone(), e2)
                    & self.weaken(t3, alpha.clone(), e3);
                (alpha, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1, e_2 => e_1, e_2, (T_1, T_2), φ_1 && φ_2
            Exp::Pair(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                (Typ::Pair(Box::new(t1), Box::new(t2)), phi1 & phi2)
            }
            // Γ ⊢ e => e, T_1, φ_1
            // ----------------------------------------------
            // Γ ⊢ fst e => fst coerce(T_1, Pair(α,β), e), α, φ_1 && strengthen(T_1, Pair(α,β))
            Exp::Fst(e) => {
                let (t1, phi1) = self.cgen(&env, e);
                let alpha = next_metavar();
                let beta = next_metavar();
                let phi2 =
                    self.strengthen(t1, Typ::Pair(Box::new(alpha.clone()), Box::new(beta)), e);
                (alpha, phi1 & phi2)
            }
            // Γ ⊢ e => e, T_1, φ_1
            // ----------------------------------------------
            // Γ ⊢ snd e => snd coerce(T_1, Pair(α,β), e), β, φ_1 && strengthen(T_1, Pair(α,β))
            Exp::Snd(e) => {
                let (t1, phi1) = self.cgen(&env, e);
                let alpha = next_metavar();
                let beta = next_metavar();
                let phi2 =
                    self.strengthen(t1, Typ::Pair(Box::new(alpha), Box::new(beta.clone())), e);
                (beta, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 :: e_2 => coerce(T_1, α) e_1 :: coerce(T_2, List(α)) e_2, List(α),
            //                   φ_1 && φ_2 && weaken(T_1, α) && strengthen(T_2, List(α))
            Exp::Cons(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let item_typ = next_metavar();
                let phi3 = self.strengthen(t2.clone(), Typ::List(Box::new(item_typ.clone())), e2)
                    & self.weaken(t1, item_typ, e1);
                (t2, phi1 & phi2 & phi3)
            }
            // ----------------------------------------------
            // Γ ⊢ empty α => List(α), true
            Exp::Empty(alpha) => (Typ::List(Box::new(alpha.clone())), self.z3.true_z3()),
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ head e => head coerce(T, List(α)) e, α,
            //               φ && strengthen(T, List(α))
            Exp::Head(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::List(Box::new(alpha.clone())), e);
                (alpha, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ tail e => tail coerce(T, List(α)) e, List(α),
            //               φ && strengthen(T, List(α))
            Exp::Tail(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let list_alpha = Typ::List(Box::new(alpha));
                let phi2 = self.strengthen(t, list_alpha.clone(), e);
                (list_alpha, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ is_empty e => is_empty coerce(T, List(α)) e, bool,
            //                   φ && strengthen(T, List(α))
            Exp::IsEmpty(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let list_alpha = Typ::List(Box::new(alpha));
                let phi2 = self.strengthen(t, list_alpha, e);
                (Typ::Bool, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ box e => box coerce(T, α) e, Box(α), φ && weaken(T, α)
            Exp::Box(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.weaken(t, alpha.clone(), e);
                (Typ::Box(Box::new(alpha)), phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ unbox e => coerce(T, Box(α)) e, α, φ && strengthen(T, Box(α))
            Exp::Unbox(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::Box(Box::new(alpha.clone())), e);
                (alpha, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ boxset! e_1 e_2 => boxset! coerce(T_1, Box(α)) e_1 coerce(T_2, α) e_2, Unit,
            //                        strengthen(T_1, Box(α)) && weaken(T_2, α)
            Exp::BoxSet(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let alpha = next_metavar();
                let phi3 = self.strengthen(t1, Typ::Box(Box::new(alpha.clone())), e1);
                let phi4 = self.weaken(t2, alpha.clone(), e2);
                (Typ::Unit, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ vector e1 e2 => vector (coerce (T_1, int) e1) (coerce(T_2, α) e), Vect(α),
            //                          φ_1 && φ_2 && && strengthen(T_1, int) && weaken(T, α)
            Exp::Vector(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let phi3 = self.strengthen(t1, Typ::Int, e1);
                let alpha = next_metavar();
                let phi4 = self.weaken(t2, alpha.clone(), e2);
                (Typ::Vect(Box::new(alpha)), phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ vector-ref e1 e2 => vector-ref (coerce(T_1, Vect(α)) e1) (coerce(T_2, Int) e2), α,
            //                              φ && strengthen(T_1, Vect(α)) && strengthen(T_2, Int)
            Exp::VectorRef(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let alpha = next_metavar();
                let phi3 = self.strengthen(t1, Typ::Vect(Box::new(alpha.clone())), e1);
                let phi4 = self.strengthen(t2, Typ::Int, e2);
                (alpha, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // Γ ⊢ e3 => T_3, φ_3
            // ----------------------------------------------
            // Γ ⊢ vector-set! e1 e2 e3 => vector-set! coerce(T_1, Vect(α)) e_1 coerce(T_2, Int) e_2 coerce(T_3, α) e_3, Unit,
            //                        strengthen(T_1, Box(α)) && weaken(T_2, α)
            Exp::VectorSet(e1, e2, e3) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let (t3, phi3) = self.cgen(env, e3);
                let alpha = next_metavar();
                let phi4 = self.strengthen(t1, Typ::Vect(Box::new(alpha.clone())), e1);
                let phi5 = self.strengthen(t2, Typ::Int, e2);
                let phi6 = self.weaken(t3, alpha.clone(), e3);
                (Typ::Unit, phi1 & phi2 & phi3 & phi4 & phi5 & phi6)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ vector-length e => vector-length coerce(e, Vect(α)), int, φ && strengthen(T, Vect(α))
            Exp::VectorLen(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::Vect(Box::new(alpha)), e);
                (Typ::Int, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ is_GROUND e => coerce(e, any), bool, φ && weaken(T, any)
            Exp::IsBool(e) | Exp::IsInt(e) | Exp::IsString(e) | Exp::IsList(e) | Exp::IsFun(e) => {
                let (t, phi1) = self.cgen(env, e);
                let phi2 = self.weaken(t, Typ::Any, e);
                (Typ::Bool, phi1 & phi2)
            }
            // Γ ⊢ e => T_3, φ
            // ----------------------------------------------
            // Γ ⊢ coerce(T_1, T_2) e => coerce(T_1, T_2) e, T_2, φ && T_1 = T_3
            Exp::Coerce(t1, t2, e) => {
                let (t3, phi) = self.cgen(env, e);
                if self.options.optimizer {
                    self.solver
                        .assert_soft(&self.t2z3(&t1)._eq(&self.t2z3(&t2)), 1, None);
                }
                (t2.clone(), phi & self.t2z3(&t1)._eq(&self.t2z3(&t3)))
            }
        }
    }

    fn solve_model(&self, model: z3::Model) -> HashMap<u32, Typ> {
        let mut result = HashMap::new();
        for (x, x_ast) in self.vars.borrow().iter() {
            let x_val_ast = model.eval(x_ast).expect("evaluating metavar");
            result.insert(*x, self.z3.z3_to_typ(&model, x_val_ast));
        }
        result
    }

    /// Provide a typ for the entire program. Returns a constraint
    /// that ensures that every type in a negative position is any
    ///
    /// DO NOT evaluate (model.eval) t before passing in. model.eval
    /// recursively evaluates. we only want to get the kind and its
    /// metavariables
    fn negative_any(&self, model: &z3::Model<'a>, t: &Dynamic<'a>) -> Bool<'a> {
        if self.z3.is_int(model, &t)
            || self.z3.is_unit(model, &t)
            || self.z3.is_float(model, &t)
            || self.z3.is_bool(model, &t)
            || self.z3.is_str(model, &t)
            || self.z3.is_char(model, &t)
            || self.z3.is_any(model, &t)
        {
            self.z3.true_z3()
        } else if self.z3.is_arr(model, &t) {
            let arg = self.z3.arr_arg(&t);
            let ret = self.z3.arr_ret(&t);
            arg._eq(&self.z3.any_z3) & self.negative_any(model, &ret)
        } else if self.z3.is_list(model, &t) {
            let t = self.z3.list_typ(&t);
            self.negative_any(model, &t)
        } else if self.z3.is_pair(model, &t) {
            let t1 = self.z3.pair1(&t);
            let t2 = self.z3.pair2(&t);
            self.negative_any(model, &t1) & self.negative_any(model, &t2)
        } else if self.z3.is_box(model, &t) {
            // A box is a negative position, no matter what is_neg says. For
            // example, p = box 5 may be put in a context that says `boxset! p
            // true`. so p must have type box any
            let t = self.z3.box_typ(&t);
            t._eq(&self.z3.any_z3)
        } else if self.z3.is_vect(model, &t) {
            let t = self.z3.vect_typ(&t);
            t._eq(&self.z3.any_z3)
        } else {
            panic!("missing case in negative_any {:?}", t);
        }
    }

    fn coerce(&self, t1: Typ, t2: Typ, exp: &mut Exp) {
        if self.options.optimizer {
            self.solver
                .assert_soft(&self.t2z3(&t1)._eq(&self.t2z3(&t2)), 1, None);
        }
        *exp = Exp::Coerce(t1, t2, Box::new(exp.take()));
    }

    /// Modifies `exp` in place to coerce from t1 to t2. Generates a
    /// constraint that T_1 must be any and T_2 must be negative-any, or they are
    /// already equal. Caller's responsibility to ensure typ(exp) = t1
    ///
    /// In other words, the constraint is that t1 and t2 are dynamically
    /// consistent, the type doesn't weaken, and the coercion is reasonable.
    ///
    /// Because this can cause dynamic errors, **this should only be used
    /// at elimination forms** in order to be safe!
    ///
    /// T_1 = T_2 || (T_1 = any && weak_negative_any(t2))
    #[must_use]
    fn strengthen(&self, t1: Typ, t2: Typ, exp: &mut Exp) -> Bool<'_> {
        let coerce_case = self.t2z3(&t1)._eq(&self.z3.any_z3) & self.weak_negative_any(&t2);
        // we don't care about putting an ID coercion, that's fine
        let dont_coerce_case = self.t2z3(&t1)._eq(&self.t2z3(&t2));
        self.coerce(t1, t2, exp);
        coerce_case | dont_coerce_case
    }

    /// Modifies `exp` in place to corce from t1 to t2. Generates a constraint
    /// that they are already equal, or t2 is any and t1 is
    /// negative-any. Caller's responsibility to ensure typ(exp) = t1
    ///
    /// In other words, the constraint is that t1 and t2 are dynamically
    /// consistent, the type doesn't strengthen, and the coercion does not lose
    /// track of important type information.
    ///
    /// This is always safe
    ///
    /// T_1 = T_2 || (T_2 = any && weak_negative_any(T_1))
    #[must_use]
    fn weaken(&self, t1: Typ, t2: Typ, exp: &mut Exp) -> Bool<'_> {
        let coerce_case = self.t2z3(&t2)._eq(&self.z3.any_z3) & self.weak_negative_any(&t1);
        let dont_coerce_case = self.t2z3(&t1)._eq(&self.t2z3(&t2));
        self.coerce(t1, t2, exp);
        coerce_case | dont_coerce_case
    }

    /// Provided a type, generate constraints that the type has any in all of
    /// its negative forms. The function is more weak / general than it could be
    /// due to the difficulties with z3.
    ///
    /// For example, if t has type * -> int, that type is safe to
    /// coerce to any (with wrapping). However, because z3 cannot produce
    /// recursive constraints, and the type * -> (int -> int) is forbidden,
    /// weak_negative_any is forced to produce the constraint that t has type *
    /// -> *.
    ///
    /// Note that anything that can be mutated is negative.
    ///
    /// One might think that lists are a special case: because lists are
    /// immutable they have no negative positions. However, imagine a function that
    /// is stored in a list. It is inferred to be int -> int, however after being
    /// pulled out of the list it is called with a bool. This is incorrect. We
    /// might say, lists simple must hold weak_negative_any types, rather than
    /// any! And you would be right, but notice that we have now produced a
    /// recursive constraint which z3 does not support.
    ///
    /// weak_negative_any t = is_arr(t) => t = any -> any
    ///                    && is_list(t) => t = list any
    ///                    && is_box(t) => t = box any
    ///                    && is_vect(t) => t = vect any
    fn weak_negative_any(&self, t: &Typ) -> Bool<'_> {
        let any_to_any = Typ::Arr(Box::new(Typ::Any), Box::new(Typ::Any));
        self.z3
            .z3_is_arr(self.t2z3(t))
            .implies(&self.t2z3(t)._eq(&self.t2z3(&any_to_any)))
            & self
                .z3
                .z3_is_list(self.t2z3(t))
                .implies(&self.t2z3(t)._eq(&self.t2z3(&Typ::List(Box::new(Typ::Any)))))
            & self
                .z3
                .z3_is_box(self.t2z3(t))
                .implies(&self.t2z3(t)._eq(&self.t2z3(&Typ::Box(Box::new(Typ::Any)))))
            & self
                .z3
                .z3_is_vect(self.t2z3(t))
                .implies(&self.t2z3(t)._eq(&self.t2z3(&Typ::Vect(Box::new(Typ::Any)))))
    }
}

fn annotate_typ(env: &HashMap<u32, Typ>, t: &mut Typ) {
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
        Typ::Arr(t1, t2) | Typ::Pair(t1, t2) => {
            annotate_typ(env, t1);
            annotate_typ(env, t2);
        }
        Typ::List(t) | Typ::Box(t) | Typ::Vect(t) => {
            annotate_typ(env, t);
        }
        Typ::Unit | Typ::Int | Typ::Float | Typ::Bool | Typ::Str | Typ::Char | Typ::Any => (),
    }
}

fn annotate(env: &HashMap<u32, Typ>, exp: &mut Exp) {
    match &mut *exp {
        Exp::PrimCoerce(..) => panic!("PrimCoerce should not appear in source"),
        Exp::Lit(..) | Exp::Var(..) => {}
        Exp::Empty(t) => annotate_typ(env, t),
        Exp::Fun(_, t, e) | Exp::Fix(_, t, e) | Exp::Ann(e, t) => {
            annotate_typ(env, t);
            annotate(env, e);
        }
        Exp::Coerce(t1, t2, e) => {
            annotate(env, e);
            annotate_typ(env, t1);
            annotate_typ(env, t2);
            if t1 == t2 {
                *exp = e.take();
            }
        }
        Exp::Head(e)
        | Exp::Tail(e)
        | Exp::Not(e)
        | Exp::Box(e)
        | Exp::Unbox(e)
        | Exp::Fst(e)
        | Exp::Snd(e)
        | Exp::IsEmpty(e)
        | Exp::IsBool(e)
        | Exp::IsInt(e)
        | Exp::IsString(e)
        | Exp::IsList(e)
        | Exp::IsFun(e) 
        | Exp::VectorLen(e) => {
            annotate(env, e);
        }
        Exp::App(e1, e2)
        | Exp::Add(e1, e2)
        | Exp::AddOverload(e1, e2)
        | Exp::IntEq(e1, e2)
        | Exp::Cons(e1, e2)
        | Exp::Pair(e1, e2)
        | Exp::Mul(e1, e2)
        | Exp::BoxSet(e1, e2)
        | Exp::Let(_, e1, e2)
        | Exp::Vector(e1, e2)
        | Exp::VectorRef(e1, e2) => {
            annotate(env, e1);
            annotate(env, e2);
        }
        Exp::If(e1, e2, e3) | Exp::VectorSet(e1, e2, e3) => {
            annotate(env, e1);
            annotate(env, e2);
            annotate(env, e3);
        }
        Exp::LetRec(bindings, e) => {
            for (_, typ, ei) in bindings {
                annotate_typ(env, typ);
                annotate(env, ei);
            }
            annotate(env, e);
        }
    }
}

#[cfg(test)]
pub fn typeinf(exp: Exp) -> Result<Exp, String> {
    typeinf_options(exp, &Default::default(), Options::default())
}
pub fn typeinf_options(mut exp: Exp, env: &Env, options: Options) -> Result<Exp, String> {
    let cfg = z3::Config::new();
    let cxt = z3::Context::new(&cfg);
    let typ = Z3State::typ(&cxt);
    let s = State {
        z3: Z3State::new(&cxt, &typ),
        vars: Default::default(),
        solver: Optimize::new(&cxt),
        options,
    };
    let (t, phi) = s.cgen(env, &mut exp);
    s.solver.assert(&phi);
    if s.options.context {
        s.solver.push();
        if options.debug {
            eprintln!("Solver state for precise type:");
            eprintln!("{}", s.solver);
        }
        match s.solver.check(&[]) {
            SatResult::Unsat => return Err("unsat (precise type)".to_string()),
            SatResult::Unknown => return Err("unknown from Z3 -- very bad".to_string()),
            SatResult::Sat => (),
        }
        let model = s.solver.get_model().expect("model not available");
        s.solver.pop();
        let negative_any = s.negative_any(&model, &s.t2z3(&t));
        s.solver.assert(&negative_any);
    }
    if options.debug {
        eprintln!("Solver state for final type:");
        eprintln!("{}", s.solver);
    }
    match s.solver.check(&[]) {
        SatResult::Unsat => return Err("unsat".to_string()),
        SatResult::Unknown => panic!("unknown from Z3 -- very bad"),
        SatResult::Sat => (),
    }
    let model = s.solver.get_model().expect("model not available");
    let result = s.solve_model(model);
    annotate(&result, &mut exp);
    Ok(exp)
}

#[cfg(test)]
mod test {
    use super::super::parser::parse;
    use super::typeinf;
    use crate::syntax::Typ;
    use crate::tests_631::*;

    #[test]
    fn test_typeinf() {
        typeinf(parse("(fun x . x) 10 ").unwrap()).unwrap();
    }

    #[test]
    fn identity_alone() {
        println!("{:?}", typeinf(parse("fun x . x").unwrap()).unwrap())
    }

    #[test]
    fn occurs_check_fun_any() {
        // In HM, this would be an occurs-check failure
        println!("{:?}", typeinf(parse("fun f . f f").unwrap()).unwrap())
    }

    #[test]
    fn test_typeinf_add() {
        typeinf(parse("(fun x . x +? 20) 10 ").unwrap()).unwrap();
    }

    #[test]
    fn str_add() {
        println!(
            "{:?}",
            typeinf(parse(r#"(fun x . x +? x) "everything is ""#).unwrap()).unwrap()
        );
    }

    #[test]
    fn add_str_int_any() {
        println!(
            "{:?}",
            typeinf(parse(r#"(fun x . fun y . x +? y) "everything is " 10"#).unwrap()).unwrap()
        );
    }

    #[test]
    fn infer_arr() {
        println!("{:?}", typeinf(parse("fun f . f 200").unwrap()));
    }

    #[test]
    fn ambiguous_add() {
        println!("{:?}", typeinf(parse("fun x . x +? x").unwrap()).unwrap());
    }

    #[test]
    fn heterogenous_list() {
        println!("{:?}", typeinf(parse("true :: 10 :: empty").unwrap()));
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
            typeinf(parse(
                "let f = fun b.fun x. if b then x + 1 else not x in
                 let y = f true 5 in
                 f false false"
            ).unwrap())
            .unwrap()
        );
    }

    #[test]
    fn strengthen_not() {
        coerces(
            "let accepts_any = fun x . not x in
            let _ = accepts_any true in
            accepts_any 5",
        );
    }

    #[test]
    fn force_any_then_cons() {
        coerces(
            "let force_any = fun x . 5 :: x in
            let _ = force_any true in
            force_any (10 :: empty)",
        );
    }

    // let's have some context fun
    #[test]
    fn app() {
        coerces("fun f. fun x. f x");
    }

    #[test]
    fn map_public() {
        coerces(
            "fix map . fun f . fun lst .
               if is_empty(lst) then
                 empty
               else
                 f(head(lst)) :: (map f (tail(lst)))",
        );
    }

    #[test]
    fn gives_list_fs() {
        succeeds("(fun x.x) :: empty");
    }

    #[test]
    fn annotate_exact() {
        succeeds("5 : int");
        // coerces("5 : bool"); // actually fails :|
    }

    #[test]
    fn arr_in_lists() {
        assert_eq!(
            coerces(
                "let id = fun x.x in // any -> any, but if not careful, int -> int
                let call_head_bool = fun x. (head x) true in
                let my_arr = id :: empty in
                let tmp1 = call_head_bool my_arr in
                id 5
                "
            ),
            Typ::Any
        )
    }

    #[test]
    fn arjun_arr_in_any() {
        assert_eq!(
            coerces(
                "let id = fun x . x in
                let tmp0 = id 5 in
                let id2 = id id in
                let id3 = fun n.n in // if this is int -> int things are bad
                let tmp1 = id3 5 in
                let id4 = id2 id3 in
                let tmp2 = id4 true in
                id3 5"
            ),
            Typ::Any
        )
    }

    #[test]
    fn introduction_arr_in_any() {
        assert_eq!(
            coerces(
                "let to_int = fun x.x*5 in
                let id = fun x.x in
                let tmp0 = id true in
                let tmp1 = id to_int in
                to_int 10"
            ),
            Typ::Any
        )
    }
}

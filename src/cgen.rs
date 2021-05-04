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

    fn cgen(&self, env: &Env, exp: &mut Exp) -> (Typ, Bool<'a>) {
        match exp {
            Exp::PrimCoerce(..) => panic!("PrimCoerce should not appear in source"),
            // ---------------------------
            // Γ ⊢ lit => coerce(lit.typ(), α, lit), α, weaken(lit.typ(), α)
            Exp::Lit(lit) => self.weaken(lit.typ(), exp, self.z3.true_z3()),
            // ---------------------------
            // Γ ⊢ x => x, Γ(x), true
            Exp::Var(x) => {
                let typ = env
                    .get(x)
                    .unwrap_or_else(|| panic!("unbound identifier {}", x))
                    .clone();

                if self.options.rigid_vars {
                    (typ, self.z3.true_z3())
                } else {
                    self.weaken(typ, exp, self.z3.true_z3())
                }
            }
            // Γ,x:T_1 ⊢ e => T_2, φ
            // ---------------------------------------
            // Γ ⊢ fun x : T_1 . e => coerce(T1 -> T2, α) fun x : T_1 . e, α,
            //                        φ && weaken(T1 -> T2, α)
            Exp::Fun(x, t1, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t1.clone());
                let (t2, phi) = self.cgen(&env, body);
                let arrow = Typ::Arr(Box::new(t1.clone()), Box::new(t2));
                self.weaken(arrow, exp, phi)
            }
            // Γ,x:T_1 ⊢ e => T_2, φ
            // ---------------------------------------
            // Γ ⊢ fix x : T_1 . e => coerce(T_1, α) fix x : T_1 . e, α,
            //                        φ && T_1 = T_2 && weaken(T_1, α)
            Exp::Fix(x, t1, body) => {
                let mut env = env.clone();
                env.insert(x.clone(), t1.clone());
                let (t2, phi1) = self.cgen(&env, body);
                let phi2 = self.t2z3(t1)._eq(&self.t2z3(&t2));
                self.weaken(t1.clone(), exp, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 e_2 => coerce(β, γ) (coerce(T_1, α -> β) e_1 e_2), γ,
            //                φ_1 && φ_2 && strengthen(T_1, α -> β) && weaken(β, γ)
            //                && T_2 = α
            Exp::App(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let alpha = next_metavar();
                let beta = next_metavar();
                let arr = Typ::Arr(Box::new(alpha.clone()), Box::new(beta.clone()));
                let phi3 = self.strengthen(t1.clone(), arr, e1);
                let phi4 = self.t2z3(&t2)._eq(&self.t2z3(&alpha));
                self.weaken(beta, exp, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ uop e => coerce(uop.res, α) coerce(T, uop.t, e), α, φ
            //              && strengthen(T, uop.t) && weaken(uop.res, α)
            Exp::UnaryOp(op, e) => {
                let (op_t, res) = op.typ();
                let (t, phi1) = self.cgen(&env, e);
                let phi2 = self.strengthen(t, op_t, e);
                self.weaken(res, exp, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 bop e_2 => coerce(bop.res, α) coerce(T_1, bop.t1) e_1 [+*] coerce(T_2, bop.t2) e_2, α,
            //                     φ_1 && φ_2 && strengthen(T_1, bop.t1) && strengthen(T_2, bop.t2)
            //                     && weaken(bop.res, α)
            Exp::BinaryOp(op, e1, e2) => {
                let (op1, op2, res) = op.typ();
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let phi3 = self.strengthen(t1, op1, &mut *e1) & self.strengthen(t2, op2, &mut *e2);
                self.weaken(res, exp, phi1 & phi2 & phi3)
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
            // Γ ⊢ e1 : T => coerce(T_1, T) e, T, φ_1 && ground(T_1) && ground(T)
            Exp::Ann(e, typ) => {
                let (t1, phi1) = self.cgen(env, e);
                let phi2 = self.ground(&t1) & self.ground(&typ);
                (typ.clone(), phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 +? e_2 => coerce(α, β) e_1 +? e_2, β,
            //                   φ_1 && φ_2 && T_1 = T_2 && (T_1 = int ||
            //                                               T_1 = str ||
            //                                               T_1 = any)
            //                   && weaken(α, β)
            Exp::AddOverload(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let t1_z3 = self.t2z3(&t1);
                let eq = t1_z3._eq(&self.t2z3(&t2));
                let valid_type = t1_z3._eq(&self.z3.int_z3)
                    | t1_z3._eq(&self.z3.str_z3)
                    | t1_z3._eq(&self.z3.any_z3);
                self.weaken(t1, exp, phi1 & phi2 & eq & valid_type)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // Γ ⊢ e_3 => T_3, φ_3
            // ----------------------------------------------
            // Γ ⊢ if e_1 then e_2 else e_3 => if coerce(T_1, bool, e_1) then e_2 else e_3, T_2,
            //                                 φ_1 && φ_2 && φ_3 &&
            //                                 strengthen(T_1, bool) && T_2 = T_3
            Exp::If(e1, e2, e3) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let (t3, phi3) = self.cgen(&env, e3);
                let phi4 = self.strengthen(t1, Typ::Bool, e1) & self.t2z3(&t2)._eq(&self.t2z3(&t3));
                (t2, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1, e_2 => coerce((T_1, T_2), α, (e_1, e_2)), α,
            //                 φ_1 && φ_2 && weaken((T_1, T_2), α)
            Exp::Pair(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                self.weaken(Typ::Pair(Box::new(t1), Box::new(t2)), exp, phi1 & phi2)
            }
            // Γ ⊢ e => e, T_1, φ_1
            // ----------------------------------------------
            // Γ ⊢ fst e => coerce(α, γ) fst coerce(T_1, Pair(α,β), e), γ,
            //              φ_1 && strengthen(T_1, Pair(α,β)) && weaken(α, γ)
            Exp::Fst(e) => {
                let (t1, phi1) = self.cgen(&env, e);
                let alpha = next_metavar();
                let beta = next_metavar();
                let phi2 =
                    self.strengthen(t1, Typ::Pair(Box::new(alpha.clone()), Box::new(beta)), e);
                self.weaken(alpha, exp, phi1 & phi2)
            }
            // Γ ⊢ e => e, T_1, φ_1
            // ----------------------------------------------
            // Γ ⊢ snd e => coerce(β, γ) snd coerce(T_1, Pair(α,β), e), γ,
            //              φ_1 && strengthen(T_1, Pair(α,β)) && weaken(β, γ)
            Exp::Snd(e) => {
                let (t1, phi1) = self.cgen(&env, e);
                let alpha = next_metavar();
                let beta = next_metavar();
                let phi2 =
                    self.strengthen(t1, Typ::Pair(Box::new(alpha), Box::new(beta.clone())), e);
                self.weaken(beta, exp, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ e_1 :: e_2 => coerce(List(T_1), α, e_1 :: coerce(T_2, List(T_1), e_2)), α,
            //                   φ_1 && φ_2 && strengthen(T_2, List(T_1)) && weaken(List(T_1), α)
            Exp::Cons(e1, e2) => {
                let (t1, phi1) = self.cgen(&env, e1);
                let (t2, phi2) = self.cgen(&env, e2);
                let list_typ = Typ::List(Box::new(t1.clone()));
                let phi3 = self.strengthen(t2.clone(), list_typ.clone(), e2);
                self.weaken(list_typ, exp, phi1 & phi2 & phi3)
            }
            // ----------------------------------------------
            // Γ ⊢ empty α => coerce(List(α), β, empty α), β, weaken(List(α), β)
            Exp::Empty(alpha) => {
                self.weaken(Typ::List(Box::new(alpha.clone())), exp, self.z3.true_z3())
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ head e => coerce(α, β) head coerce(T, List(α), e), β,
            //               φ && strengthen(T, List(α)) && weaken(α, β)
            Exp::Head(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::List(Box::new(alpha.clone())), e);
                self.weaken(alpha, exp, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ tail e => coerce(List(α), β, tail coerce(T, List(α), e)), β,
            //               φ && strengthen(T, List(α)) && weaken(List(α), β)
            Exp::Tail(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let list_alpha = Typ::List(Box::new(alpha));
                let phi2 = self.strengthen(t, list_alpha.clone(), e);
                self.weaken(list_alpha, exp, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ is_empty e => coerce(bool, β) is_empty coerce(T, List(α), e), β,
            //                   φ && strengthen(T, List(α)) && weaken(bool, β)
            Exp::IsEmpty(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let list_alpha = Typ::List(Box::new(alpha));
                let phi2 = self.strengthen(t, list_alpha, e);
                self.weaken(Typ::Bool, exp, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ box e => coerce(Box(α), β) box e, β, φ && weaken(Box(α), β)
            Exp::Box(e) => {
                let (t, phi1) = self.cgen(env, e);
                self.weaken(Typ::Box(Box::new(t)), exp, phi1)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ unbox e => coerce(α, β, coerce(T, Box(α))) e, β, φ
            //                && strengthen(T, Box(α)) && weaken(α, β)
            Exp::Unbox(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::Box(Box::new(alpha.clone())), e);
                self.weaken(alpha, exp, phi1 & phi2)
            }
            // Γ ⊢ e_1 => T_1, φ_1
            // Γ ⊢ e_2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ boxset! e_1 e_2 => coerce(Unit, α) boxset! coerce(T_1, Box(T_2)) e_1 e_2, α,
            //                        strengthen(T_1, Box(T_2)) && weaken(Unit, α)
            Exp::BoxSet(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let phi3 = self.strengthen(t1, Typ::Box(Box::new(t2)), e1);
                self.weaken(Typ::Unit, exp, phi1 & phi2 & phi3)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ vector e1 e2 => coerce(Vect(T_2), α) vector (coerce(T_1, int) e1) e, α,
            //                          φ_1 && φ_2 && && strengthen(T_1, int) && weaken(vect(T_2), α)
            Exp::Vector(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let phi3 = self.strengthen(t1, Typ::Int, e1);
                self.weaken(Typ::Vect(Box::new(t2)), exp, phi1 & phi2 & phi3)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // ----------------------------------------------
            // Γ ⊢ vector-ref e1 e2 =>
            //     coerce(α, β) vector-ref (coerce(T_1, Vect(α)) e1) (coerce(T_2, Int) e2), β,
            //                         φ && strengthen(T_1, Vect(α)) && strengthen(T_2, Int)
            //                         && weaken(α, β)
            Exp::VectorRef(e1, e2) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let alpha = next_metavar();
                let phi3 = self.strengthen(t1, Typ::Vect(Box::new(alpha.clone())), e1);
                let phi4 = self.strengthen(t2, Typ::Int, e2);
                self.weaken(alpha, exp, phi1 & phi2 & phi3 & phi4)
            }
            // Γ ⊢ e1 => T_1, φ_1
            // Γ ⊢ e2 => T_2, φ_2
            // Γ ⊢ e3 => T_3, φ_3
            // ----------------------------------------------
            // Γ ⊢ vector-set! e1 e2 e3 =>
            //     coerce(Unit, α) vector-set! coerce(T_1, Vect(T_3)) e_1 coerce(T_2, Int) e_2 e_3, α,
            //                             strengthen(T_1, Vect(T_3)) && weaken(Unit, α)
            Exp::VectorSet(e1, e2, e3) => {
                let (t1, phi1) = self.cgen(env, e1);
                let (t2, phi2) = self.cgen(env, e2);
                let (t3, phi3) = self.cgen(env, e3);
                let phi4 = self.strengthen(t1, Typ::Vect(Box::new(t3)), e1);
                let phi5 = self.strengthen(t2, Typ::Int, e2);
                self.weaken(Typ::Unit, exp, phi1 & phi2 & phi3 & phi4 & phi5)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ vector-length e => coerce(int, β) vector-length coerce(e, Vect(α)), β,
            //                        φ && strengthen(T, Vect(α)) && weaken(int, β)
            Exp::VectorLen(e) => {
                let (t, phi1) = self.cgen(env, e);
                let alpha = next_metavar();
                let phi2 = self.strengthen(t, Typ::Vect(Box::new(alpha)), e);
                self.weaken(Typ::Int, exp, phi1 & phi2)
            }
            // Γ ⊢ e => T, φ
            // ----------------------------------------------
            // Γ ⊢ is_GROUND e => coerce(bool, α) is_GROUND e, α, φ && T = any && weaken(bool, α)
            Exp::IsBool(e) | Exp::IsInt(e) | Exp::IsString(e) | Exp::IsList(e) | Exp::IsFun(e) => {
                let (t, phi1) = self.cgen(env, e);
                let phi2 = self.t2z3(&t)._eq(&self.z3.any_z3);
                self.weaken(Typ::Bool, exp, phi1 & phi2)
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
    /// T_1 = T_2 || (T_1 = any && ground(t2))
    #[must_use]
    fn strengthen(&self, t1: Typ, t2: Typ, exp: &mut Exp) -> Bool<'a> {
        let coerce_case = self.t2z3(&t1)._eq(&self.z3.any_z3) & self.ground(&t2);
        // we don't care about putting an ID coercion, that's fine
        let dont_coerce_case = self.t2z3(&t1)._eq(&self.t2z3(&t2));
        self.coerce(t1, t2, exp);
        coerce_case | dont_coerce_case
    }

    /// (α, weaken'(t1, α, exp) & phi1) where weaken'(t1, t2, exp) =
    ///
    /// Modifies `exp` in place to corce from t1 to t2. Generates a constraint
    /// that they are already equal, or t2 is any and t1 is
    /// negative-any. Caller's responsibility to ensure typ(exp) = t1
    ///
    /// In other words, the constraint is that t1 and t2 are dynamically
    /// consistent, the type doesn't strengthen, and the coercion does not lose
    /// track of important type information.
    ///
    /// This is always safe, so it happens on all expressions.
    ///
    /// The peculiarities of this signature are because weaken should occur
    /// on every expression that may have a different type than any of its
    /// sub-expressions and NEVER otherwise. Therefore it is easy to call
    /// self.weaken(true_typ, whole_exp, other_constraints) at the end of a match
    /// arm in cgen
    ///
    /// ----------------------------------------------
    /// Γ ⊢ e: T => coerce(T, α, e), α, φ
    ///             && T = α || (α = any && ground(T))      |> weaken'
    fn weaken(&self, t1: Typ, exp: &mut Exp, phi1: Bool<'a>) -> (Typ, Bool<'a>) {
        let alpha = next_metavar();
        let coerce_case = self.t2z3(&alpha)._eq(&self.z3.any_z3) & self.ground(&t1);
        let dont_coerce_case = self.t2z3(&t1)._eq(&self.t2z3(&alpha));
        self.coerce(t1, alpha.clone(), exp);
        (alpha, phi1 & (coerce_case | dont_coerce_case))
    }

    /// Provided a type, generate constraints that the type has any in all of
    /// its negative forms. The function is more weak / general than it could be
    /// due to the difficulties with z3.
    ///
    /// For example, if t has type * -> int, that type is safe to
    /// coerce to any (with wrapping). However, because z3 cannot produce
    /// recursive constraints, and the type * -> (int -> int) is forbidden,
    /// ground is forced to produce the constraint that t has type *
    /// -> *.
    ///
    /// Note that anything that can be mutated is negative.
    ///
    /// One might think that lists are a special case: because lists are
    /// immutable they have no negative positions. However, imagine a function that
    /// is stored in a list. It is inferred to be int -> int, however after being
    /// pulled out of the list it is called with a bool. This is incorrect. We
    /// might say, lists must hold ground types, rather than
    /// any! And you would be right, but notice that we have now produced a
    /// recursive constraint which z3 does not support.
    ///
    /// ground t = is_arr(t) => t = any -> any
    ///                    && is_list(t) => t = list any
    ///                    && is_box(t) => t = box any
    ///                    && is_vect(t) => t = vect any
    fn ground(&self, t: &Typ) -> Bool<'a> {
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
            match env.get(i) {
                Some(s) => *t = s.clone(),
                // there is no constraint whatsoever on what this type
                // can be. Migeed and Parsberg seem to choose Int in this
                // case, though i haven't read enough to know if they
                // explicitly mention that
                None => (),
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
        | Exp::UnaryOp(_, e)
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
        | Exp::BinaryOp(_, e1, e2)
        | Exp::AddOverload(e1, e2)
        | Exp::Cons(e1, e2)
        | Exp::Pair(e1, e2)
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
    if options.debug {
        eprintln!("Simplified constraints:");
        eprintln!("{}", phi.simplify());
    }
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
        if options.debug {
            let mut exp_precise = exp.clone();
            let result = s.solve_model(model);
            annotate(&result, &mut exp_precise);
            println!("precise annotation: {}", exp_precise);
        }
    }
    if options.debug {
        eprintln!("Solver state for final type:");
        eprintln!("{}", s.solver);
    }
    match s.solver.check(&[]) {
        SatResult::Unsat => return Err("unsat (context)".to_string()),
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
        coerces(
            "let f = fun b.fun x. if b then x + 1 else not x in
             let y = f true 5 in
             f false false",
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

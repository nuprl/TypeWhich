//! Binary predicates to check if a type or expression is less precise (or identical) than
//! another.
//!
//! In addition to the usual precision check, these predicates:
//!
//! 1. Account for alpha-renaming, e.g., it considers `fun x . x` and `fun y . y` to be 
//!    identical; and
//!
//! 2. Treat type metavariables as `any`, e.g., it treats `fun x : 'a . x` and `fun x : any . x` as
//!    identical expressions.

use super::syntax::{Exp, Typ, Id};

type Env = im_rc::HashMap<Id, Id>;

pub fn typ_lt(t1: &Typ, t2: &Typ) -> bool {
    use Typ::*;
    match (t1, t2) {
        (_, Any) => true,
        (_, Metavar(_)) => true, // see module-level note
        (Unit, Unit) => true,
        (Int, Int) => true,
        (Float, Float) => true,
        (Bool, Bool) => true,
        (Str, Str) => true,
        (Char, Char) => true,
        (Arr(t11, t12), Arr(t21, t22)) => typ_lt(t11, t21) && typ_lt(t12, t22),
        (Pair(t11, t12), Pair(t21, t22)) => typ_lt(t11, t21) && typ_lt(t12, t22),
        (Vect(t11), Vect(t21)) => typ_lt(t11, t21),
        _ => false,
    }
}


fn exp_lt_rec(env: &Env, e1: &Exp, e2: &Exp) -> bool {
    use Exp::*;
    match (e1, e2) {
        (Lit(l1), Lit(l2)) => l1 == l2,
        (Var(x), Var(y1)) => match env.get(x) {
            None => false,
            Some(y2) => y1 == y2
        },
        (Fun(x, t1, e1), Fun(y, t2, e2)) => {
            if typ_lt(t1, t2) == false {
                false
            }
            else {
                let mut env = env.clone();
                env.insert(x.clone(), y.clone());
                exp_lt_rec(&env, e1, e2)
            }
        }
        (App(e11, e12), App(e21, e22)) => exp_lt_rec(env, e11, e21) && exp_lt_rec(env, e12, e22),
        (BinaryOp(op1, e11, e12), BinaryOp(op2, e21, e22)) => op1 == op2 && exp_lt_rec(env, e11, e21) && exp_lt_rec(env, e12, e22),
        (If(e11, e12, e13), If(e21, e22, e23)) =>exp_lt_rec(env, e11, e21) && exp_lt_rec(env, e12, e22) && exp_lt_rec(env, e13, e23),
        _ => false
    }
}

pub fn exp_lt(e1: &Exp, e2: &Exp) -> bool {
  let env = Env::default();
  exp_lt_rec(&env, e1, e2)
}
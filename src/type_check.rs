use super::syntax::*;
use im_rc::HashMap;

type Env = HashMap<String, Typ>;

pub fn type_check(exp: &Exp) -> Result<Typ, String> {
    tcheck(&Default::default(), exp)
}

fn tcheck(env: &Env, exp: &Exp) -> Result<Typ, String> {
    match exp {
        // ---------------------------
        // Γ ⊢ lit : lit.typ()
        Exp::Lit(lit) => Ok(lit.typ()),
        // ---------------------------
        // Γ ⊢ x : Γ(x)
        Exp::Var(x) => env
            .get(x)
            .ok_or(format!("unbound identifier {}", x))
            .map(Clone::clone),
        // Γ,x:T_1 ⊢ e : T_2
        // ---------------------------------------
        // Γ ⊢ fun (x : T_1) . e : T_1 -> T_2
        Exp::Fun(x, t, body) => {
            let mut env = env.clone();
            env.insert(x.clone(), t.clone());
            let t_body = tcheck(&env, body)?;
            Ok(Typ::Arr(Box::new(t.clone()), Box::new(t_body)))
        }
        // Γ,x:T ⊢ e : T
        // ---------------------------------------
        // Γ ⊢ fix (x : T) . e : T
        Exp::Fix(x, t1, body) => {
            let mut env = env.clone();
            env.insert(x.clone(), t1.clone());
            let t2 = tcheck(&env, body)?;
            should_match(t1, t2)
        }
        // Γ ⊢ e_1 : T_1 -> T_2
        // Γ ⊢ e_2 : T_1
        // ----------------------------------------------
        // Γ ⊢ e_1 e_2 : T_2
        Exp::App(e1, e2) => {
            let t1_to_t2 = tcheck(&env, e1)?;
            let t1 = tcheck(&env, e2)?;
            match t1_to_t2 {
                Typ::Arr(arr_t1, arr_t2) => {
                    should_match(&*arr_t1, t1)?;
                    Ok(*arr_t2)
                }
                _ => Err("expected arrow in application".to_string()),
            }
        }
        // Γ ⊢ e1 : T_1
        // Γ,x:T_1 ⊢ e2 : T_2
        // ---------------------------------------
        // Γ ⊢ let x: T_1 = e1 in e2 : T_2
        Exp::Let(x, t1_ann, e1, e2) => {
            let t1 = should_match(t1_ann, tcheck(&env, e1)?)?;
            let mut env = env.clone();
            env.insert(x.clone(), t1);
            let t2 = tcheck(&env, e2)?;
            Ok(t2)
        }
        // Γ ⊢ e_1 : int
        // Γ ⊢ e_2 : int
        // ----------------------------------------------
        // Γ ⊢ e_1 [+*] e_2 : int
        Exp::Add(e1, e2) | Exp::Mul(e1, e2) => {
            should_match(&Typ::Int, tcheck(&env, e1)?)?;
            should_match(&Typ::Int, tcheck(&env, e2)?)?;
            Ok(Typ::Int)
        }
        // Γ ⊢ e_1 : int
        // Γ ⊢ e_2 : int
        // ----------------------------------------------
        // Γ ⊢ e_1 = e_2 : bool
        Exp::IntEq(e1, e2) => {
            should_match(&Typ::Int, tcheck(&env, e1)?)?;
            should_match(&Typ::Int, tcheck(&env, e2)?)?;
            Ok(Typ::Bool)
        }
        // Γ ⊢ e : bool
        // ----------------------------------------------
        // Γ ⊢ not e : bool
        Exp::Not(e) => {
            should_match(&Typ::Bool, tcheck(&env, e)?)?;
            Ok(Typ::Bool)
        }
        // Γ ⊢ e_1 : T_1 where T_1 ∈ {int, str, any}
        // Γ ⊢ e_2 : T_1
        // ----------------------------------------------
        // Γ ⊢ e_1 +? e_2 : T_1
        Exp::AddOverload(e1, e2) => {
            let t1 = tcheck(&env, e1)?;
            should_match(&t1, tcheck(&env, e2)?)?;
            match t1 {
                Typ::Int | Typ::Str | Typ::Any => Ok(t1),
                _ => Err("add overload not int, str, or any".to_string()),
            }
        }
        // Γ ⊢ e_1 : bool
        // Γ ⊢ e_2 : T_1
        // Γ ⊢ e_3 : T_1
        // ----------------------------------------------
        // Γ ⊢ if e_1 then e_2 else e_3 : T_1
        Exp::If(e1, e2, e3) => {
            should_match(&Typ::Bool, tcheck(&env, e1)?)?;
            let t1 = tcheck(&env, e2)?;
            should_match(&t1, tcheck(&env, e3)?)?;
            Ok(t1)
        }
        // Γ ⊢ e_1 : T_1
        // Γ ⊢ e_2 : T_2
        // ----------------------------------------------
        // Γ ⊢ e_1, e_2 : (T_1, T_2)
        Exp::Pair(e1, e2) => {
            let t1 = tcheck(&env, e1)?;
            let t2 = tcheck(&env, e2)?;
            Ok(Typ::Pair(Box::new(t1), Box::new(t2)))
        }
        // Γ ⊢ e_1 : T_1
        // Γ ⊢ e_2 : List(T_1)
        // ----------------------------------------------
        // Γ ⊢ e_1 :: e_2 : List(T_1)
        Exp::Cons(e1, e2) => {
            let t1 = tcheck(&env, e1)?;
            should_match(&Typ::List(Box::new(t1)), tcheck(&env, e2)?)
        }
        // ----------------------------------------------
        // Γ ⊢ empty : ???
        // TODO(luna): Exp::Empty probably needs an annotation
        Exp::Empty => todo!(),
        // Γ ⊢ e : List(T)
        // ----------------------------------------------
        // Γ ⊢ head e : T
        Exp::Head(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::List(res) => Ok(*res),
                _ => Err("head non-list".to_string()),
            }
        }
        // Γ ⊢ e : List(T)
        // ----------------------------------------------
        // Γ ⊢ tail e : List(T)
        Exp::Tail(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::List(_) => Ok(t),
                _ => Err("tail non-list".to_string()),
            }
        }
        // Γ ⊢ e : List(T)
        // ----------------------------------------------
        // Γ ⊢ is_empty e : bool
        Exp::IsEmpty(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::List(_) => Ok(Typ::Bool),
                _ => Err("is_empty non-list".to_string()),
            }
        }
        // Γ ⊢ e : any
        // ----------------------------------------------
        // Γ ⊢ is_GROUND e : bool
        Exp::IsBool(e) | Exp::IsInt(e) | Exp::IsString(e) | Exp::IsList(e) | Exp::IsFun(e) => {
            should_match(&Typ::Any, tcheck(env, e)?)?;
            Ok(Typ::Bool)
        }
        // Γ ⊢ e : T where T != any
        // ----------------------------------------------
        // Γ ⊢ to_any e : any
        Exp::ToAny(e) => {
            let t1 = tcheck(env, e)?;
            if t1 != Typ::Any {
                Ok(Typ::Any)
            } else {
                Err("to_any any".to_string())
            }
        }
        // Γ ⊢ e : any
        // ----------------------------------------------
        // Γ ⊢ from_any e : ???
        // TODO(luna): i think from_any needs a typ annotation
        Exp::FromAny(e) => {
            should_match(&Typ::Any, tcheck(env, e)?)?;
            todo!("from_any annotation")
        }
        Exp::MaybeToAny(..) => panic!("maybe_to_any not removed by type inference"),
        Exp::MaybeFromAny(..) => panic!("maybe_from_any not removed by type inference"),
    }
}

fn should_match(t1: &Typ, t2: Typ) -> Result<Typ, String> {
    if t1 == &t2 {
        Ok(t2)
    } else {
        mismatched(t1, &t2)
    }
}

fn mismatched<T>(t1: &Typ, t2: &Typ) -> Result<T, String> {
    Err(format!("expected {} got {}", t1, t2))
}

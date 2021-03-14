use super::syntax::*;
use im_rc::HashMap;

type Env = HashMap<String, Typ>;

// this serves as a replacement for line numbers haha. Make this true to
// panic instead of Err (doesn't work for bespoke checks) and you can check the
// backtrace
const PANIC_ON_MISMATCH: bool = true;

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
        // Γ ⊢ let x = e1 in e2 : T_2
        Exp::Let(x, e1, e2) => {
            let t1 = tcheck(&env, e1)?;
            let mut env = env.clone();
            env.insert(x.clone(), t1);
            let t2 = tcheck(&env, e2)?;
            Ok(t2)
        }
        // Γ,x1:T_1,...,xn:T_n ⊢ ei : T_i
        // Γ,x1:T_1,...,xn:T_n ⊢ e : T
        // ---------------------------------------
        // Γ ⊢ letrec x1 : T_1 = e1 ... xn : T_n = en in e : T
        Exp::LetRec(es, e) => {
            let mut env = env.clone();
            for (id, typ, _) in es {
                env.insert(id.clone(), typ.clone());
            }
            for (_, typ, ei) in es {
                should_match(typ, tcheck(&env, ei)?)?;
            }
            tcheck(&env, e)
        }
        // Γ ⊢ e : T
        // ---------
        // Γ ⊢ (e : T) : T
        Exp::Ann(e, typ) => {
            should_match(typ, tcheck(env, e)?)
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
        // Γ ⊢ e : Pair(T_1, T_2)
        // ----------------------------------------------
        // Γ ⊢ fst e : T_1
        Exp::Fst(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::Pair(t1, _) => Ok(*t1),
                _ => Err("fst non-pair".to_string()),
            }
        }
        // Γ ⊢ e : Pair(T_1, T_2)
        // ----------------------------------------------
        // Γ ⊢ snd e : T_1
        Exp::Snd(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::Pair(_, t2) => Ok(*t2),
                _ => Err("snd non-pair".to_string()),
            }
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
        // Γ ⊢ empty: T : List(T)
        Exp::Empty(t) => Ok(Typ::List(Box::new(t.clone()))),
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
        // Γ ⊢ e : T
        // ----------------------------------------------
        // Γ ⊢ box e : Box(T)
        Exp::Box(e) => {
            let t = tcheck(env, e)?;
            Ok(Typ::Box(Box::new(t)))
        }
        // Γ ⊢ e : Box(T)
        // ----------------------------------------------
        // Γ ⊢ unbox e : T
        Exp::Unbox(e) => {
            let t = tcheck(env, e)?;
            match t {
                Typ::Box(t) => Ok(*t),
                _ => Err("unbox non-box".to_string()),
            }
        }
        // Γ ⊢ e_1 : Box(T)
        // Γ ⊢ e_2 : T
        // ----------------------------------------------
        // Γ ⊢ boxset! e1 e2 : Box(T)
        Exp::BoxSet(e1, e2) => {
            let t1 = tcheck(env, e1)?;
            let t2 = tcheck(env, e2)?;
            should_match(&Typ::Box(Box::new(t2)), t1)
        }
        // Γ ⊢ e : any
        // ----------------------------------------------
        // Γ ⊢ is_GROUND e : bool
        Exp::IsBool(e) | Exp::IsInt(e) | Exp::IsString(e) | Exp::IsList(e) | Exp::IsFun(e) => {
            should_match(&Typ::Any, tcheck(env, e)?)?;
            Ok(Typ::Bool)
        }
        // Γ ⊢ e : T_1
        // ----------------------------------------------
        // Γ ⊢ coerce(T_1, T_2) e : T_2
        Exp::Coerce(t1, t2, e) => {
            should_match(t1, tcheck(env, e)?)?;
            Ok(t2.clone())
        }
    }
}

fn should_match(t1: &Typ, t2: Typ) -> Result<Typ, String> {
    if t1 == &t2 {
        Ok(t2)
    } else {
        let e = mismatched(t1, &t2);
        // this serves as a replacement for line numbers haha
        if PANIC_ON_MISMATCH {
            panic!("PANIC_ON_MISMATCH = true: {:?}", e);
        }
        e
    }
}

fn mismatched<T>(t1: &Typ, t2: &Typ) -> Result<T, String> {
    Err(format!("expected {} got {}", t1, t2))
}

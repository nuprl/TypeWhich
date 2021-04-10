//! Type-directed coercion insertion.
//! 
//! This is vanilla, type-directed coercion insertion for the GTLC. There should
//! be nothing innovative in this file, and it has nothing to do with type
//! migration.
//! 
//! At the moment, it only supports the fragment of the language we need for
//! the comparative evaluation.
use im_rc::HashMap;
use super::syntax::{Id, Exp, Typ, Lit, Coerce, GroundTyp};

type Env = HashMap<Id, Typ>;

type R = Result<Typ, String>;

fn lit_typ(lit: &Lit) -> Typ {
    match lit {
        Lit::Int(_) => Typ::Int,
        Lit::Bool(_) => Typ::Bool,
        _ => panic!("unsupported literal {:?}", lit),
    }
}

fn fun_typ(t: Typ) -> Result<(Typ, Typ), String> {
    match t {
        Typ::Arr(dom, rng) => Ok((*dom, *rng)),
        Typ::Any => Ok((Typ::Any, Typ::Any)),
        _ => Ok((Typ::Any, Typ::Any)) // will be doomed
    }
}

pub fn coerce(src: &Typ, dst: &Typ) -> Coerce {
    match (src, dst) {
        (Typ::Int, Typ::Any) => Coerce::Tag(GroundTyp::Int),
        (Typ::Bool, Typ::Any) => Coerce::Tag(GroundTyp::Bool),
        (Typ::Arr(dom, rng), Typ::Any) => Coerce::Wrap(Box::new(coerce(&Typ::Any, dom)), Box::new(coerce(rng, &Typ::Any))).seq(&Coerce::Tag(GroundTyp::Fun)),
        (Typ::Arr(dom1, rng1), Typ::Arr(dom2, rng2)) => 
            Coerce::Wrap(Box::new(coerce(dom2, dom1)),
                Box::new(coerce(rng1, rng2))),
        (Typ::Any, Typ::Int) => Coerce::Untag(GroundTyp::Int),
        (Typ::Any, Typ::Bool) => Coerce::Untag(GroundTyp::Bool),
        (Typ::Any, Typ::Arr(dom, rng)) => 
            Coerce::Untag(GroundTyp::Fun).seq(&Coerce::Wrap(Box::new(coerce(dom, &Typ::Any)), Box::new(coerce(&Typ::Any, rng)))),
        _ => {
            if src == dst {
                Coerce::Id
            }
            else {
                Coerce::Doomed
            }
        }
    }
}

fn ins(mut env: Env, exp: &mut Exp) -> R {
    match exp {
        Exp::Var(x) => {
            let t = env.get(x).ok_or("unbound identifier".to_string())?.clone();
            Ok(t)
        }
        Exp::Lit(l) => {
            Ok(lit_typ(&l))
        }
        Exp::Fun(x, t1, e) => {
            env.insert(x.clone(), t1.clone());
            let t2 = ins(env, e)?;
            Ok(Typ::Arr(Box::new(t1.clone()), Box::new(t2)))
        }
        Exp::Add(e1, e2) => {
            let t1 = ins(env.clone(), e1)?;
            let t2 = ins(env.clone(), e2)?;
            let k2 = coerce(&t2, &Typ::Int);
            let k1 = coerce(&t1, &Typ::Int);
            let e1_inner = std::mem::replace(&mut **e1, Exp::Lit(Lit::Unit));
            **e1 = Exp::PrimCoerce(k1, Box::new(e1_inner));
            let e2_inner = std::mem::replace(&mut **e2, Exp::Lit(Lit::Unit));
            **e2 = Exp::PrimCoerce(k2, Box::new(e2_inner));
            Ok(Typ::Int)
        }
        Exp::App(e1, e2) => {
            let t1 = ins(env.clone(), e1)?;
            let t2 = ins(env.clone(), e2)?;
            let (t11, t12) = fun_typ(t1.clone())?;
            let k2 = coerce(&t2, &t11);
            let k1 = coerce(&t1, &Typ::Arr(Box::new(t11), Box::new(t12.clone())));
            let e1_inner = std::mem::replace(&mut **e1, Exp::Lit(Lit::Unit));
            **e1 = Exp::PrimCoerce(k1, Box::new(e1_inner));
            let e2_inner = std::mem::replace(&mut **e2, Exp::Lit(Lit::Unit));
            **e2 = Exp::PrimCoerce(k2, Box::new(e2_inner));
            Ok(t12)
        }
        Exp::Coerce(_, t2, e) => {
            ins(env.clone(), e)?;
            Ok(t2.clone())
        }
        _ => unimplemented!("{:?}", exp),
    }
}

/// Inserts coercions into the program, which makes it suitable for evaluation.
///
/// This function modifies the expression in-place, because we are in Rust, and
/// can happily do such things! The function will produce an error message on
/// expected failures, e.g., if the program has a free variable.
pub fn insert_coercions(exp: &mut Exp) -> Result<(), String> {
    ins(Env::new(), exp)?;
    return Ok(());
}
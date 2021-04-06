use crate::syntax::{Exp as SrcExp, Typ as SrcTyp, Lit as SrcLit};
use super::syntax::*;

type Result<T> = std::result::Result<T, ()>;

fn next_metavar_typ() -> Typ {
    Typ::Metavar(crate::parser::inc_metavar())
}

pub fn from_typ(t: &SrcTyp) -> Result<Typ> {
    match t {
        SrcTyp::Int => Ok(Typ::Int),
        SrcTyp::Bool => Ok(Typ::Bool),
        SrcTyp::Any => Ok(Typ::Any),
        SrcTyp::Unit => Ok(Typ::Null),
        SrcTyp::Arr(t1, t2) => Ok(Typ::Arr(Box::new(from_typ(t1)?), Box::new(from_typ(t2)?))),
        SrcTyp::Metavar(n) => Ok(Typ::Metavar(*n)),
        _ => Err(()),
    }
}

pub fn from_lit(l: &SrcLit) -> Result<Lit> {
    match l {
        // skips null
        SrcLit::Int(n) => Ok(Lit::Int(*n)),
        SrcLit::Bool(b) => Ok(Lit::Bool(*b)),
        SrcLit::Unit => Ok(Lit::Null),
        _ => Err(()),
    }
}

pub fn from_exp(e: &SrcExp) -> Result<Exp> {
    match e {
        // skips assign and seq
        SrcExp::Lit(l) => Ok(Exp::Lit(from_lit(l)?)),
        SrcExp::Var(x) => Ok(Exp::Var(x.clone())),
        SrcExp::Fun(x, t, e) => Ok(Exp::Fun(x.clone(), from_typ(t)?, Box::new(from_exp(e)?), next_metavar_typ())),
        SrcExp::App(e1, e2) => Ok(Exp::App(Box::new(from_exp(e1)?), Box::new(from_exp(e2)?))),
        SrcExp::If(e1, e2, e3) => 
            Ok(Exp::If(Box::new(from_exp(e1)?), Box::new(from_exp(e2)?), Box::new(from_exp(e3)?))),
        SrcExp::Add(e1, e2) =>
            Ok(Exp::Add(Box::new(from_exp(e1)?), Box::new(from_exp(e2)?))),
        SrcExp::Let(x, e1, e2) =>
            Ok(Exp::App(Box::new(Exp::Fun(x.to_string(), next_metavar_typ(), Box::new(from_exp(e2)?), next_metavar_typ())), Box::new(from_exp(e1)?))),
        _ => Err(())
    }
}

fn to_typ(t: Typ) -> Result<SrcTyp> {
    match t {
        Typ::Null => Ok(SrcTyp::Unit),
        Typ::Int => Ok(SrcTyp::Int),
        Typ::Bool => Ok(SrcTyp::Bool),
        Typ::Arr(t1, t2) => Ok(SrcTyp::Arr(Box::new(to_typ(*t1)?), Box::new(to_typ(*t2)?))),
        Typ::Any => Ok(SrcTyp::Any),
        Typ::Metavar(n) => Ok(SrcTyp::Metavar(n)),
        Typ::MetavarArg(_) => Err(()),
        Typ::MetavarRet(_) => Err(()),
    }
}

fn to_lit(l: Lit) -> SrcLit {
    match l {
        Lit::Null => SrcLit::Unit,
        Lit::Int(n) => SrcLit::Int(n),
        Lit::Bool(b) => SrcLit::Bool(b)
    }
}

pub fn to_exp(e: Exp) -> Result<SrcExp> {
    match e {
        Exp::Lit(l) => Ok(SrcExp::Lit(to_lit(l))),
        Exp::Var(x) => Ok(SrcExp::Var(x)),
        Exp::Assign(_, _) => Err(()),
        Exp::Fun(x, t, e, _) => Ok(SrcExp::Fun(x, to_typ(t)?, Box::new(to_exp(*e)?))),
        Exp::App(e1, e2) => Ok(SrcExp::App(Box::new(to_exp(*e1)?), Box::new(to_exp(*e2)?))),
        Exp::Coerce(t1, t2, e) => Ok(SrcExp::Coerce(to_typ(t1)?, to_typ(t2)?, Box::new(to_exp(*e)?))),
        Exp::If(e1, e2, e3) => Ok(SrcExp::If(Box::new(to_exp(*e1)?), Box::new(to_exp(*e2)?), Box::new(to_exp(*e3)?))),
        Exp::Add(e1, e2) => Ok(SrcExp::Add(Box::new(to_exp(*e1)?), Box::new(to_exp(*e2)?))),
        Exp::Seq(_, _) => Err(()),    
    }
}


//! An evaluator for GTLC + extensions needed for the comparative evaluation.
use im_rc::HashMap;
use derive_more::Display;
use super::syntax::{Lit, Exp, Id, Coerce, GroundTyp};

type Env<'a> = HashMap<&'a Id, Val<'a>>;

#[derive(Clone, Debug)]
enum Val<'a> {
    Lit(Lit),
    Closure(Env<'a>, &'a Id, &'a Exp, Coerce, Coerce),
    Tagged(GroundTyp, Box<Val<'a>>),
}

pub enum Answer {
    Lit(Lit),
    Closure
}

#[derive(Display)]
pub enum Error {
    #[display(fmt = "coercion failed: {}", _0)]
    Coercion(String),
}

struct Eval {
    // Eventually, we will probably need an arena here for mutable references.
}

impl<'a> Val<'a> {
    fn ground_typ(&self) -> GroundTyp {
        match self {
            Val::Closure(..) => GroundTyp::Fun,
            Val::Lit(Lit::Bool(..)) => GroundTyp::Bool,
            Val::Lit(Lit::Int(..)) => GroundTyp::Int,
            // Coercion insertion should ensure this does not occur
            _ => panic!("unsupported type"),
        }
    }
    
    fn to_answer(self) -> Answer {
        match self {
            Val::Lit(l) => Answer::Lit(l),
            Val::Tagged(_, v) => v.to_answer(),
            Val::Closure(..) => Answer::Closure
        }
    }
}

type EvalResult<'a> = Result<Val<'a>, Error>;

impl Eval {

    fn eval_k<'a>(&'a self, k: &Coerce, v: Val<'a>) -> EvalResult<'a>  {
        match k {
            Coerce::Doomed => Err(Error::Coercion("doomed".to_string())),
            Coerce::Id => Ok(v),
            Coerce::Seq(k1, k2) => self.eval_k(k2, self.eval_k(k1, v)?),
            Coerce::Tag(g) => {
                let g2 = v.ground_typ();
                if &g2 == g {
                    Ok(Val::Tagged(g2, Box::new(v)))
                }
                else {
                    Err(Error::Coercion(format!("tag({:?}) on {:?}", g, v)))
                }
            },
            Coerce::Untag(g) => {
                match v {
                    Val::Tagged(g2, v) => {
                        if g == &g2 {
                            Ok(*v)
                        }
                        else {
                            Err(Error::Coercion(format!("untag({:?})", g)))
                        }
                    }
                    _ => Err(Error::Coercion(format!("untagged a not-tagged value")))
                }
            }
            Coerce::Wrap(dom, rng) => {
                match v {
                    Val::Closure(env, x, body, dom1, rng1) => {
                        // TODO(arjun): Ordering matters
                        Ok(Val::Closure(env, x, body, dom.seq(&dom1), rng1.seq(&rng)))
                    }
                    _ => Err(Error::Coercion(format!("wrap on a non-function"))),
                }
            }
        }
    } 

    fn eval<'a>(&'a self, env: Env<'a>, exp: &'a Exp) -> EvalResult<'a>  {
        match exp {
            Exp::Lit(l) => Ok(Val::Lit(l.clone())),
            Exp::Var(x) => {
                // Coercion insertion should ensure this does not occur
                Ok(env.get(x).cloned().expect("unbound identifier"))
            }
            Exp::Fun(x, _, e) => Ok(Val::Closure(env.clone(), x, e, Coerce::Id, Coerce::Id)),
            Exp::App(e1, e2) => {
               
                let v1 = self.eval(env.clone(), e1)?;
                let v2 = self.eval(env.clone(), e2)?;
                match v1 {
                    Val::Closure(mut cl_env, x, body, dom, rng) => {
                        let v2 = self.eval_k(&dom, v2)?;
                        cl_env.insert(x, v2);
                        let r = self.eval(cl_env, body)?;
                        self.eval_k(&rng, r)
                    }
                    // Coercion insertion should ensure this does not occur
                    _ => panic!("expected closure value in function position (got {:?})", v1)
                }
            }
            Exp::Coerce(t1, t2, e) => {
                let k = super::insert_coercions::coerce(&t1, &t2);
                let v = self.eval(env, e)?;
                self.eval_k(&k, v)
            }
            Exp::PrimCoerce(k, e) => {
                self.eval_k(k, self.eval(env, e)?)
            }
            Exp::Add(e1, e2) => {
                let v1 = self.eval(env.clone(), e1)?;
                let v2 = self.eval(env.clone(), e2)?;
                match (v1, v2) {
                    (Val::Lit(Lit::Int(m)), Val::Lit(Lit::Int(n))) => Ok(Val::Lit(Lit::Int(m + n))),
                    // Panic because coercion insertion produced an unsafe program!
                    _ => panic!("+ received a non-int argument"),
                }
            }
            _ => unimplemented!(),
        }
    }
}

/// Assumes that the expression has coercions inserted.
pub fn eval(exp: Exp) -> Result<Answer, Error> {
    let eval = Eval {  };
    let v = eval.eval(Env::new(), &exp)?;
    return Ok(v.to_answer());
}


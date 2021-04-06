use crate::syntax::*;
use im_rc::HashMap;

pub fn decorate(exp: &mut Exp, solution: &HashMap<Typ, Typ>) {
    match exp {
        Exp::Lit(_) | Exp::Var(_) => (),
        Exp::Assign(_, e) => {
            decorate(e, &solution);
        }
        Exp::Fun(_, t1, e, t2) => {
            decorate_typ(t1, &solution);
            decorate_typ(t2, &solution);
            decorate(e, &solution);
        }
        Exp::App(e1, e2) | Exp::Seq(e1, e2) | Exp::Add(e1, e2) => {
            decorate(e1, &solution);
            decorate(e2, &solution);
        }
        Exp::Coerce(t1, t2, e) => {
            decorate_typ(t1, &solution);
            decorate_typ(t2, &solution);
            decorate(e, &solution);
            if t1 == t2 {
                *exp = e.take();
            }
        }
        Exp::If(e1, e2, e3) => {
            decorate(e1, &solution);
            decorate(e2, &solution);
            decorate(e3, &solution);
        }
    }
}

pub fn decorate_typ(t: &mut Typ, solution: &HashMap<Typ, Typ>) {
    match t {
        // this isn't exactly said in the paper but i think it's necessary to
        // fully decorate. well after all none of decoration is really written
        // down
        //
        // to what extent does this duplicate with Î? i don't know
        Typ::Arr(t1, t2) => {
            decorate_typ(t1, solution);
            decorate_typ(t2, solution);
        }
        _ if t.is_metavar() => match solution.get(t) {
            Some(found) => {
                assert!(!found.is_metavar());
                *t = found.clone();
                decorate_typ(t, solution);
            }
            None => {
                eprintln!(
                    "WARNING: no solution for {}. this probably means
                there are no constraints on the type. Writing int",
                    t
                );
                *t = Typ::Metavar(4444);
            }
        },
        _ => (),
    }
}

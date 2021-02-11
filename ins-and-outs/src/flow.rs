use crate::syntax::*;
use crate::Closure;

/// provide Base and Comp (from Figure 4) (aka Def 3.9: 1)
pub fn compute_closure(cs: Closure) -> Closure {
    let init = cs.clone();
    let step2 = pull_and_factor(cs);
    // Def 3.9: 5
    if step2 != init {
        compute_closure(split_fun(tran_and_exp_fun(step2)))
    } else {
        init
    }
}

/// this isn't in the spec, but makes it easier to read closures, and i don't
/// think it should make a difference
fn e(e: &mut Closure, t1: Typ, t2: Typ) {
    if t1 != t2 {
        e.insert((t1, t2));
    }
}

/// Definition 3.9: 2
fn pull_and_factor(cs: Closure) -> Closure {
    // Base
    let mut expanded = cs.clone();
    for c in cs.clone().into_iter() {
        match c {
            (from, to @ Typ::Metavar(..)) if !from.is_metavar() => {
                // K |> X
                // Pull
                // surely this doesn't have to be O(n^2)? check complexity analysis
                for c in cs.clone().into_iter() {
                    match c {
                        (from2, to2 @ Typ::Metavar(..)) if from2 == to => {
                            e(&mut expanded, from.clone(), to2);
                        }
                        _ => (),
                    }
                }
                // Factor
                // TODO(luna): why doesn't the figure specify K |> X when it specifies T |> X?
                let kind = from.kind_of_typ_var(&to);
                e(&mut expanded, from, kind.clone());
                e(&mut expanded, kind, to);
            }
            _ => (),
        }
    }
    if cs != expanded {
        pull_and_factor(expanded)
    } else {
        expanded
    }
}

/// Def 3.9: 3
fn tran_and_exp_fun(cs: Closure) -> Closure {
    let any_to_any = Typ::Arr(Box::new(Typ::Any), Box::new(Typ::Any));
    let mut expanded = cs.clone();
    for c in cs.clone().into_iter() {
        match c {
            // ExpFunL
            (Typ::Any, to @ Typ::Arr(..)) => {
                e(&mut expanded, any_to_any.clone(), to.clone());
            }
            // ExpFunR
            (from @ Typ::Arr(..), Typ::Any) => {
                e(&mut expanded, from.clone(), any_to_any.clone());
            }
            // Tran
            (k, x) if !k.is_metavar() => {
                // surely this doesn't have x be O(n^2)? check complexity analysis
                for c in cs.clone().into_iter() {
                    match c {
                        (x2, t) if x2 == x && k.dyn_consistent(&t) => {
                            e(&mut expanded, k.clone(), t);
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }
    }
    if cs != expanded {
        tran_and_exp_fun(expanded)
    } else {
        expanded
    }
}

/// Def 3.9: 4
fn split_fun(cs: Closure) -> Closure {
    let mut expanded = cs.clone();
    for c in cs.clone().into_iter() {
        match c {
            // SplitFun
            // TODO(luna): potential optimization: if any t[12]p? is_arr, then
            // repeat, but if it never happens, don't bother checking equality
            // and exit
            (Typ::Arr(t1, t2), Typ::Arr(t1p, t2p)) => {
                e(&mut expanded, *t1p.clone(), *t1.clone());
                e(&mut expanded, *t2.clone(), *t2p.clone());
            }
            _ => (),
        }
    }
    if cs != expanded {
        split_fun(expanded)
    } else {
        expanded
    }
}

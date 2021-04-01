use crate::syntax::*;
use crate::Closure;

/// provide Base and Comp (from Figure 4) (aka Def 3.9: 1)
pub fn compute_closure(cs: Closure) -> Closure {
    compute_closure_check_complete(split_fun(tran_and_exp_fun(pull_and_factor(cs))))
}

fn compute_closure_check_complete(cs: Closure) -> Closure {
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
fn e(c: &mut Closure, t1: Typ, t2: Typ) {
    if t1 != t2 {
        assert!(
            !t1.is_base() || !t2.is_base(),
            "flow should not produce base |> base coercions: {} |> {}",
            t1,
            t2
        );
        c.insert((t1, t2));
    }
}

/// for debugging, this prints the coercion (or two) that combine to produce
/// the coercion that is added to the closure, along with that coercion
fn p(c: &mut Closure, description: &'static str, reasons: Vec<(&Typ, &Typ)>, t1: Typ, t2: Typ) {
    if t1 != t2 {
        if crate::DEBUG {
            let mut reasoning = String::from(description);
            reasoning.push_str(": ");
            for reason in reasons {
                reasoning.push_str(&format!("{} |> {}, ", reason.0, reason.1));
            }
            reasoning.push_str(&format!("==> {} |> {}", t1, t2));
            eprintln!("{}", reasoning);
        }
        let as_tup = (t1, t2);
        if !c.contains(&as_tup) {
            e(c, as_tup.0, as_tup.1);
        }
    }
}

/// Definition 3.9: 2
fn pull_and_factor(cs: Closure) -> Closure {
    // Base
    let mut expanded = cs.clone();
    for c in cs.clone().into_iter() {
        match c {
            (from, to) if to.is_metavar() => {
                if from.is_kind(&to) {
                    // K |> X
                    // Pull
                    // surely this doesn't have to be O(n^2)? check complexity analysis
                    for c in cs.clone().into_iter() {
                        match c {
                            (from2, to2) if from2 == to && to2.is_metavar() => {
                                p(
                                    &mut expanded,
                                    "Pull",
                                    vec![(&from, &to), (&from2, &to2.clone())],
                                    from.clone(),
                                    to2,
                                );
                            }
                            _ => (),
                        }
                    }
                }
                // Factor
                // T |> X
                if !from.is_metavar() {
                    let kind = from.kind_of_typ_var(&to);
                    p(
                        &mut expanded,
                        "Factor",
                        vec![(&from, &to)],
                        from.clone(),
                        kind.clone(),
                    );
                    p(
                        &mut expanded,
                        "Factor",
                        vec![(&from, &to.clone())],
                        kind,
                        to,
                    );
                }
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
                p(
                    &mut expanded,
                    "ExpFunL",
                    vec![(&Typ::Any, &to)],
                    any_to_any.clone(),
                    to.clone(),
                );
            }
            // ExpFunR
            (from @ Typ::Arr(..), Typ::Any) => {
                p(
                    &mut expanded,
                    "ExpFunR",
                    vec![(&from, &Typ::Any)],
                    from.clone(),
                    any_to_any.clone(),
                );
            }
            // Tran
            (k, x) if x.is_metavar() && k.is_kind(&x) => {
                // surely this doesn't have x be O(n^2)? check complexity analysis
                for c in cs.clone().into_iter() {
                    match c {
                        (x2, t) if x2 == x && k.dyn_consistent(&t) => {
                            // it isn't seen in the paper formulation because
                            // they only have one base type, but a base |> base
                            // makes no sense. it doesn't cause any problems
                            // based on the rules, but it makes closures bigger
                            // than they have to be
                            if !(k.is_base() && t.is_base()) {
                                p(
                                    &mut expanded,
                                    "Tran",
                                    vec![(&k, &x), (&x2, &t.clone())],
                                    k.clone(),
                                    t,
                                );
                            }
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
                p(
                    &mut expanded,
                    "SplitFun",
                    vec![(
                        &Typ::Arr(t1.clone(), t2.clone()),
                        &Typ::Arr(t1p.clone(), t2p.clone()),
                    )],
                    *t1p.clone(),
                    *t1.clone(),
                );
                p(
                    &mut expanded,
                    "SplitFun",
                    vec![(
                        &Typ::Arr(t1.clone(), t2.clone()),
                        &Typ::Arr(t1p.clone(), t2p.clone()),
                    )],
                    *t2.clone(),
                    *t2p.clone(),
                );
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

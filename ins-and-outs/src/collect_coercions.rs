use crate::parser::next_metavar_typ;
use crate::syntax::*;
use im_rc::{HashMap, HashSet};

/// This also performs Base and Comp of the closure computation
pub fn compile_coercions(x: Exp) -> (Exp, HashSet<(Typ, Typ)>) {
    // Base
    let (e, t, mut c) = compile(x, &HashMap::new());
    // Comp
    c.insert((t.get_ret(), Typ::Any));
    c.insert((Typ::Any, t.get_arg()));
    let as_arrow = Typ::Arr(Box::new(t.get_arg()), Box::new(t.get_ret()));
    c.insert((as_arrow.clone(), t.clone()));
    c.insert((t, as_arrow));
    (e, c)
}

/// this matches Figure 3: Compilation judgment, except that we collect
/// coercions into a HashSet so as to not have to collect them for closure
/// computation later
fn compile(exp: Exp, env: &HashMap<Id, Typ>) -> (Exp, Typ, HashSet<(Typ, Typ)>) {
    match exp {
        // -------------------------
        // Γ ⊢ null ↪ null :: null
        Exp::Null => (exp, Typ::Null, empty()),
        // ------------------
        // Γ ⊢ x : (Γ(x), ∅)
        Exp::Var(ref x) => {
            let t = env.get(x).expect("unknown identifier").clone();
            (exp, t, empty())
        }
        // Γ(x) = t1
        // Γ ⊢ e ↪ e' :: t2
        // ------------------
        // Γ ⊢ x = e ↪ ⟨t2 ▷ t1⟩ e' :: t1
        Exp::Assign(id, e) => {
            let t1 = env.get(&id).expect("unknown identifier").clone();
            let (ep, t2, c1) = compile(*e, env);
            let (coerced, c2) = coerce(t2, t1.clone(), ep);
            (Exp::Assign(id, Box::new(coerced)), t1, c1.union(c2))
        }
        // Γ[x↦t1] ⊢ e2 ↪ e2' :: t2'
        // e2'' = ⟨t2' ▷ t2⟩ e2'
        // -----------------------------
        // Γ ⊢ fun (x:t1) e2:t2 ↪ fun (x:t1) (e2'':t2) :: t1 -> t2
        Exp::Fun(x, t1, e2, t2) => {
            let mut env = env.clone();
            env.insert(x.clone(), t1.clone());
            let (e2p, t2p, c1) = compile(*e2, &env);
            let (e2pp, c2) = coerce(t2p, t2.clone(), e2p);
            let e = Exp::Fun(x, t1.clone(), Box::new(e2pp), t2.clone());
            let t = Typ::Arr(Box::new(t1), Box::new(t2));
            (e, t, c1.union(c2))
        }
        // Γ ⊢ e ↪ e' :: t
        // t -o t1 -> t2
        // Γ ⊢ e1 ↪ e1' :: t1'
        // e1'' = ⟨t1' ▷ t1⟩ e1'
        // ----------------------
        // Γ ⊢ e e1 ↪ (⟨t ▷ t1 -> t2⟩ e') e1'' :: t2
        Exp::App(e, e1) => {
            let (ep, t, c1) = compile(*e, env);
            let (e1p, t1p, c2) = compile(*e1, env);
            let t1 = t.get_arg();
            let (e1pp, c3) = coerce(t1p, t1.clone(), e1p);
            let t2 = t.get_ret();
            let arr_typ = Typ::Arr(Box::new(t1.clone()), Box::new(t2.clone()));
            let (f, c4) = coerce(t, arr_typ, ep);
            let res_e = Exp::App(Box::new(f), Box::new(e1pp));
            (res_e, t2, c1.union(c2).union(c3).union(c4))
        }
        // Γ ⊢ e ↪ e' :: t
        // Γ ⊢ e1 ↪ e1' :: t1
        // Γ ⊢ e2 ↪ e2' :: t2
        // Γ ⊢ e1'' = ⟨t1 ▷ α⟩ e1'
        // Γ ⊢ e2'' = ⟨t2 ▷ α⟩ e2'
        // ----------------------------
        // Γ ⊢ if e then e1 else e2 ↪ if e' then e1'' else e2'' :: α
        Exp::If(e, e1, e2) => {
            // t is null-check, which i guess, every type is actually T | null. i
            // guess this is called ⊥? although evaluating ⊥ doesn't seem
            // to be a problem, since `if` would have to evaluate it to reach its
            // alternate arm. this is weird, and i don't like it, but i think
            // this is the ActionScript talking, although eliminating
            // null/undefined is one part of why we're doing type inference for
            // Jankscripten, so i'm sticking with "i don't like it"
            let (ep, _t, c1) = compile(*e, env);
            let (e1p, t1, c2) = compile(*e1, env);
            let (e2p, t2, c3) = compile(*e2, env);
            let alpha = next_metavar_typ();
            let (e1pp, c4) = coerce(t1, alpha.clone(), e1p);
            let (e2pp, c5) = coerce(t2, alpha.clone(), e2p);
            let res_e = Exp::If(Box::new(ep), Box::new(e1pp), Box::new(e2pp));
            (res_e, alpha, c1.union(c2).union(c3).union(c4).union(c5))
        }
        Exp::Coerce(..) => panic!("coercions don't exist during compilation"),
    }
}

fn empty() -> HashSet<(Typ, Typ)> {
    HashSet::new()
}

fn coerce(t1: Typ, t2: Typ, e: Exp) -> (Exp, HashSet<(Typ, Typ)>) {
    if t1 == t2 {
        (e, empty())
    } else {
        (
            Exp::Coerce(t1.clone(), t2.clone(), Box::new(e)),
            HashSet::unit((t1, t2)),
        )
    }
}

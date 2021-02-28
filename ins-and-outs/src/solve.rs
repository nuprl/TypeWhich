use crate::syntax::Typ;
use crate::Closure;
use im_rc::HashMap;

pub fn solve_closure(cs: Closure) -> HashMap<Typ, Typ> {
    // This is called T+ in the paper
    // (X => set of types that flow IN to X)
    let mut inflows = HashMap::new();
    for (from, to) in cs.into_iter() {
        if to.is_metavar() && !from.is_metavar() {
            inflows.entry(to).or_insert(vec![]).push(from);
        }
    }
    eprintln!("inflows (T+): {:?}", inflows);
    // this is the big summation that goes into Î
    // it is the least upper bound of the given
    let mut lubs = HashMap::new();
    for (x, t) in inflows.into_iter() {
        eprintln!("solving {}", x);
        // all entries will either not exist or have at least a 0th entry
        let (first, t) = t.split_first().unwrap();
        let lub = t.iter().fold(first.kind_of_typ_var(&x), |lub, k| {
            lub.least_upper_bound(&k.kind_of_typ_var(&x))
        });
        lubs.insert(x, lub);
    }
    eprintln!("pre-recursion lubs: {:?}", lubs);
    let mut solution = HashMap::new();
    for (x, i) in lubs.clone() {
        solution.insert(x, solve_recursively(i, &lubs));
    }
    solution
}

/// this is called Î in the paper - it substitutes the solved kinds
/// recursively into higher kinded types (arrows, since we've ignored objects
/// so far)
///
/// the paper claims that this solution being not fully solved at the time of
/// resolving this is not a problem "since kinds do not have cyclic dependencies"
fn solve_recursively(t: Typ, i: &HashMap<Typ, Typ>) -> Typ {
    match t {
        Typ::Arr(t1, t2) => Typ::Arr(
            Box::new(solve_recursively(*t1, i)),
            Box::new(solve_recursively(*t2, i)),
        ),
        _ if t.is_metavar() => match i.get(&t) {
            Some(t) => t.clone(),
            None => {
                eprintln!(
                    "while resolving a higher kinded type, the lower-kinded type {} was not solved",
                    t
                );
                // this is probly definitely wrong, and this oddness in *never* specified
                Typ::Any
            }
        },
        _ => t,
    }
}

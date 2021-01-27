use crate::tests_631::coerces;

#[test]
fn fact_church() {
    coerces(
        "
        let add1  = 
              fun x. 1 + x in
        let one  = 
              fun f. fun x. f x in
        let five  = 
              fun f. fun x. f (f (f (f (f x)))) in
        let pred  = 
              fun n.
                (fun f.
                  (fun x.
                    (((n (fun g. fun h. h (g f))) 
                      (fun u. x))
                     (fun u. u)))) in
        let mult  = 
              fun m.
                (fun n.
                  (fun f. m (n f))) in
        let _true   = 
               fun a. fun b. a in
        let _false  = 
               fun a. fun b. b in
        let is0   = 
              fun n. n (fun x. _false) _true in
        let fact  = 
              fix fact. fun n.
                ((     (is0 n) // if
                       (fun x. one))
                       (fun x. (mult n) (fact (pred n)))) in
        let realize = fun n . n add1 0 in // : (int -> int) -> (int -> int)
        let n = fact five in
        realize n",
    );
}

#[test]
fn fact_dyn() {
    coerces(
        "
        let f = fun f.fun n.
            if n = 0
                then 1
                else n * (f f (n + -1)) in
        f f 6",
    );
}

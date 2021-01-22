mod cgen;
mod parser;
mod pretty;
mod syntax;

use std::io::*;

lrlex::lrlex_mod!("lexer.l"); // effectively mod `lexer_l`
lrpar::lrpar_mod!("parser.y"); // effectively mod `parser_y`

fn main() -> Result<()> {
    let mut args = std::env::args();
    args.next();
    let source = match args.next() {
        Some(file) => std::fs::read_to_string(file)?,
        None => {
            let mut out = String::new();
            stdin().read_to_string(&mut out)?;
            out
        }
    };
    println!("{}", cgen::typeinf(&parser::parse(source)).unwrap());
    Ok(())
}

#[cfg(test)]
mod tests_631 {
    use super::cgen::typeinf;
    use super::parser::parse;
    use super::syntax::Exp;
    trait PairOr {
        fn or(&self, other: Self) -> Self;
    }
    impl PairOr for (bool, bool) {
        fn or(&self, other: Self) -> Self {
            (self.0 || other.0, self.1 || other.1)
        }
    }
    // (to_any, from_any)
    pub fn contains_coercions(e: Exp) -> (bool, bool) {
        match e {
            Exp::ToAny(.., e) => (true, contains_coercions(*e).1),
            Exp::FromAny(.., e) => (contains_coercions(*e).0, true),
            Exp::MaybeFromAny(..) | Exp::MaybeToAny(..) => {
                panic!("should have been eliminated by typeinf")
            }
            Exp::Lit(..) | Exp::Var(..) | Exp::Empty => (false, false),
            Exp::Fun(_, _, e)
            | Exp::Fix(_, _, e)
            | Exp::Head(e)
            | Exp::Tail(e)
            | Exp::IsEmpty(e)
            | Exp::IsBool(e)
            | Exp::IsInt(e)
            | Exp::IsString(e)
            | Exp::IsList(e)
            | Exp::IsFun(e) => contains_coercions(*e),
            Exp::App(e1, e2)
            | Exp::Add(e1, e2)
            | Exp::Mul(e1, e2)
            | Exp::Cons(e1, e2)
            | Exp::Pair(e1, e2)
            | Exp::Let(_, e1, e2) => contains_coercions(*e1).or(contains_coercions(*e2)),
            Exp::If(e1, e2, e3) => contains_coercions(*e1)
                .or(contains_coercions(*e2))
                .or(contains_coercions(*e3)),
        }
    }
    pub fn succeeds(program: &str) {
        let orig = parse(program);
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(&orig).unwrap();
        println!("\nAfter type inference:\n{}", e);
        let coercions = contains_coercions(e);
        assert!(!coercions.0 && !coercions.1);
    }
    pub fn no_from_any(program: &str) {
        let orig = parse(program);
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(&orig).unwrap();
        println!("\nAfter type inference:\n{}", e);
        let coercions = contains_coercions(e);
        assert!(!coercions.1);
    }
    pub fn coerces(program: &str) {
        let orig = parse(program);
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(&orig).unwrap();
        println!("\nAfter type inference:\n{}", e);
        let coercions = contains_coercions(e);
        assert!(coercions.0 || coercions.1);
    }
    #[test]
    fn addition() {
        succeeds("200 + 9101");
    }
    #[test]
    fn num_plus_bool() {
        coerces("1 + true");
    }
    #[test]
    fn indir_int_equal_bool() {
        coerces(
            "fun p .
                (fun foo . foo 10 p true)
                    (fun x . fun y . fun z .
                        if true then z
                        else
                            (fun w . w) (if true then y else (fun w0 . w0) x))",
        );
    }
    #[test]
    fn key_is_bool_and_int() {
        // previously ended in else key > 10 which would yield bool and
        // constrain key to int, so we make something similar type-wise without
        // adding comparisons
        coerces(
            "fun key . if true then if true then key else true else
                (fun i . true) (key + 10)",
        );
    }
    #[test]
    fn lots_of_conditionals() {
        coerces(
            "fun x . fun y . fun z .
               (if true then x else y) :: (if true then y else z) ::
                  (if true then z else (fun w . w) 5) :: (if x then empty else empty)",
        );
    }
    #[test]
    fn bool_const() {
        succeeds("true");
    }
    #[test]
    fn list_of_booleans() {
        succeeds("true :: empty");
    }
    #[test]
    fn list_of_numbers() {
        succeeds("100 :: empty");
    }
    #[test]
    fn factorial() {
        // should be if n == 0 instead of if false but it's probably not a
        // particularly important operation to have
        succeeds(
            "let fac = fix fac . fun n . if false then 1 else n * fac (n + -1) in
             fac 50 + fac 100",
        );
    }
    #[test]
    fn extract_list() {
        succeeds("head (2 :: empty) + 5");
    }
    #[test]
    fn identity_polymorphic() {
        coerces(
            "let id = fun x . x in
            let f = fun anid .
                let n = id 10 in
                let b = id true in
                5 in
            f id",
        );
    }
    #[test]
    fn simple_arith() {
        succeeds("(fun x . x + 1) 10");
    }
    #[test]
    fn numeric_const() {
        succeeds("908");
    }
    /// i believe this test counters the hypothesis: although is_empty 500
    /// fails in HM, it also fails in our language, because we statically know that
    /// 500 is not a list, and there is no opportunity for it to become an any
    #[test]
    #[should_panic]
    fn is_empty_number() {
        coerces("is_empty 500");
    }
    #[test]
    fn is_empty_list() {
        succeeds("is_empty (1 :: empty)");
    }
    #[test]
    fn real_map() {
        succeeds(
            "let map = fix map . fun f . fun lst .
               if is_empty(lst) then
                 empty
               else
                 f(head(lst)) :: (map f (tail(lst))) in
               map (fun n . n + 1) (1 :: 2 :: 3 :: empty)",
        );
    }
    #[test]
    fn bogus_map() {
        succeeds(
            "let map = fun f . fun lst .
               f(head(lst)) :: f(head(tail(lst))) :: empty in
                   map (fun n . n + 1) (1 :: 2 :: 3 :: empty)",
        );
    }
    // = not yet supported: extract a value from a record =
    // = not yet supported: extract a value from a non-record =
    #[test]
    fn double() {
        succeeds(
            "let square = fun n . if false then 0 else n + n in
            square 10 + square 5",
        );
    }
    #[test]
    fn tail_wag() {
        succeeds("12 :: (tail (12 :: empty))");
    }
    #[test]
    fn tail_toggle() {
        succeeds("tail (1 :: empty)");
    }
    // = not yet supported: arrays are homogenous =
    #[test]
    fn dyn_list_single_level() {
        coerces("1 :: (false :: empty)");
    }
    #[test]
    fn dyn_list_nested() {
        coerces("1 :: (false :: ((2 :: (true :: empty)) :: empty))");
    }
    #[test]
    fn flatten_body() {
        coerces(
            "let flatten = fun append . fun f . fun x .
               if is_list x then append (f (head x)) (f (tail x)) else x :: empty in
               let l = 1 :: (false :: ((2 :: (true :: empty)) :: empty)) in
               flatten (fun x . fun y. x) (fun x. x) l",
        );
    }
}

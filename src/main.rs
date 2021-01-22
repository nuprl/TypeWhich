mod cgen;
mod parser;
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
    println!("{:?}", cgen::typeinf(&parser::parse(source)));
    Ok(())
}

#[cfg(test)]
mod tests_631 {
    use super::cgen::typeinf;
    use super::parser::parse;
    use super::syntax::Exp;
    fn contains_coercions(e: Exp) -> bool {
        match e {
            Exp::FromAny(..) | Exp::ToAny(..) => true,
            Exp::MaybeFromAny(..) | Exp::MaybeToAny(..) => {
                panic!("should have been eliminated by typeinf")
            }
            Exp::Lit(..) | Exp::Var(..) | Exp::Empty => false,
            Exp::Fun(_, _, e)
            | Exp::Head(e)
            | Exp::Tail(e)
            | Exp::IsEmpty(e)
            | Exp::IsBool(e)
            | Exp::IsInt(e)
            | Exp::IsString(e)
            | Exp::IsList(e)
            | Exp::IsFun(e) => contains_coercions(*e),
            Exp::App(e1, e2) | Exp::Add(e1, e2) | Exp::Cons(e1, e2) => {
                contains_coercions(*e1) || contains_coercions(*e2)
            }
            Exp::If(e1, e2, e3) => {
                contains_coercions(*e1) || contains_coercions(*e2) || contains_coercions(*e3)
            }
        }
    }
    fn succeeds(program: &str) {
        let e = typeinf(&parse(program)).unwrap();
        println!("{:?}", e);
        assert!(!contains_coercions(e));
    }
    fn coerces(program: &str) {
        let e = typeinf(&parse(program)).unwrap();
        println!("{:?}", e);
        assert!(contains_coercions(e));
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
    fn extract_list() {
        succeeds("head (2 :: empty) + 5");
    }
    #[test]
    fn bogus_map() {
        succeeds(
            "let map = fun f . fun lst .
               f(head(lst)) :: f(head(tail(lst))) :: empty in
                   map (fun n . n + 1) (1 :: 2 :: 3 :: empty)",
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
    fn double() {
        succeeds(
            "let square = fun n . if false then 0 else n + n in
            square 10 + square 5",
        );
    }
    #[test]
    fn flatten_body() {
        coerces("let flatten = fun append . fun f . fun x . 
                    if is_list x then append (f (head x)) (f (tail x)) else x :: empty
                  in flatten (1 :: empty)");
    }
}

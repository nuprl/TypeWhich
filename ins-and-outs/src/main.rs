mod collect_coercions;
mod decorate;
mod flow;
mod parser;
mod pretty;
mod solve;
mod syntax;
//mod type_check;

use std::io::*;

lrlex::lrlex_mod!("lexer.l"); // effectively mod `lexer_l`
lrpar::lrpar_mod!("parser.y"); // effectively mod `parser_y`

type Closure = im_rc::HashSet<(syntax::Typ, syntax::Typ)>;

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
    let parsed = parser::parse(source);
    typeinf(parsed);
    Ok(())
}

pub fn typeinf(parsed: syntax::Exp) -> syntax::Exp {
    let (mut coerced, coercions) = collect_coercions::compile_coercions(parsed);
    println!(
        "{}\ncoercions:\n{}",
        coerced,
        pretty::DisplayClosure(&coercions)
    );
    let coercions_closure = flow::compute_closure(coercions);
    println!("closure:\n{}", pretty::DisplayClosure(&coercions_closure));
    let solution = solve::solve_closure(coercions_closure);
    println!("solution:\n{:?}", solution);
    decorate::decorate(&mut coerced, &solution);
    println!("annotated:\n{}", coerced);
    coerced
}

#[cfg(test)]
mod tests {
    use super::parser::parse;
    use super::syntax::*;
    use super::typeinf;
    //use super::type_check::type_check;
    pub fn contains_coercions(e: Exp) -> bool {
        match e {
            Exp::Coerce(..) => true,
            Exp::Lit(..) | Exp::Var(..) => false,
            Exp::Fun(_, _, e, _) | Exp::Assign(_, e) => contains_coercions(*e),
            Exp::App(e1, e2) => contains_coercions(*e1) || contains_coercions(*e2),
            Exp::If(e1, e2, e3) => {
                contains_coercions(*e1) || contains_coercions(*e2) || contains_coercions(*e3)
            }
        }
    }
    pub fn succeeds(program: &str) {
        let orig = parse(program);
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(orig);
        println!("\nAfter type inference:\n{}", e);
        let coercions = contains_coercions(e);
        assert!(!coercions);
    }
    pub fn coerces(program: &str) {
        let orig = parse(program);
        println!("\nOriginal program:\n{}", &orig);
        let e = typeinf(orig);
        println!("\nAfter type inference:\n{}", e);
        let coercions = contains_coercions(e);
        assert!(coercions);
    }
    #[test]
    fn an_int() {
        succeeds("5");
    }
    #[test]
    fn fun_app_int() {
        succeeds("(fun x.x) 5");
    }
    #[test]
    fn fun_fun_app_int_int() {
        succeeds("(fun x.fun y.y) 5 8");
    }
    #[test]
    fn local_variables_having_base_types() {
        // we don't worry about elimination forms since ins and outs doesn't
        // even use them!
        succeeds(
            "fun n:int.
                let index = 0 in
                let sum = index in
                // while (index < sum) <- index |> int, sum |> int aren't used (outflows)
                //     index = index + 1 <- index |> int (outflow); int |> index (already there)
                //     sum = sum + index <- index |> int, sum |> int (outflows); int |> sum (already there)
                sum",
        );
    }
    #[test]
    fn identity_twice() {
        succeeds("(fun i.i) ((fun i.i) null)");
    }
    #[test]
    fn identity_public() {
        let e = typeinf(parse("(fun i . i)"));
        match e {
            Exp::Fun(_, Typ::Any, _, Typ::Any) => (),
            _ => panic!(),
        }
    }
    #[test]
    fn call_identity() {
        succeeds("(fun i.i null) (fun x.x)");
    }
    #[test]
    fn i_on_f() {
        // probly (fun i:(any->any)->any->any.(i (fun y:any.y:any))) (fun x:any->any.x:any->any)
        succeeds("(fun i.(i (fun y.y))) (fun x.x)");
    }
    #[test]
    fn i_on_f_twice() {
        succeeds("(fun i.(fun a.a) (i (fun y.y))) (fun x.x)");
    }
    #[test]
    fn broke_migeed() {
        coerces("(fun i.(fun a.fun b.b) (i 5) (i true)) (fun x.x)");
    }
}

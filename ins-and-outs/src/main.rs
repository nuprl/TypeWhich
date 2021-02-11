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
            Exp::Null | Exp::Var(..) => false,
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
        succeeds("(fun i.(fun a.fun b.b) (i null) (i (fun y.y))) (fun x.x)");
    }
}

use super::lexer_l;
use super::parser_y;
use super::syntax::{Exp, Typ};
use std::cell::RefCell;

thread_local!(static NEXT_METAVAR: RefCell<u32> = RefCell::new(0));

pub fn next_metavar_deprecated() -> u32 {
    NEXT_METAVAR.with(|mv| {
        let mut mv = mv.borrow_mut();
        let i = *mv;
        *mv = i + 1;
        i
    })
}

pub fn next_metavar() -> Typ {
    Typ::Metavar(next_metavar_deprecated())
}

/// Parses the input string, producing an `Exp` where very type annotation
/// is set to `Typ::Metavar`. Each `Typ::Metavar` is numbered sequentially,
/// starting with `0`.
pub fn parse(input: impl AsRef<str>) -> Exp {
    let input = input.as_ref();
    let lexerdef = lexer_l::lexerdef();
    let lexer = lexerdef.lexer(input);
    let (res, errs) = parser_y::parse(&lexer);
    if errs.len() == 0 {
        return res.unwrap();
    }
    for err in errs.into_iter() {
        eprintln!("{}", err.pp(&lexer, &|t| parser_y::token_epp(t)));
    }
    panic!("Error parsing expressions");
}

#[cfg(test)]
mod test {
    use super::parse;

    #[test]
    fn test_parse() {
        parse("fun x . x");
    }
}

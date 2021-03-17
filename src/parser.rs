use super::syntax::{Exp, Typ};
use std::cell::RefCell;
use std::collections::hash_set::HashSet;

lrlex::lrlex_mod!("lexer.l"); // effectively mod `lexer_l`
lrpar::lrpar_mod!("parser.y"); // effectively mod `parser_y`

thread_local!(static NEXT_METAVAR: RefCell<u32> = RefCell::new(0));
thread_local!(static PARSER_WARNINGS: RefCell<HashSet<String>> = RefCell::new(HashSet::new()));

pub fn next_metavar() -> Typ {
    Typ::Metavar(inc_metavar())
}

fn inc_metavar() -> u32 {
    NEXT_METAVAR.with(|mv| {
        let mut mv = mv.borrow_mut();
        let i = *mv;
        *mv = i + 1;
        i
    })
}

pub fn parser_warning(msg: impl AsRef<str>) {
    let msg = msg.as_ref().to_string();
    PARSER_WARNINGS.with(|s| {
        let mut s = s.borrow_mut();
        s.insert(msg);
    });
}

/// Parses the input string, producing an `Exp` where very type annotation
/// is set to `Typ::Metavar`. Each `Typ::Metavar` is numbered sequentially,
/// starting with `0`.
pub fn parse(input: impl AsRef<str>) -> Exp {
    let input = input.as_ref();
    let lexerdef = lexer_l::lexerdef();
    let lexer = lexerdef.lexer(input);
    let (res, errs) = parser_y::parse(&lexer);
    PARSER_WARNINGS.with(|ws| {
        for w in ws.replace(HashSet::new()).into_iter() {
            eprintln!("Warning: {}", w);
        }
    });
    if errs.is_empty() {
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

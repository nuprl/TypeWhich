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

pub fn inc_metavar() -> u32 {
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

pub fn show_warnings() {
    PARSER_WARNINGS.with(|ws| {
        for w in ws.replace(HashSet::new()).into_iter() {
            eprintln!("Warning: {}", w);
        }
    });
}

/// Parses the input string, producing an `Exp` where very type annotation
/// is set to `Typ::Metavar`. Each `Typ::Metavar` is numbered sequentially,
/// starting with `0`.
pub fn parse(input: impl AsRef<str>) -> Result<Exp, String> {
    let input = input.as_ref();
    let lexerdef = lexer_l::lexerdef();
    let lexer = lexerdef.lexer(input);
    let (res, errs) = parser_y::parse(&lexer);
    let mut errors = String::new();
    let did_err = errs.is_empty() == false;
    for err in errs.into_iter() {
        errors.push_str(&format!("{}", err.pp(&lexer, &|t| parser_y::token_epp(t))));
    }

    match res {
        Some(Ok(exp)) => if did_err == false { Ok(exp) } else { Err(errors) },
        Some(Err()) | None => Err(errors)
    }
}


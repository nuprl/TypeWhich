use cfgrammar::yacc::YaccKind;
use lrlex::LexerBuilder;
use lrpar::CTParserBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!(r"cargo:rustc-link-search=/usr/local/lib/");
    
    // '.error_on_conflicts(true)' does not print the conflicts that occur, which makes the error
    // hard to debug. Instead, we print the conflicts ourselves below.
    let mut parser_builder = CTParserBuilder::new()
        .yacckind(YaccKind::Grmtools)
        .error_on_conflicts(false);

    let lex_rule_ids_map = parser_builder.process_file_in_src("parser.y")?;

    if let Some(conflicts) = parser_builder.conflicts() {
        println!("{}", conflicts.3.pp(conflicts.0));
        panic!("Found shift-reduce or reduce-reduce conflicts (described above).");
    }

    LexerBuilder::new()
        .rule_ids_map(lex_rule_ids_map)
        .process_file_in_src("lexer.l")?;
    Ok(())
}

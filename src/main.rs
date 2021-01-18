mod syntax;
mod parser;
mod cgen;

lrlex::lrlex_mod!("lexer.l"); // effectively mod `lexer_l`
lrpar::lrpar_mod!("parser.y"); // effectively mod `parser_y`

fn main() {
    println!("Hello, world!");
}

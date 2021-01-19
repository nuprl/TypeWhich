pub mod cgen;
mod parser;
mod syntax;

lrlex::lrlex_mod!("lexer.l"); // effectively mod `lexer_l`
lrpar::lrpar_mod!("parser.y"); // effectively mod `parser_y`

fn main() {
    println!("Hello, world!");
}

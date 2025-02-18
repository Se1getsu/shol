use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod tokens;
pub mod lexer;
lalrpop_mod!(pub shol);

fn main() {
    let input = "%print\n12345\n";
    let mut lexer = lexer::Lexer::new(input);
    let parser = shol::ProgramParser::new();
    let ast = parser.parse(&mut lexer);
    println!("{:?}", ast);
}

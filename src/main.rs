use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod tokens;
pub mod lexer;
lalrpop_mod!(pub shol);

fn main() {
    // コマンドライン引数
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <src_file>", args[0]);
        std::process::exit(1);
    }
    let src_file = &args[1];

    // 入力ファイルの読み込み
    let program = std::fs::read_to_string(src_file)
        .expect("Failed to read program file");

    // 構文解析の都合上, 入力の末尾には改行が必要
    let program = {
        let mut s = program;
        if !s.ends_with('\n') {
            s.push('\n');
        }
        s
    };

    // AST 生成
    let mut lexer = lexer::Lexer::new(&program);
    let parser = shol::ProgramParser::new();
    let ast = parser.parse(&mut lexer)
        .expect("Failed to parse program");
    println!("{{\"AST\":{:?}}}", ast);
}

use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod tokens;
pub mod lexer;
pub mod preprocessor;
pub mod semantics;
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

    // 前処理
    println!("[*] Preprocessing...");
    let program = preprocessor::preprocess(&program);
    println!("{}", program);

    // AST 生成
    println!("[*] AST generating...");
    let mut lexer = lexer::Lexer::new(&program);
    let parser = shol::ProgramParser::new();
    let mut ast = parser.parse(&mut lexer)
        .expect("Failed to parse program");
    println!("{{\"AST\":{:?}}}", ast);

    // 意味解析
    println!("[*] Semantics analyzing...");
    semantics::analyze_program(&mut ast);

    println!("[*] Completed.");
}

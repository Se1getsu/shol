use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod tokens;
pub mod lexer;
pub mod preprocessor;
pub mod semantics;
pub mod code_generator;
lalrpop_mod!(pub shol);

fn main() {
    // コマンドライン引数
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <src_file>", args[0]);
        std::process::exit(1);
    }
    let src_file = &args[1];

    // 出力ファイル名の設定
    let out_rs_file = format!("{}.rs", src_file.trim_end_matches(".shol"));
    let out_exe_file = format!("{}.out", src_file.trim_end_matches(".shol"));

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
    println!("\n[*] Semantics analyzing...");
    semantics::analyze_program(&mut ast);
    println!("{{\"AST\":{:?}}}", ast);

    // 出力ファイルを開く
    let mut out_file = std::fs::File::create(&out_rs_file)
        .expect("Failed to create output file");

    // コード生成
    println!("\n[*] Generating code...");
    code_generator::generate(&mut out_file, &ast, src_file)
        .expect("Failed to write code");

    // コンパイル
    println!("\n[*] Compiling...");
    let status = std::process::Command::new("rustc")
        .arg(&out_rs_file)
        .arg("-o")
        .arg(out_exe_file)
        .status()
        .expect("Failed to execute rustc");
    if !status.success() {
        eprintln!("rustc returned with non-zero exit code");
        std::process::exit(1);
    }

    // 中間生成ファイルの削除
    // std::fs::remove_file(&out_rs_file).expect("Failed to remove intermediate file");

    println!("[*] Completed.");
}

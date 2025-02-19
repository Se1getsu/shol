use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod tokens;
pub mod lexer;
lalrpop_mod!(pub shol);

fn main() {
    // コマンドライン引数
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }
    let input_file = &args[1];

    // 入力ファイルの読み込み
    let input = std::fs::read_to_string(input_file)
        .expect("Failed to read input file");

    // 構文解析の都合上, 入力の末尾には改行が必要
    let input = {
        let mut s = input;
        if !s.ends_with('\n') {
            s.push('\n');
        }
        s
    };

    // AST 生成
    let mut lexer = lexer::Lexer::new(&input);
    let parser = shol::ProgramParser::new();
    let ast = parser.parse(&mut lexer);

    println!("{:?}", ast);
}

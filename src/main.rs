use std::{env, io::Write, fs::File, process::ExitCode};
use lalrpop_util::lalrpop_mod;
use getopts::Options;
use tempfile::Builder;

pub mod ast;
pub mod tokens;
pub mod lexer;
pub mod preprocessor;
pub mod semantics;
pub mod code_generator;
lalrpop_mod!(pub shol);

struct Config {
    /// ソースファイル名
    src_file: String,
    /// 中間生成ファイル名
    shi_file: Option<String>,
    /// 中間生成ファイル名
    rs_file: String,
    /// 実行ファイル名
    exe_file: String,
}

fn main() -> ExitCode {
    // コマンドライン引数のパース
    let args: Vec<String> = env::args().collect();
    let config = parse_args(args);
    if let Err(e) = config {
        return e;
    }
    let config = config.unwrap();

    // 入力ファイルの読み込み
    let program = std::fs::read_to_string(&config.src_file)
        .expect("Failed to read program file");

    // プリプロセス
    println!("[*] Preprocessing...");
    let program = preprocessor::preprocess(&program);

    if let Some(path) = config.shi_file {
        let mut out_file = File::create(&path)
            .expect("Failed to create output file");
        out_file.write_all(program.as_bytes())
            .expect("Failed to write program to file");
    }

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
    let mut out_file = File::create(&config.rs_file)
        .expect("Failed to create output file");

    // コード生成
    println!("\n[*] Generating code...");
    code_generator::generate(&mut out_file, &ast, &config.src_file)
        .expect("Failed to write code");

    // コンパイル
    println!("\n[*] Compiling...");
    if let Err(e) = compile_rs_file(&config.rs_file, &config.exe_file) {
        return e;
    }

    println!("[*] Completed.");
    ExitCode::SUCCESS
}

/// rustc でコンパイル
fn compile_rs_file(rs_file: &str, exe_file: &str) -> Result<(), ExitCode> {
    let status = std::process::Command::new("rustc")
        .arg(rs_file)
        .arg("-o")
        .arg(exe_file)
        .arg("--crate-name") // クレート名に使用できないファイル名の場合に必要
        .arg("shol_generated")
        .status()
        .expect("Failed to execute rustc");

    if !status.success() {
        eprintln!("rustc returned with non-zero exit code");
        Err(ExitCode::FAILURE)
    } else {
        Ok(())
    }
}

/// コマンドライン引数のパース
fn parse_args(args: Vec<String>) -> Result<Config, ExitCode> {
    fn print_usage(program: &str, opts: &Options) {
        let brief = format!("Usage: {} [options] <src_file>", program);
        print!("{}", opts.usage(&brief));
    }

    let program = args[0].clone();

    // コマンドラインオプションの設定
    let mut opts = Options::new();
    opts.optflag("h", "help", "このヘルプメッセージを表示します");
    opts.optflag("S", "save-temps", "中間生成ファイルを保持します");

    // オプションのパース
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}", f);
            return Err(ExitCode::FAILURE);
        }
    };

    if matches.opt_present("h") {
        print_usage(&program, &opts);
        return Err(ExitCode::SUCCESS);
    }

    if matches.free.is_empty() {
        eprintln!("エラー: ソースファイルが指定されていません");
        print_usage(&program, &opts);
        return Err(ExitCode::FAILURE);
    }

    let src_file = matches.free[0].clone();
    let save_temps = matches.opt_present("save-temps");

    // 出力ファイル名の設定
    let rs_file = if save_temps {
        format!("{}.rs", src_file.trim_end_matches(".shol"))
    } else {
        Builder::new()
            .suffix(".rs")
            .tempfile()
            .expect("一時ファイルの作成に失敗しました")
            .path()
            .to_string_lossy()
            .into_owned()
    };

    let shi_file = if save_temps {
        Some(format!("{}.shi", src_file.trim_end_matches(".shol")))
    } else {
        None
    };

    let exe_file = if cfg!(target_os = "windows") {
        format!("{}.exe", src_file.trim_end_matches(".shol"))
    } else {
        src_file.trim_end_matches(".shol").to_string()
    };

    Ok(Config {
        src_file,
        shi_file,
        rs_file,
        exe_file,
    })
}

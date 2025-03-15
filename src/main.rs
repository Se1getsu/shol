use std::{env, io::Write, fs::File, process::ExitCode};
use lalrpop_util::lalrpop_mod;
use getopts::Options;
use tempfile;

pub mod ast;
pub mod tokens;
pub mod lexer;
pub mod parser;
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
    rs_file: OutputFile,
    /// 実行ファイル名
    exe_file: String,
}

enum OutputFile {
    /// 一時ファイル
    Tmp(tempfile::NamedTempFile),
    /// 出力ファイル
    Out(String),
}

impl OutputFile {
    /// ファイルパスを取得
    fn path(&self) -> String {
        match self {
            OutputFile::Tmp(temp) => temp.path().to_string_lossy().into_owned(),
            OutputFile::Out(path) => path.clone(),
        }
    }
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
    let program = match std::fs::read_to_string(&config.src_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("ファイル読み込みエラー: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // プリプロセス
    println!("[*] Preprocessing...");
    let program = preprocessor::preprocess(&program);

    if let Some(path) = config.shi_file {
        let mut out_file = match File::create(&path) {
            Ok(file) => file,
            Err(e) => {
                eprintln!("ファイル作成エラー: {}", e);
                return ExitCode::FAILURE;
            }
        };
        if let Err(e) = out_file.write_all(program.as_bytes()) {
            eprintln!("ファイル書き込みエラー: {}", e);
            return ExitCode::FAILURE;
        }
    }

    // AST 生成
    println!("[*] AST generating...");
    let mut ast = match parser::parse_program(&program) {
        Ok(ast) => ast,
        Err(e) => return e,
    };
    println!("{{\"AST\":{:?}}}", ast);

    // 意味解析
    println!("\n[*] Semantics analyzing...");
    semantics::analyze_program(&mut ast);
    println!("{{\"AST\":{:?}}}", ast);

    // 出力ファイルを開く
    let mut out_file = match File::create(&config.rs_file.path()) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("出力ファイル作成エラー: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // コード生成
    println!("\n[*] Generating code...");
    if let Err(_) = code_generator::generate(&mut out_file, &ast, &config.src_file) {
        // TODO: エラーメッセージ
        return ExitCode::FAILURE;
    }

    // コンパイル
    println!("\n[*] Compiling...");
    if let Err(e) = compile_rs_file(&config.rs_file.path(), &config.exe_file) {
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
        OutputFile::Out(format!("{}.rs", src_file.trim_end_matches(".shol")))
    } else {
        let temp = tempfile::Builder::new()
            .suffix(".rs")
            .tempfile()
            .expect("一時ファイルの作成に失敗しました");
        OutputFile::Tmp(temp)
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

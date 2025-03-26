use std::{env, io::Write, fs::File, process::ExitCode};
use lalrpop_util::lalrpop_mod;
use getopts::Options;
use tempfile;

pub mod ast;
pub mod compile_error;
pub mod logger;
pub mod tokens;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod semantics;
pub mod semantic_error;
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
    exe_file: Option<String>,
    /// ログ出力の有効化
    log_enabled: bool,
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

    // ロガーの初期化
    let logger = logger::Logger::new(config.log_enabled);

    // 入力ファイルの読み込み
    let program = match std::fs::read_to_string(&config.src_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("ファイル読み込みエラー: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // プリプロセス
    log!(logger, "[*] Preprocessing...");
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
    log!(logger, "[*] AST generating...");
    let mut ast = match parser::parse_program(&program) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            return ExitCode::FAILURE;
        }
    };
    log!(logger, "{{\"AST\":{:?}}}", ast);

    // 意味解析
    log!(logger, "\n[*] Semantics analyzing...");
    if let Err(e) = semantics::analyze_program(&mut ast, &logger) {
        let e = e.build_compile_error(&program);
        eprintln!("{}", e);
        return ExitCode::FAILURE;
    }
    log!(logger, "{{\"AST\":{:?}}}", ast);

    // 出力ファイルを開く
    let mut out_file = match File::create(&config.rs_file.path()) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("出力ファイル作成エラー: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // コード生成
    log!(logger, "\n[*] Generating code...");
    if let Err(e) = code_generator::generate(&mut out_file, &ast, &config.src_file) {
        eprintln!("出力ファイル書き込みエラー: {}", e);
        return ExitCode::FAILURE;
    }

    // コンパイル
    if let Some(exe_file) = &config.exe_file {
        log!(logger, "\n[*] Compiling...");
        if let Err(e) = compile_rs_file(&config.rs_file.path(), exe_file) {
            return e;
        }
    }

    log!(logger, "[*] Completed.");
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
    opts.optflag("v", "version", "バージョンを表示します");
    opts.optopt("o", "", "出力ファイル名を指定します", "FILENAME");
    opts.optflag("", "rs", "Rust プログラムにトランスパイルします");
    opts.optflag("S", "save-temps", "中間生成ファイルを保持します");
    opts.optflag("", "log", "コンパイル過程のログを出力します");

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

    if matches.opt_present("v") {
        println!("Shol {}", env!("CARGO_PKG_VERSION"));
        return Err(ExitCode::SUCCESS);
    }

    if matches.free.is_empty() {
        eprintln!("エラー: ソースファイルが指定されていません");
        print_usage(&program, &opts);
        return Err(ExitCode::FAILURE);
    }

    let src_file = matches.free[0].clone();
    let rust_mode = matches.opt_present("rs");
    let save_temps = matches.opt_present("save-temps");
    let log_enabled = matches.opt_present("log");
    let output_file = matches.opt_str("o");

    // 出力ファイル名の設定
    let shi_file = if save_temps {
        Some(format!("{}.shi", src_file.trim_end_matches(".shol")))
    } else {
        None
    };

    let (rs_file, exe_file) =
        if rust_mode {
            let rs_path = match output_file {
                Some(path) => path,
                None => format!("{}.rs", src_file.trim_end_matches(".shol")),
            };
            (OutputFile::Out(rs_path), None)
        } else {
            let rs_file = if save_temps {
                OutputFile::Out(format!("{}.rs", src_file.trim_end_matches(".shol")))
            } else {
                let temp = tempfile::Builder::new()
                    .suffix(".rs")
                    .tempfile()
                    .expect("一時ファイルの作成に失敗しました");
                OutputFile::Tmp(temp)
            };

            let exe_path = match output_file {
                Some(path) => path,
                None => if cfg!(target_os = "windows") {
                    format!("{}.exe", src_file.trim_end_matches(".shol"))
                } else {
                    src_file.trim_end_matches(".shol").to_string()
                },
            };
            (rs_file, Some(exe_path))
        };

    Ok(Config {
        src_file,
        shi_file,
        rs_file,
        exe_file,
        log_enabled,
    })
}

use std::process::ExitCode;
use std::usize;
use lalrpop_util::ParseError;
use crate::shol;
use crate::lexer;
use crate::ast;
use crate::tokens;

// MARK: 構文解析

/// プログラムを構文解析して AST を生成
pub fn parse_program(program: &str) -> Result<Vec<ast::StatementAST>, ExitCode> {
    let mut lexer = lexer::Lexer::new(&program);
    let parser = shol::ProgramParser::new();
    let parse_result = parser.parse(&mut lexer);
    match parse_result {
        Ok(ast) => Ok(ast),
        Err(err) => {
            let compile_error = convert_parse_error(err, &program);
            eprintln!("{}", compile_error.format(&program));
            return Err(ExitCode::FAILURE);
        }
    }
}

// MARK: エラー型

/// コンパイルエラー型
#[derive(Debug)]
    /// 構文解析エラー
struct SyntaxError {
    /// エラーが発生した位置（行）
    line: usize,
    /// エラーが発生した位置（列）
    column: usize,
    /// エラーが発生した位置(長さ)
    length: usize,
    /// エラーメッセージ
    message: String
}

impl SyntaxError {
    /// コンパイルエラーとして出力するエラーメッセージを生成
    pub fn format(&self, source: &str) -> String {
        let line = self.line;
        let column = self.column;
        let length = self.length;
        let line_content = source.lines().nth(line - 1).unwrap_or("");

        // エラー位置を示す矢印を作成
        let mut pointer = String::new();
        for _ in 0..column-1 {
            pointer.push(' ');
        }
        pointer.push_str("\x1b[1;31m");
        for _ in 0..length {
            pointer.push('^');
        }
        pointer.push_str("\x1b[0m");

        // エラーメッセージを整形
        let line = line.to_string();
        format!(
            "{red}構文エラー{reset}: {msg}\n\
            {blue}  -->{reset} {line} 行目 {column} 文字目\n\
            {blue}{space} |\n\
            {line} | {reset}{line_content}\n\
            {blue}{space} | {reset}{pointer}\n",
            red = "\x1b[1;31m",
            blue = "\x1b[1;34m",
            reset = "\x1b[0m",
            msg = self.message,
            space = " ".repeat(line.len()),
        )
    }
}

// MARK: トークンフォーマット

/// 文法ファイルのトークン名を人間が読める形式に変換
fn format_token(token: &str) -> Option<String> {
    let token = &token[1..token.len()-1];
    match token {
        "identf" => Some("識別子".to_string()),
        "double" => Some("doubleリテラル".to_string()),
        "int" => Some("intリテラル".to_string()),
        "intmin" => None,
        "str" => Some("strリテラル".to_string()),
        "true" => Some("boolリテラル".to_string()),
        "false" => None,
        "nl" => Some("行終端".to_string()),
        "#xx" => Some("`#`".to_string()),
        "$xx" => Some("`$`".to_string()),
        _ => Some(format!("`{}`", token)),
    }
}

/// 期待されるトークンのリストを整形
fn format_expected_tokens(tokens: &[String]) -> String {
    if tokens.is_empty() {
        "なし".to_string()
    } else {
        tokens
            .iter()
            .filter_map(|token| format_token(token))
            .collect::<Vec<String>>()
            .join(", ")
    }
}

// MARK: エラー変換

/// LALRPOP のエラーを SyntaxError に変換
fn convert_parse_error(
    error: ParseError<usize, tokens::Token, tokens::LexicalError>, 
    program: &str
) -> SyntaxError {
    match error {
        ParseError::InvalidToken { location } => {
            // どういう時にこのエラーが出るのか不明
            let (line, column) = position_to_line_column(program, location);
            SyntaxError {
                line,
                column,
                length: 1,
                message: "無効なトークンです。(InvalidToken)".to_string()
            }
        },
        ParseError::UnrecognizedEof { location, expected } => {
            let (line, column) = position_to_line_column(program, location);
            SyntaxError {
                line,
                column,
                length: 1,
                message: format!("予期せぬファイル終端です。期待されるトークン: {}", format_expected_tokens(&expected))
            }
        },
        ParseError::UnrecognizedToken { token: (start, token, end), expected } => {
            let (line, column) = position_to_line_column(program, start);
            SyntaxError {
                line,
                column,
                length: end - start,
                message: match token {
                    tokens::Token::NewLine =>
                        format!("予期せぬ行終端です。期待されるトークン: {}", format_expected_tokens(&expected)),
                    _ =>
                        format!("予期せぬトークンです。期待されるトークン: {}", format_expected_tokens(&expected)),
                }
            }
        },
        ParseError::ExtraToken { token: (start, _, end) } => {
            // どういう時にこのエラーが出るのか不明
            let (line, column) = position_to_line_column(program, start);
            SyntaxError {
                line,
                column,
                length: end - start,
                message: format!("無効な文法です。(ExtraToken)")
            }
        },
        ParseError::User { error } =>
            convert_lexical_error(error, program),
    }
}

/// LexicalError を SyntaxError に変換
fn convert_lexical_error(error: tokens::LexicalError, program: &str) -> SyntaxError {
    let (line, column) = position_to_line_column(program, error.range.start);
    let length = error.range.end - error.range.start;
    match error.error_type {
        tokens::LexicalErrorKind::InvalidToken =>
            SyntaxError {
                line, column, length,
                message: "無効なトークンです。".to_string()
            },
        tokens::LexicalErrorKind::InvalidIntegerLiteral =>
            SyntaxError {
                line, column, length,
                message: "整数リテラルを int 型の値にパースできません。".to_string()
            },
        tokens::LexicalErrorKind::InvalidFloatLiteral =>
            SyntaxError {
                line, column, length,
                message: "浮動小数点リテラルを double 型の値にパースできません。".to_string()
            },
        tokens::LexicalErrorKind::InvalidStringEscape { message, position } =>
            SyntaxError {
                line,
                column: column + position.start + 1,
                length: position.end - position.start,
                message: format!("{}", message)
            },
    }
}

/// 行と列の位置に変換
fn position_to_line_column(program: &str, pos: usize) -> (usize, usize) {
    program
        .chars()
        .take(pos)
        .fold((1, 1), |(line, col), c| {
            if c == '\n' {
                (line + 1, 1)
            } else {
                (line, col + 1)
            }
        })
}

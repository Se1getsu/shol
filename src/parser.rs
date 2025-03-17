use lalrpop_util::ParseError;
use crate::shol;
use crate::lexer;
use crate::ast;
use crate::tokens;
use crate::compile_error::{CompileError, CompileErrorBuilder, ErrorKind::SyntaxError};

// MARK: 構文解析

/// プログラムを構文解析して AST を生成
pub fn parse_program(source: &str) -> Result<Vec<ast::StatementAST>, CompileError> {
    let mut lexer = lexer::Lexer::new(&source);
    let parser = shol::ProgramParser::new();
    let parse_result = parser.parse(&mut lexer);
    match parse_result {
        Ok(ast) => Ok(ast),
        Err(err) => {
            let compile_error = convert_parse_error(err, &source);
            return Err(compile_error);
        }
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
    source: &str
) -> CompileError {
    match error {
        ParseError::InvalidToken { location } => {
            // どういう時にこのエラーが出るのか不明
            CompileErrorBuilder::new(source, SyntaxError)
                .header("無効なトークンです。(InvalidToken)", location)
                .location_pointer(&(location..location+1))
                .build()
        },
        ParseError::UnrecognizedEof { location, expected } => {
            let message = format!(
                "予期せぬファイル終端です。期待されるトークン: {}",
                format_expected_tokens(&expected)
            );
            CompileErrorBuilder::new(source, SyntaxError)
                .header(&message, location)
                .location_pointer(&(location..location+1))
                .build()
        },
        ParseError::UnrecognizedToken { token: (start, token, end), expected } => {
            let message = match token {
                tokens::Token::NewLine =>
                    format!("予期せぬ行終端です。期待されるトークン: {}", format_expected_tokens(&expected)),
                _ =>
                    format!("予期せぬトークンです。期待されるトークン: {}", format_expected_tokens(&expected)),
            };
            CompileErrorBuilder::new(source, SyntaxError)
                .header(&message, start)
                .location_pointer(&(start..end))
                .build()
        },
        ParseError::ExtraToken { token: (start, _, end) } => {
            // どういう時にこのエラーが出るのか不明
            CompileErrorBuilder::new(source, SyntaxError)
                .header("無効な文法です。(ExtraToken)", start)
                .location_pointer(&(start..end))
                .build()
        },
        ParseError::User { error } =>
            convert_lexical_error(error, source),
    }
}

/// LexicalError を CompileError に変換
fn convert_lexical_error(error: tokens::LexicalError, source: &str) -> CompileError {
    match error.error_type {
        tokens::LexicalErrorKind::InvalidToken => {
            let message = if let Some('"') = source.chars().nth(error.location.start) {
                "文字列リテラルが閉じられていません。".to_string()
            } else {
                "無効なトークンです。".to_string()
            };
            CompileErrorBuilder::new(source, SyntaxError)
                .header(&message, error.location.start)
                .location_pointer(&error.location)
                .build()
        }
        tokens::LexicalErrorKind::InvalidIntegerLiteral => {
            CompileErrorBuilder::new(source, SyntaxError)
                .header("整数リテラルを int 型の値にパースできません。", error.location.start)
                .location_pointer(&error.location)
                .build()
        }
        tokens::LexicalErrorKind::InvalidFloatLiteral => {
            CompileErrorBuilder::new(source, SyntaxError)
                .header("浮動小数点リテラルを double 型の値にパースできません。", error.location.start)
                .location_pointer(&error.location)
                .build()
        }
        tokens::LexicalErrorKind::InvalidStringEscape { message, position } => {
            let location = error.location.start + position.start .. error.location.start + position.end;
            CompileErrorBuilder::new(source, SyntaxError)
                .header(&format!("{}", message), location.start)
                .location_pointer(&location)
                .build()
        }
    }
}

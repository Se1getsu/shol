use std::num::ParseIntError;
use logos::Logos;
use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(_: ParseIntError) -> Self {
        LexicalError::InvalidToken
    }
}

/// クォートで囲まれた文字列リテラルをデコードする
fn decode_string(input: &str) -> String {
    let without_quotes = &input[1..input.len()-1];
    let mut result = String::with_capacity(without_quotes.len());
    let mut chars = without_quotes.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    _ => {
                        result.push('\\');
                        result.push(next);
                    }
                }
            }
        } else {
            result.push(c);
        }
    }
    result
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexicalError)]
#[logos(skip r"[ \t]+")]
pub enum Token {
    // 識別子とリテラル
    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("0|[1-9][0-9]*", |lex| lex.slice().parse())]
    IntegerLiteral(i32),
    #[regex(r#""([^"\\\x00-\x1F]|\\.)*""#, |lex| decode_string(lex.slice()))]
    StringLiteral(String),
    #[token("true")]
    True,
    #[token("false")]
    False,

    // 式に使われる記号
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,

    // 構文に使われる記号
    #[token(".")]
    Dot,
    #[token("|")]
    Pipe,
    #[token(",")]
    Comma,
    #[regex(r"#((\p{XID_Start}|_)\p{XID_Continue}*)?", // #a -> Some("a"), # -> None
        |lex| lex.slice().strip_prefix('#').filter(|s| !s.is_empty()).map(|s| s.to_string()))]
    Destination(Option<String>),
    #[regex(r"\$((\p{XID_Start}|_)\p{XID_Continue}*)?", // $a -> "a", $ -> ""
        |lex| lex.slice().strip_prefix('$').unwrap().to_string())]
    Capture(String),

    // 改行
    #[regex(r"\n|\r\n|\r|\f", |lex| lex.slice().to_string())]
    NewLine(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

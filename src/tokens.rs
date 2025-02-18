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

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexicalError)]
pub enum Token {
    // 数値と識別子
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("[1-9][0-9]*", |lex| lex.slice().parse())]
    Integer(i32),

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
    #[token("#")]
    Hash,
    #[token("$")]
    Dollar,

    // スペースや改行
    #[regex(r"[ \t]", |lex| lex.slice().to_string())]
    Space(String),
    #[regex(r"[\r\n]", |lex| lex.slice().to_string())]
    NewLine(String),

    // 任意の文字をトークン化
    #[regex(r".", |lex| lex.slice().to_string(), priority = 1)]
    Other(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

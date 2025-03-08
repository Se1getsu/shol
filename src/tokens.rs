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
                    'a' => result.push('\x07'),
                    'b' => result.push('\x08'),
                    't' => result.push('\t'),
                    'n' => result.push('\n'),
                    'v' => result.push('\x0b'),
                    'f' => result.push('\x0c'),
                    'r' => result.push('\r'),
                    'e' => result.push('\x1b'),
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'x' => {
                        let hex = chars.by_ref().take(2).collect::<String>();
                        let code = match (hex.len() == 2, u8::from_str_radix(&hex, 16)) {
                            (true, Ok(code)) => code,
                            _ => panic!("Unicode エスケープ \\xXX をデコードできません: \\x{}", hex),
                        };
                        result.push(code as char);
                    },
                    'u' => {
                        let hex = chars.by_ref().take(4).collect::<String>();
                        let code = match (hex.len() == 4, u16::from_str_radix(&hex, 16)) {
                            (true, Ok(code)) => code,
                            _ => panic!("Unicode エスケープ \\uXXXX をデコードできません: \\u{}", hex),
                        };
                        let c = match char::from_u32(code as u32) {
                            Some(c) => c,
                            None => panic!("Unicode エスケープ \\u{} は不正な文字です。", hex),
                        };
                        result.push(c);
                    },
                    'U' => {
                        let hex = chars.by_ref().take(8).collect::<String>();
                        let code = match (hex.len() == 8, u32::from_str_radix(&hex, 16)) {
                            (true, Ok(code)) => code,
                            _ => panic!("Unicode エスケープ \\UXXXXXXXX をデコードできません: \\U{}", hex),
                        };
                        let c = match char::from_u32(code) {
                            Some(c) => c,
                            None => panic!("Unicode エスケープ \\U{} は不正な文字です。", hex),
                        };
                        result.push(c);
                    },
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
    // 数値と識別子
    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("0|[1-9][0-9]*", |lex| lex.slice().parse())]
    IntegerLiteral(i32),
    #[regex(r#""([^"\\\x00-\x1F]|\\.)*""#, |lex| decode_string(lex.slice()))]
    StringLiteral(String),

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

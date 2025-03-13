use std::num::{ ParseIntError, ParseFloatError };
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

impl From<ParseFloatError> for LexicalError {
    fn from(_: ParseFloatError) -> Self {
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
                    '0' => result.push('\0'),
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
#[logos(subpattern fractional = r"[0-9]+\.|[0-9]*\.[0-9]+")]
#[logos(subpattern exponent = r"[eE][+-]?[0-9]+")]
#[logos(subpattern double_literal = r"(?&fractional)(?&exponent)?|[0-9]+(?&exponent)")]
pub enum Token {
    // 識別子とリテラル
    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"(?&double_literal)", |lex| lex.slice().parse())]
    DoubleLiteral(f64),
    #[regex("0|[1-9][0-9]*", |lex| lex.slice().parse())]
    IntegerLiteral(i32),
    #[token("-2147483648", |_| -2147483648)] // - と 2147483648 を別トークンにするとオーバフローするため特別扱い
    IntegerMin(i32),
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
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
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
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token(">>")]
    BitShiftRight,
    #[token("<<")]
    BitShiftLeft,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,
    #[token("!")]
    LogicalNot,
    #[token(":int")]
    AsInt,
    #[token(":double")]
    AsDouble,
    #[token(":str")]
    AsStr,
    #[token(":bool")]
    AsBool,

    // 構文に使われる記号
    #[token(".")]
    Dot,
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

// MARK: - Tests

#[test]
fn test_tokenize_double_literal() {
    let tokens = Token::lexer("123.456")
        .collect::<Vec<Result<Token, LexicalError>>>();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].as_ref().unwrap(), &Token::DoubleLiteral(123.456));
}

#[test]
fn test_tokenize_double_literal_variants() {
    let test_cases = vec![
        ("0.123", 0.123),
        (".456", 0.456),
        ("123.", 123.0),
        ("00.", 0.0),
        ("1e5", 1e5),
        ("0e0", 0.0),
        ("001.00e+04", 1.00e+04),
        ("1.23e-12", 1.23e-12),
        (".00e-23", 0.00e-23),
        ("12.e-23", 12.0e-23),
        ("0.e+18", 0.0),
    ];
    for (input, expected) in test_cases {
        let tokens = Token::lexer(input)
            .collect::<Vec<Result<Token, LexicalError>>>();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].as_ref().unwrap(), &Token::DoubleLiteral(expected));
    }
}

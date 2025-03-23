use std::num::{ ParseIntError, ParseFloatError };
use std::ops::Range;
use logos::{ Lexer, Logos };
use std::fmt;

// MARK: LexicalError

#[derive(Debug, Clone, PartialEq)]
pub struct LexicalError {
    /// エラーが発生した範囲 (Lexer が後から設定する)
    pub location: Range<usize>,
    /// エラーの種類
    pub error_type: LexicalErrorKind,
}

/// エラーの種類
#[derive(Debug, Clone, PartialEq)]
pub enum LexicalErrorKind {
    /// 無効なトークン
    InvalidToken,
    /// 整数パースエラー
    InvalidIntegerLiteral,
    /// 浮動小数点パースエラー
    InvalidFloatLiteral,
    /// 文字列リテラルのエスケープエラー
    InvalidStringEscape {
        /// エラーメッセージ
        message: String,
        /// 文字列中のエスケープシーケンスの位置
        position: Range<usize>,
    },
    /// ブロックコメントが閉じていない
    UnclosedBlockComment {
        /// ブロックコメントの開始位置
        block_comment_start: Range<usize>,
        /// 最後に終端したブロックコメントの開始位置と終了位置
        last_terminated: Option<(Range<usize>, Range<usize>)>,
    }
}

// MARK: LexicalError の生成ルール

impl Default for LexicalError {
    fn default() -> Self {
        LexicalError {
            location: 0..0,
            error_type: LexicalErrorKind::InvalidToken,
        }
    }
}
impl From<ParseIntError> for LexicalError {
    fn from(_: ParseIntError) -> Self {
        LexicalError {
            location: 0..0,
            error_type: LexicalErrorKind::InvalidIntegerLiteral,
        }
    }
}
impl From<ParseFloatError> for LexicalError {
    fn from(_: ParseFloatError) -> Self {
        LexicalError {
            location: 0..0,
            error_type: LexicalErrorKind::InvalidFloatLiteral,
        }
    }
}

// MARK: トークン定義

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexicalError)]
#[logos(skip r"[ \t]+")]
#[logos(skip r"//.*")]
#[logos(subpattern fractional = r"[0-9]+\.|[0-9]*\.[0-9]+")]
#[logos(subpattern exponent = r"[eE][+-]?[0-9]+")]
// r"[0-9]+\." の表記は `5.abs` を `5.`, `abs` にしてしまうので, 文法の方で処理
#[logos(subpattern double_literal = r"[0-9]*\.[0-9]+|(?&fractional)(?&exponent)|[0-9]+(?&exponent)")]
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
    #[regex(r"'((\p{XID_Start}|_)\p{XID_Continue}*)?", // 'a -> "a", ' -> ""
        |lex| lex.slice().strip_prefix('\'').unwrap().to_string())]
    SymbolLiteral(String),
    #[token("true")]
    True,
    #[token("false")]
    False,

    // 括弧類
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    // 比較演算子
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

    // 算術演算子
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

    // ビット演算子
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

    // 論理演算子
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,
    #[token("!")]
    LogicalNot,

    // 文字列スライス
    #[token(":")]
    Colon,

    // 型ヒント演算子
    #[regex(r":i(nt)?")]
    AsInt,
    #[regex(r":d(ouble)?")]
    AsDouble,
    #[regex(r":s(tr)?")]
    AsStr,
    #[regex(r":b(ool)?")]
    AsBool,
    #[regex(r":sym(bol)?")]
    AsSymbol,

    // 型変換
    #[regex(r"\.i(nt)?")]
    ToInt,
    #[regex(r"\.d(ouble)?")]
    ToDouble,
    #[regex(r"\.s(tr)?")]
    ToString,

    // ユーティリティ演算子
    #[token(".ceil")]
    UtilCeil,
    #[token(".floor")]
    UtilFloor,
    #[token(".round")]
    UtilRound,
    #[token(".abs")]
    UtilAbs,
    #[token(".ord")]
    UtilOrd,
    #[token(".chr")]
    UtilChr,
    #[token(".len")]
    UtilLen,

    // 構文に使われる記号
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("@debug")]
    AtDebug,
    #[token("@sleep")]
    AtSleep,
    #[regex(r"#((\p{XID_Start}|_)\p{XID_Continue}*)?", // #a -> Some("a"), # -> None
        |lex| lex.slice().strip_prefix('#').filter(|s| !s.is_empty()).map(|s| s.to_string()))]
    Destination(Option<String>),
    #[regex(r"\$((\p{XID_Start}|_)\p{XID_Continue}*)?", // $a -> "a", $ -> ""
        |lex| lex.slice().strip_prefix('$').unwrap().to_string())]
    Capture(String),

    // 改行
    #[regex(r"\n|\r\n|\r|\f")]
    NewLine,

    // ブロックコメント
    #[regex(r"/\*", parse_nested_comment)]
    BlockCommentStart,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// MARK: ネストされたブロックコメントのパース

fn parse_nested_comment<'a>(lex: &mut Lexer<'a, Token>) -> Result<(), LexicalError> {
    // ブロックコメントの開始位置からのオフセット
    let mut offset = 2;
    // ブロックコメントの開始位置を保持
    let mut nest_start_points = vec![0..2];
    // 最後に終端したブロックコメントの開始位置と終了位置を保持
    let mut last_terminated = None;
    // 最初の `/*` 以降のプログラム
    let remainder = lex.remainder();

    while !nest_start_points.is_empty() {
        let slice = &remainder[offset-2..];

        // EOF
        if slice.is_empty() {
            return Err(LexicalError {
                location: 0..0,
                error_type: LexicalErrorKind::UnclosedBlockComment {
                    block_comment_start: 0..2,
                    last_terminated,
                },
            });
        }
        // 新たなブロックコメントの開始
        else if slice.starts_with("/*") {
            nest_start_points.push(offset..offset+2);
            offset += 2;
        }
        // ブロックコメントの終了
        else if slice.starts_with("*/") {
            last_terminated = Some((nest_start_points.pop().unwrap(), offset..offset+2));
            offset += 2;
        }
        // その他の文字
        else {
            offset += 1;
        }
    }

    lex.bump(offset-2);
    Ok(())
}

// MARK: 文字列リテラルのデコード

/// クォートで囲まれた文字列リテラルをデコードする
fn decode_string(input: &str) -> Result<String, LexicalError> {
    let mut result = String::with_capacity(input.len());
    let mut chars = input
        .chars()
        .enumerate()
        .skip(1).take(input.chars().count() - 2); // 両端のクォートを除去

    while let Some((start, c)) = chars.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }

        let (_, next) = {
            let next = chars.next();
            if next.is_none() {
                result.push(c);
                break;
            }
            next.unwrap()
        };
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
                let hex: String = chars.by_ref().take(2).map(|(_, c)| c).collect();
                let code = match (hex.len() == 2, u8::from_str_radix(&hex, 16)) {
                    (true, Ok(code)) => code,
                    _ => return Err(LexicalError {
                        location: 0..0,
                        error_type: LexicalErrorKind::InvalidStringEscape {
                            message: format!("Unicode エスケープ \\xXX をデコードできません: \\x{}", hex),
                            position: start .. start + "\\x".len() + hex.len(),
                        },
                    }),
                };
                result.push(code as char);
            },
            'u' => {
                let hex = chars.by_ref().take(4).map(|(_, c)| c).collect::<String>();
                let code = match (hex.len() == 4, u16::from_str_radix(&hex, 16)) {
                    (true, Ok(code)) => code,
                    _ => return Err(LexicalError {
                        location: 0..0,
                        error_type: LexicalErrorKind::InvalidStringEscape {
                            message: format!("Unicode エスケープ \\uXXXX をデコードできません: \\u{}", hex),
                            position: start .. start + "\\u".len() + hex.len(),
                        },
                    }),
                };
                let c = match char::from_u32(code as u32) {
                    Some(c) => c,
                    None => return Err(LexicalError {
                        location: 0..0,
                        error_type: LexicalErrorKind::InvalidStringEscape {
                            message: format!("Unicode エスケープ \\u{} は不正な文字です。", hex),
                            position: start .. start + "\\u".len() + hex.len(),
                        },
                    }),
                };
                result.push(c);
            },
            'U' => {
                let hex = chars.by_ref().take(8).map(|(_, c)| c).collect::<String>();
                let code = match (hex.len() == 8, u32::from_str_radix(&hex, 16)) {
                    (true, Ok(code)) => code,
                    _ => return Err(LexicalError {
                        location: 0..0,
                        error_type: LexicalErrorKind::InvalidStringEscape {
                            message: format!("Unicode エスケープ \\UXXXXXXXX をデコードできません: \\U{}", hex),
                            position: start .. start + "\\U".len() + hex.len(),
                        },
                    }),
                };
                let c = match char::from_u32(code) {
                    Some(c) => c,
                    None => return Err(LexicalError {
                        location: 0..0,
                        error_type: LexicalErrorKind::InvalidStringEscape {
                            message: format!("Unicode エスケープ \\U{} は不正な文字です。", hex),
                            position: start .. start + "\\U".len() + hex.len(),
                        },
                    }),
                };
                result.push(c);
            },
            _ => {
                result.push('\\');
                result.push(next);
            }
        }
    } // while let Some(c) = chars.next()
    Ok(result)
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

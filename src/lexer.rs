use logos::{Logos, SpannedIter};

use crate::tokens::{Token, LexicalError};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { token_stream: Token::lexer(input).spanned() }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| {
            // デバッグ出力
            println!("token {}-{}: {:?}", span.start, span.end, token);
            // トークンがエラーの場合も、ここで ? によりエラーを伝播
            Ok((span.start, token?, span.end))
        })
    }
}

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
        loop {
            match self.token_stream.next() {
                Some((Ok(Token::BlockCommentStart), _)) => {
                    continue;
                }
                Some((Ok(token), span)) => {
                    return Some(Ok((span.start, token, span.end)));
                }
                Some((Err(mut error), span)) => {
                    error.location = span; // エラー発生位置の情報を付加
                    return Some(Err(error));
                }
                None => {
                    return None;
                }
            }
        }
    }
}

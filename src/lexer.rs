use crate::tokens::{LexicalError, Token};
use logos::Logos;

/// A tuple of (start, token, end)
pub type SpannedToken = (usize, Token, usize);

pub struct Lexer<'input> {
    inner: logos::SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            inner: Token::lexer(source).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = (usize, Result<Token, LexicalError>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(tok, span)| match tok {
            Ok(token) => (span.start, Ok(token), span.end),
            Err(_) => (span.start, Err(LexicalError::InvalidToken), span.end),
        })
    }
}

use crate::tokens::Token;
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
    type Item = (usize, Token, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(tok, span)| match tok {
            Ok(token) => (span.start, token, span.end),
            Err(_) => (span.start, Token::Error, span.end),
        })
    }
}

use std::fmt;
use std::num::{ParseIntError, ParseFloatError};
logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    InvalidFloat(ParseFloatError),
    #[default]
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

impl From<ParseFloatError> for LExicalError {
    fn from(err: ParseFloatError) -> Self {
        LexicalError::InvalidFloat(err)
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?", error = LexicalError)]
pub enum Token {
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("impl")]
    Impl,
    #[token("for")]
    For,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("if")]
    If,
    #[token("else"]
    Else,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("match")]
    Match,
    #[token("Some")]
    Some,
    #[token("None")]
    None,
    #[token("Ok")]
    Ok,
    #[token("Err")]
    Err,
    #[token("use")]
    Use,
    #[token("in")]
    In,
    #[token("import")]
    Import,
    #[token("#derive")]
    Derive,
    #[token("trait"]
    Trait,
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token("_")]
    Underscore,
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("++")]
    PlusPlus,
    #[token("|>")]
    PipeForward,
    #[arrow("->"]
    Arrow,
    #[token("=>")
    FatArrow,
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[regex(r"[0-9]+\\.[0-9]+", |lex| lex.slice().to_string())]
    Int(i64),
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Float(f64),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().parse::<f64>().ok())]
    Str(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[token("true", |lex| lex.slice().parse::<Bool>())]
    True,
    #[token("false", |lex| lex.slice().parse::<Bool>())]
    False,
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

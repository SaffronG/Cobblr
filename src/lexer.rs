// src/token.rs
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("impl")]
    Impl,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
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
    #[token("import")]
    Import,
    #[token("#derive")]
    Derive,

    // Symbols
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
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
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
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

    // Literals
    #[regex(r"[0-9]+\.[0-9]+")]
    Float(f64),
    #[regex(r"[0-9]+")]
    Int(i64),
    #[regex(r#""([^"\\]|\\.)*""#)]
    Str(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(String),

    // Logos requires this for errors
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

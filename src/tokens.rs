use logos::Logos;
use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(std::num::ParseIntError),
    InvalidFloat(std::num::ParseFloatError),
    #[default]
    InvalidToken,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?", skip r"//.*", error = LexicalError)]
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
    #[token("else")]
    Else,
    #[token("break")]
    Break,
    #[token("end")]
    End,
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
    #[token("trait")]
    Trait,
    #[token("as")]
    As,
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
    #[token("|")]
    Pipe,
    #[token("|>")]
    PipeForward,
    #[token("->")]
    Arrow,
    #[token("<-")]
    LArrow,
    #[token("=>")]
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
    #[token("^")]
    Carat,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().expect("Failed to parse type"), priority = 1)]
    Float64(f64),
    
    #[regex(r"[0-9]+\.[0-9]", |lex| lex.slice().parse::<f32>().expect("Failed to parse type"), priority= 2)]
    Float32(f32),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().expect("Failed to parse type") , priority = 1)]
    Int64(i64),

    #[regex(r"[0-9]+", |lex|  lex.slice().parse::<i32>().expect("Failed to parse type"), priority = 2)]
    Int32(i32),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string(), priority = 1)]
    Str(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

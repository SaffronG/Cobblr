use logos::Logos;

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'input> {
    // ----------------------
    // Literals
    // ----------------------
    #[regex(r"[0-9]+", |lex| lex.slice())]
    NumSymbol(&'input str),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice())]
    FloatSymbol(&'input str),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| &lex.slice()[1..lex.slice().len()-1])]
    StringSymbol(&'input str),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 2)]
    Identifier(&'input str),

    // ----------------------
    // Multi-char Operators (must come before single-char)
    // ----------------------
    #[token("==")]
    EqEq,

    #[token("!=")]
    NotEq,

    #[token("++")]
    PlusPlus,

    #[token("<=")]
    LtEq,

    #[token(">=")]
    GtEq,

    #[token("->")]
    Arrow,

    #[token("::")]
    ColonColon,

    // ----------------------
    // Single-char Operators
    // ----------------------
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("!")]
    Bang,

    #[token(".")]
    Dot,

    #[token("|")]
    Pipe,

    #[token("&")]
    Ampersand,

    #[token("_", priority = 3)]
    Underscore,

    #[token("=")]
    Eq,

    // ----------------------
    // Delimiters
    // ----------------------
    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,

    // ----------------------
    // Keywords
    // ----------------------
    #[token("fn")]
    Fn,

    #[token("async")]
    Async,

    #[token("struct")]
    Struct,

    #[token("let")]
    Let,

    #[token("mut")]
    Mut,

    #[token("impl")]
    Impl,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("loop")]
    Loop,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("match")]
    Match,

    #[token("Some")]
    Some,

    #[token("Ok")]
    Ok,

    #[token("Err")]
    Err,

    #[token("None")]
    None,

    #[token("#derive")]
    Derive,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("trait")]
    Trait,

    #[token("enum")]
    Enum,

    #[token("use")]
    Use,

    #[token("import")]
    Import,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("in")]
    In,
}

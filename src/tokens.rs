logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+", skip r"#.*\n?", error = LexicalError)]
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

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

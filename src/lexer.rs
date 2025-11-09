use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    /* ---------- Keywords ---------- */
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("return")]
    Return,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("class")]
    Class,
    #[token("struct")]
    Struct,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("enum")]
    Enum,
    #[token("use")]
    Use,
    #[token("import")]
    Import,
    #[token("true")]
    True,
    #[token("false")]
    False,

    /* ---------- Derive & Attributes ---------- */
    #[token("#derive")]
    Derive,
    #[token("#")]
    Hash,

    /* ---------- Identifiers & Literals ---------- */
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse())]
    Number(f64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let slice = lex.slice();
        slice[1..slice.len()-1].to_string()
    })]
    String(String),

    /* ---------- Operators ---------- */
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEq,
    #[token(">=")]
    GreaterEq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("=")]
    Eq,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("::")]
    ColonColon,
    #[token(":")]
    Colon,
    #[token("|>")]
    PipeFwd,
    #[token("|")]
    Pipe,
    #[token("++")]
    Append,
    #[token("..")]
    Spread,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("`")]
    Tick,
    #[token("~")]
    Tilde,
    #[token("%")]
    Modulo,
    #[token("@")]
    At,

    /* ---------- Binary Operators ---------- */
    #[token("&")]
    And,
    #[token("!&")]
    Nand,
    #[token("!|")]
    Nor,
    #[token("^")]
    Xor,
    #[token("!")]
    Not,

    /* ---------- Punctuation ---------- */
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    /* ---------- Attributes ---------- */
    #[regex(r"#\[[^\]]*\]")]
    Attribute,

    /* ---------- Comments ---------- */
    #[regex(r"//[^\n]*", logos::skip)] // skip single-line comments
    #[regex(r"/\*([^*]|\*+[^*/])*\*/", logos::skip)] // skip block comments

    /* ---------- Whitespace ---------- */
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    /* ---------- Unknown ---------- */
    #[error]
    Error,
}

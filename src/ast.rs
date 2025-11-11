#[derive(Debug, Clone)]
pub struct Program(pub Vec<Decl>);

// ---------- Declarations ----------
#[derive(Debug, Clone)]
pub enum Decl {
    Function(String, Vec<String>, Block),
    AsyncFunction(String, Vec<String>, Block),
    Struct(String, Vec<StructMember>),
    DerivedStruct(Vec<String>, Box<Decl>),
    Impl {
        target: String,
        items: Vec<ImplItem>,
    },
    ImplTrait {
        trait_name: String,
        target: String,
        items: Vec<ImplItem>,
    },
    Import(Path),
    Let(Stmt),
}

// ---------- Paths ----------
#[derive(Debug, Clone)]
pub enum Path {
    Single(String),
    Nested(Box<Path>, String),
}

// ---------- Struct Members ----------
#[derive(Debug, Clone)]
pub enum StructMember {
    Field(String, TypeExpr),
    FieldDefault(String, TypeExpr, Expr),
    Method(Decl),
}

// ---------- Implementations ----------
#[derive(Debug, Clone)]
pub enum ImplItem {
    Function(Decl),
    Const(String, Expr),
}

// ---------- Blocks ----------
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub implicit_return: Option<Expr>,
}

// ---------- Statements ----------
#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        mutable: bool,
        pattern: Pattern,
        ty: Option<TypeExpr>,
        value: Expr,
    },
    While {
        cond: Expr,
        body: Block,
    },
    Loop {
        body: Block,
    },
    For {
        pattern: Pattern,
        iter: Expr,
        body: Block,
    },
    Break,
    Continue,
    Return(Expr),
    Expr(Expr),
}

// ---------- Patterns ----------
#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    NumberInt(i64),
    NumberFloat(f64),
    String(String),
    Wildcard,
    Some(Box<Pattern>),
    None,
    Ok(Box<Pattern>),
    Err(Box<Pattern>),
    Tuple(Vec<Pattern>),
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
}

// ---------- Expressions ----------
#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    NumberInt(i64),
    NumberFloat(f64),
    String(String),
    Bool(bool),
    Call(String, Vec<Expr>),
    FunctionCall(Box<Expr>, Vec<Expr>),
    Some(Box<Expr>),
    None,
    Ok(Box<Expr>),
    Err(Box<Expr>),
    Binary(String, Box<Expr>, Box<Expr>),
    Unary(String, Box<Expr>),
    Append(Box<Expr>, Box<Expr>),
    PipeForward(Box<Expr>, Box<Expr>),
    PostfixInc(Box<Expr>),
    Map(Box<Expr>, Box<Expr>),    // e.g., iter.map(...)
    Filter(Box<Expr>, Box<Expr>), // e.g., iter.filter(...)
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Grouped(Box<Expr>),
    Block(Block),
    Spread(String),
}

// ---------- Match Arms ----------
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

// ---------- Type Expressions ----------
#[derive(Debug, Clone)]
pub enum TypeExpr {
    Simple(String),
    Scoped(String, String),
}

// ---------- Helpers ----------
pub type Identifier = String;

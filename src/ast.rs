#[derive(Debug, Clone)]
pub struct Program(pub Vec<Decl>);

#[derive(Debug, Clone)]
pub enum Decl {
    Function(String, Vec<String>, Block),
    AsyncFunction(String, Vec<String>, Block),
    Struct(String, Vec<StructMember>),
    Enum(String, Vec<(String, Option<Vec<TypeExpr>>)>),
    Trait(String, Vec<TraitMethod>),
    Impl(String, Vec<Decl>),
    Let(String, Expr),
    LetMut(String, Expr),
    Import(Path),
    DerivedStruct(Vec<String>, Box<Decl>),
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub name: String,
    pub ty: TypeExpr,
    pub init: Option<Expr>,
}

impl StructMember {
    pub fn Field(name: String, ty: TypeExpr) -> Self {
        StructMember {
            name,
            ty,
            init: None,
        }
    }

    pub fn FieldInit(name: String, ty: TypeExpr, init: Expr) -> Self {
        StructMember {
            name,
            ty,
            init: Some(init),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<String>,
}

impl TraitMethod {
    pub fn Sig(name: String, params: Vec<String>) -> Self {
        TraitMethod { name, params }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub implicit_return: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    LetDecl(Decl),
    ExprStmt(Expr),
    Return(Expr),
    If(IfStmt),
    While(Expr, Block),
    For(String, Expr, Block),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Box<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Number(Number),
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Binary operations
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    // Comparison
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Var(String),
    Number(Number),
    Wildcard,
    Some(Box<Pattern>),
    None,
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Int64,
    Float64,
    Bool,
    String,
    Custom(String),
    Generic(String, Vec<TypeExpr>),
}

pub type Path = PathExpr;

#[derive(Debug, Clone)]
pub enum PathExpr {
    Single(String),
    Nested(Box<PathExpr>, String),
}

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Decl>);

#[derive(Debug, Clone)]
pub enum Decl {
    Function(String, Vec<Param>, Option<TypeExpr>, Block),
    AsyncFunction(String, Vec<Param>, Option<TypeExpr>, Block),
    Struct(String, Vec<StructMember>),
    Enum(String, Vec<(String, Option<Vec<TypeExpr>>)>),
    Trait(String, Vec<TraitMethod>),
    Impl(String, Vec<Decl>),
    ImplTrait(String, String, Vec<Decl>),
    Let(String, Option<TypeExpr>, Expr),
    LetMut(String, Option<TypeExpr>, Expr),
    Import(Path),
    DerivedStruct(Vec<String>, Box<Decl>),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: TypeExpr,
    pub is_mut: bool,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub name: String,
    pub ty: TypeExpr,
    pub init: Option<Expr>,
}

impl StructMember {
    pub fn field(name: String, ty: TypeExpr) -> Self {
        StructMember {
            name,
            ty,
            init: None,
        }
    }

    pub fn field_init(name: String, ty: TypeExpr, init: Expr) -> Self {
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
    pub params: Vec<Param>,
}

impl TraitMethod {
    pub fn sig(name: String, params: Vec<Param>) -> Self {
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
    Loop(Block),
    Break,
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
    NumberInt(i64),
    NumberFloat(f64),
    String(String),
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Call(Box<Expr>, Vec<Expr>),
    AssocCall(String, String, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    StructLit(String, Vec<(String, Expr)>),
    Closure(Vec<Param>, Box<Expr>),

    // Postfix accesor operations
    Index(Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),

    // Binary infix operations
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    // Binary operations
    Concat(Box<Expr>, Box<Expr>),

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
    Variant(String, Box<Pattern>),
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Int64,
    Int32,
    Float64,
    Float32,
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

#[derive(Debug, Clone)]
pub enum Bool {
    True,
    False,
}

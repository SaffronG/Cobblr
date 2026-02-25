#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Function(String, Vec<Vec<Param>>, Option<TypeExpr>, Block), // name, param-groups, ret, body
    AsyncFunction(String, Vec<Vec<Param>>, Option<TypeExpr>, Block),
    Struct(String, Vec<String>, Vec<StructMember>), // name, type_params, members
    Enum(String, Vec<(String, Option<Vec<TypeExpr>>)>),
    Trait(String, Vec<TraitMethod>),
    Impl(String, Vec<Decl>),
    ImplTrait(String, String, Vec<Decl>),
    Let(String, Option<TypeExpr>, Expr),
    LetMut(String, Option<TypeExpr>, Expr),
    Import(Path),
    DerivedStruct(Vec<String>, Box<Decl>),
}

impl Decl {
    pub fn iter(&self) -> DeclIter<'_> {
        match self {
            Decl::Impl(_, decls) | Decl::ImplTrait(_, _, decls) => DeclIter {
                inner: decls.iter(),
            },
            _ => DeclIter { inner: [].iter() },
        }
    }
}

pub struct DeclIter<'a> {
    inner: std::slice::Iter<'a, Decl>,
}

impl<'a> Iterator for DeclIter<'a> {
    type Item = &'a Decl;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<TypeExpr>,
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
    Match(Expr),
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
    String(String),
    Reference(Box<Expr>),
    MutReference(Box<Expr>),
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Tuple(Vec<Expr>),
    // Function calls
    Call(Box<Expr>, Vec<Expr>),
    AssocCall(String, String, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    // Struct literal: StructName { field: expr, ... }
    StructLit(String, Vec<(String, Expr)>),
    // Closures
    Closure(Vec<Param>, Box<Expr>),
    // Enum variants: Some(x) or None
    Variant(String, Option<Box<Expr>>),
    // Pipe operator: left |> right
    Pipe(Box<Expr>, Box<Expr>),
    // Postfix accessors
    Index(Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    DotAccess(Box<Expr>, String),
    // Binary arithmetic
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    // String concat
    Concat(Box<Expr>, Box<Expr>),
    // Logical Operators
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    // Comparisons
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    // Bitwise
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Nor(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Number {
    Int64(i64),
    Int32(i32),
    Float64(f64),
    Float32(f32),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Number(Number),
    Wildcard,
    Some(Box<Pattern>),
    Path(Path),
    Variant(String, Box<Pattern>),
    // PartialOp(Box<Expr>, BinaryOp),
    Expr(Expr),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Int64,
    Int32,
    Float64,
    Float32,
    Bool,
    String,
    Custom(String),
    Generic(String, Vec<TypeExpr>),
    MutableReference(Box<TypeExpr>),
    Reference(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
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

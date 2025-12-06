use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Span(pub usize, pub usize); // byte offsets: start..end

// --- Tokens ---
#[derive(Debug, Clone)]
pub enum Token {
    Ident(String, Span),
    Number(String, Span),
    StringLit(String, Span),
    Kw(String, Span),
    Punct(char, Span),
    Arrow(Span),    // ->
    FatArrow(Span), // =>
    Comma(Span),
    Colon(Span),
    Semicolon(Span),
    LParen(Span),
    RParen(Span),
    LBrace(Span),
    RBrace(Span),
    LBracket(Span),
    RBracket(Span),
}

// --- CST Nodes ---
#[derive(Debug, Clone)]
pub struct CstProgram {
    pub decls: Vec<CstDecl>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum CstDecl {
    Function(CstFunction),
    AsyncFunction(CstFunction),
    Struct(CstStruct),
    Enum(CstEnum),
    Trait(CstTrait),
    Impl(CstImpl),
    Let(CstLet),
    Import(CstImport),
    DerivedStruct(CstDerived),
}

#[derive(Debug, Clone)]
pub struct CstFunction {
    pub async_kw: Option<Token>,
    pub fn_kw: Token,
    pub name: Token,
    pub generics: Option<Vec<Token>>,
    pub param_groups: Vec<Vec<CstParam>>, // preserve grouping and punctuation
    pub ret_type: Option<(Token, CstType)>, // '->' token + type
    pub body: CstBlock,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct CstParam {
    pub name: Token,
    pub colon: Token,
    pub ty: CstType,
    pub is_mut: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct CstStruct {
    pub struct_kw: Token,
    pub name: Token,
    pub type_params: Option<Vec<Token>>,
    pub members: Vec<CstStructMember>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct CstStructMember {
    pub name: Token,
    pub colon: Token,
    pub ty: CstType,
    pub equal: Option<Token>,
    pub init_expr: Option<Box<CstExpr>>,
}

#[derive(Debug, Clone)]
pub struct CstEnum {
    pub enum_kw: Token,
    pub name: Token,
    pub variants: Vec<(Token, Option<Vec<CstType>>)>,
}

#[derive(Debug, Clone)]
pub struct CstTrait {
    pub trait_kw: Token,
    pub name: Token,
    pub methods: Vec<CstTraitMethod>,
}

#[derive(Debug, Clone)]
pub struct CstTraitMethod {
    pub name: Token,
    pub params: Vec<CstParam>,
}

#[derive(Debug, Clone)]
pub struct CstImpl {
    pub impl_kw: Token,
    pub target: Token,
    pub items: Vec<CstDecl>,
}

#[derive(Debug, Clone)]
pub struct CstLet {
    pub let_kw: Token,
    pub name: Token,
    pub colon_ty: Option<(Token, CstType)>,
    pub equal: Token,
    pub value: Box<CstExpr>,
}

#[derive(Debug, Clone)]
pub struct CstImport {
    pub import_kw: Token,
    pub path: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct CstDerived {
    pub attrs: Vec<Token>,
    pub inner: Box<CstDecl>,
}

#[derive(Debug, Clone)]
pub struct CstBlock {
    pub lbrace: Token,
    pub statements: Vec<CstStmt>,
    pub implicit_return: Option<Box<CstExpr>>,
    pub rbrace: Token,
}

#[derive(Debug, Clone)]
pub enum CstStmt {
    LetDecl(Box<CstDecl>),
    ExprStmt(Box<CstExpr>),
    Return(Token, Box<CstExpr>), // 'return' token + expr
    If(CstIf),
    While(Box<CstExpr>, CstBlock),
    Loop(CstBlock),
    Break(Token),
    For(
        Token, /*ident*/
        Token, /*in*/
        Box<CstExpr>,
        CstBlock,
    ),
    Match(Box<CstExpr>, Token /*match kw*/),
}

#[derive(Debug, Clone)]
pub struct CstIf {
    pub if_kw: Token,
    pub cond: Box<CstExpr>,
    pub then_block: CstBlock,
    pub else_kw: Option<Token>,
    pub else_block: Option<CstBlock>,
}

#[derive(Debug, Clone)]
pub enum CstExpr {
    Identifier(Token),
    Number(Token),
    StringLit(Token),
    Reference(Token /*&*/, Box<CstExpr>),
    MutReference(Token /*&mut*/, Box<CstExpr>),
    Match {
        match_kw: Token,
        value: Box<CstExpr>,
        lbrace: Token,
        arms: Vec<CstMatchArm>,
        rbrace: Token,
    },
    Tuple(Token /*lparen*/, Vec<CstExpr>, Token /*rparen*/),
    Call {
        callee: Box<CstExpr>,
        lparen: Token,
        args: Vec<(CstExpr, Option<Token>)>, // expr + optional comma token
        rparen: Token,
    },
    AssocCall {
        path_left: Token,
        coloncolon: Token,
        name: Token,
        lparen: Token,
        args: Vec<CstExpr>,
        rparen: Token,
    },
    MethodCall {
        receiver: Box<CstExpr>,
        dot: Token,
        name: Token,
        lparen: Token,
        args: Vec<CstExpr>,
        rparen: Token,
    },
    StructLit {
        name: Token,
        lbrace: Token,
        fields: Vec<(Token, Token /*:*/, CstExpr, Option<Token>)>,
        rbrace: Token,
    },
    Closure {
        pipe_l: Token,
        params: Vec<CstParam>,
        pipe_r: Token,
        arrow: Option<Token>,
        body: Box<CstExpr>,
    },
    Variant {
        name: Token,
        lparen: Option<Token>,
        inner: Option<Box<CstExpr>>,
        rparen: Option<Token>,
    },
    Pipe {
        left: Box<CstExpr>,
        pipe: Token,
        right: Box<CstExpr>,
    },
    Index {
        target: Box<CstExpr>,
        lbrack: Token,
        idx: Box<CstExpr>,
        rbrack: Token,
    },
    FieldAccess {
        target: Box<CstExpr>,
        dot: Token,
        name: Token,
    },
    DotAccess {
        target: Box<CstExpr>,
        dot: Token,
        name: Token,
    },
    Binary {
        left: Box<CstExpr>,
        op: Token,
        right: Box<CstExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct CstMatchArm {
    pub pattern: CstPattern,
    pub fat_arrow: Token,
    pub body: CstBlock,
}

#[derive(Debug, Clone)]
pub enum CstPattern {
    Var(Token),
    Number(Token),
    Wildcard(Token),
    Some(Token, Box<CstPattern>),
    None(Token),
    Variant(Token, Box<CstPattern>),
}

#[derive(Debug, Clone)]
pub enum CstType {
    Simple(Token),
    Generic {
        base: Token,
        lt: Token,
        params: Vec<CstType>,
        gt: Token,
    },
    Ref {
        amp: Token,
        inner: Box<CstType>,
    },
    MutRef {
        amp: Token,
        mut_kw: Token,
        inner: Box<CstType>,
    },
    Tuple {
        lparen: Token,
        elems: Vec<CstType>,
        rparen: Token,
    },
}

// ------------------------------------------------------------
// Simple converter helpers: convert AST nodes into minimal CST
// Note: These helpers assume source positions/spans are unknown; they
// create tokens with dummy spans (0,0). In a real parser you'd attach
// exact spans and original tokens.

fn tk_ident(name: &str) -> Token {
    Token::Ident(name.to_string(), Span(0, 0))
}

fn kw(s: &str) -> Token {
    Token::Kw(s.to_string(), Span(0, 0))
}

fn punct(c: char) -> Token {
    Token::Punct(c, Span(0, 0))
}

// The converter below is intentionally conservative: it tries to map
// AST shapes to corresponding CST nodes while preserving structural
// details (like parentheses, commas) using placeholder tokens.

impl From<&crate::Program> for CstProgram {
    fn from(ast: &crate::Program) -> Self {
        let decls = ast.0.iter().map(|d| CstDecl::from(d)).collect();
        CstProgram { decls, span: None }
    }
}

impl From<&crate::Decl> for CstDecl {
    fn from(ast: &crate::Decl) -> Self {
        match ast {
            crate::Decl::Function(name, param_groups, ret, body) => {
                CstDecl::Function(CstFunction {
                    async_kw: None,
                    fn_kw: kw("fn"),
                    name: tk_ident(name),
                    generics: None,
                    param_groups: param_groups
                        .iter()
                        .map(|group| group.iter().map(|p| CstParam::from(p)).collect())
                        .collect(),
                    ret_type: ret.as_ref().map(|t| (punct('-'), CstType::from(t))),
                    body: CstBlock::from(body),
                    span: None,
                })
            }
            crate::Decl::AsyncFunction(name, param_groups, ret, body) => {
                CstDecl::AsyncFunction(CstFunction {
                    async_kw: Some(kw("async")),
                    fn_kw: kw("fn"),
                    name: tk_ident(name),
                    generics: None,
                    param_groups: param_groups
                        .iter()
                        .map(|group| group.iter().map(|p| CstParam::from(p)).collect())
                        .collect(),
                    ret_type: ret.as_ref().map(|t| (punct('-'), CstType::from(t))),
                    body: CstBlock::from(body),
                    span: None,
                })
            }
            crate::Decl::Struct(name, tparams, members) => CstDecl::Struct(CstStruct {
                struct_kw: kw("struct"),
                name: tk_ident(name),
                type_params: if tparams.is_empty() {
                    None
                } else {
                    Some(tparams.iter().map(|s| tk_ident(s)).collect())
                },
                members: members.iter().map(|m| CstStructMember::from(m)).collect(),
                span: None,
            }),
            crate::Decl::Enum(name, variants) => CstDecl::Enum(CstEnum {
                enum_kw: kw("enum"),
                name: tk_ident(name),
                variants: variants
                    .iter()
                    .map(|(n, opt)| {
                        (
                            tk_ident(n),
                            opt.as_ref()
                                .map(|v| v.iter().map(|t| CstType::from(t)).collect()),
                        )
                    })
                    .collect(),
            }),
            crate::Decl::Trait(name, methods) => CstDecl::Trait(CstTrait {
                trait_kw: kw("trait"),
                name: tk_ident(name),
                methods: methods.iter().map(|m| CstTraitMethod::from(m)).collect(),
            }),
            crate::Decl::Impl(name, items) => CstDecl::Impl(CstImpl {
                impl_kw: kw("impl"),
                target: tk_ident(name),
                items: items.iter().map(|d| CstDecl::from(d)).collect(),
            }),
            crate::Decl::ImplTrait(trait_name, for_name, items) => CstDecl::Impl(CstImpl {
                impl_kw: kw("impl"),
                target: tk_ident(&format!("{} for {}", trait_name, for_name)),
                items: items.iter().map(|d| CstDecl::from(d)).collect(),
            }),
            crate::Decl::Let(name, opt_ty, expr) => CstDecl::Let(CstLet::from((
                true,
                false,
                name.clone(),
                opt_ty.clone(),
                expr.clone(),
            ))),
            crate::Decl::LetMut(name, opt_ty, expr) => CstDecl::Let(CstLet::from((
                false,
                true,
                name.clone(),
                opt_ty.clone(),
                expr.clone(),
            ))),
            crate::Decl::Import(path) => CstDecl::Import(CstImport::from(path)),
            crate::Decl::DerivedStruct(attrs, inner) => CstDecl::Derived(CstDerived {
                attrs: attrs.iter().map(|a| tk_ident(a)).collect(),
                inner: Box::new(CstDecl::from(inner.as_ref())),
            }),
        }
    }
}

impl From<&crate::StructMember> for CstStructMember {
    fn from(ast: &crate::StructMember) -> Self {
        CstStructMember {
            name: tk_ident(&ast.name),
            colon: punct(':'),
            ty: CstType::from(&ast.ty),
            equal: ast.init.as_ref().map(|_| punct('=')),
            init_expr: ast.init.as_ref().map(|e| Box::new(CstExpr::from(e))),
        }
    }
}

impl From<&crate::Param> for CstParam {
    fn from(ast: &crate::Param) -> Self {
        CstParam {
            name: tk_ident(&ast.name),
            colon: punct(':'),
            ty: CstType::from(&ast.ty),
            is_mut: if ast.is_mut { Some(kw("mut")) } else { None },
        }
    }
}

impl From<&crate::Block> for CstBlock {
    fn from(ast: &crate::Block) -> Self {
        CstBlock {
            lbrace: punct('{'),
            statements: ast.statements.iter().map(|s| CstStmt::from(s)).collect(),
            implicit_return: ast
                .implicit_return
                .as_ref()
                .map(|e| Box::new(CstExpr::from(e))),
            rbrace: punct('}'),
        }
    }
}

impl From<&crate::Stmt> for CstStmt {
    fn from(ast: &crate::Stmt) -> Self {
        match ast {
            crate::Stmt::LetDecl(d) => CstStmt::LetDecl(Box::new(CstDecl::from(d))),
            crate::Stmt::ExprStmt(e) => CstStmt::ExprStmt(Box::new(CstExpr::from(e))),
            crate::Stmt::Return(e) => CstStmt::Return(kw("return"), Box::new(CstExpr::from(e))),
            crate::Stmt::If(ifst) => CstStmt::If(CstIf::from(ifst)),
            crate::Stmt::While(cond, blk) => {
                CstStmt::While(Box::new(CstExpr::from(cond)), CstBlock::from(blk))
            }
            crate::Stmt::Loop(blk) => CstStmt::Loop(CstBlock::from(blk)),
            crate::Stmt::Break => CstStmt::Break(kw("break")),
            crate::Stmt::For(ident, expr, blk) => CstStmt::For(
                tk_ident(ident),
                kw("in"),
                Box::new(CstExpr::from(expr)),
                CstBlock::from(blk),
            ),
            crate::Stmt::Match(expr) => CstStmt::Match(Box::new(CstExpr::from(expr)), kw("match")),
        }
    }
}

impl From<&crate::IfStmt> for CstIf {
    fn from(ast: &crate::IfStmt) -> Self {
        CstIf {
            if_kw: kw("if"),
            cond: Box::new(CstExpr::from(&ast.cond)),
            then_block: CstBlock::from(&ast.then_branch),
            else_kw: ast.else_branch.as_ref().map(|_| kw("else")),
            else_block: ast.else_branch.as_ref().map(|b| CstBlock::from(b)),
        }
    }
}

impl From<&crate::Expr> for CstExpr {
    fn from(ast: &crate::Expr) -> Self {
        match ast {
            crate::Expr::Identifier(s) => CstExpr::Identifier(tk_ident(s)),
            crate::Expr::Number(n) => match n {
                crate::Number::Int(i) => CstExpr::Number(Token::Number(i.to_string(), Span(0, 0))),
                crate::Number::Float(f) => {
                    CstExpr::Number(Token::Number(f.to_string(), Span(0, 0)))
                }
            },
            crate::Expr::String(s) => CstExpr::StringLit(Token::StringLit(s.clone(), Span(0, 0))),
            crate::Expr::Reference(e) => CstExpr::Reference(punct('&'), Box::new(CstExpr::from(e))),
            crate::Expr::MutReference(e) => {
                CstExpr::MutReference(punct('&'), Box::new(CstExpr::from(e)))
            }
            crate::Expr::Tuple(elems) => CstExpr::Tuple(
                punct('('),
                elems.iter().map(|e| CstExpr::from(e)).collect(),
                punct(')'),
            ),
            crate::Expr::Call(callee, args) => CstExpr::Call {
                callee: Box::new(CstExpr::from(callee)),
                lparen: punct('('),
                args: args
                    .iter()
                    .map(|a| (CstExpr::from(a), Some(punct(','))))
                    .collect(),
                rparen: punct(')'),
            },
            crate::Expr::AssocCall(left, name, args) => CstExpr::AssocCall {
                path_left: tk_ident(left),
                coloncolon: punct(':'),
                name: tk_ident(name),
                lparen: punct('('),
                args: args.iter().map(|a| CstExpr::from(a)).collect(),
                rparen: punct(')'),
            },
            crate::Expr::MethodCall(receiver, name, args) => CstExpr::MethodCall {
                receiver: Box::new(CstExpr::from(receiver)),
                dot: punct('.'),
                name: tk_ident(name),
                lparen: punct('('),
                args: args.iter().map(|a| CstExpr::from(a)).collect(),
                rparen: punct(')'),
            },
            crate::Expr::StructLit(name, fields) => CstExpr::StructLit {
                name: tk_ident(name),
                lbrace: punct('{'),
                fields: fields
                    .iter()
                    .map(|(n, e)| (tk_ident(n), punct(':'), CstExpr::from(e), Some(punct(','))))
                    .collect(),
                rbrace: punct('}'),
            },
            crate::Expr::Closure(params, body) => CstExpr::Closure {
                pipe_l: punct('|'),
                params: params.iter().map(|p| CstParam::from(p)).collect(),
                pipe_r: punct('|'),
                arrow: None,
                body: Box::new(CstExpr::from(body)),
            },
            crate::Expr::Variant(name, maybe) => CstExpr::Variant {
                name: tk_ident(name),
                lparen: maybe.as_ref().map(|_| punct('(')),
                inner: maybe.as_ref().map(|e| Box::new(CstExpr::from(e))),
                rparen: maybe.as_ref().map(|_| punct(')')),
            },
            crate::Expr::Pipe(l, r) => CstExpr::Pipe {
                left: Box::new(CstExpr::from(l)),
                pipe: punct('|'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Index(t, i) => CstExpr::Index {
                target: Box::new(CstExpr::from(t)),
                lbrack: punct('['),
                idx: Box::new(CstExpr::from(i)),
                rbrack: punct(']'),
            },
            crate::Expr::FieldAccess(t, name) => CstExpr::FieldAccess {
                target: Box::new(CstExpr::from(t)),
                dot: punct('.'),
                name: tk_ident(name),
            },
            crate::Expr::DotAccess(t, name) => CstExpr::DotAccess {
                target: Box::new(CstExpr::from(t)),
                dot: punct('.'),
                name: tk_ident(name),
            },
            crate::Expr::Add(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('+'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Sub(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('-'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Mul(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('*'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Div(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('/'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Concat(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('+'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Equal(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('='),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::NotEqual(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('!'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Less(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('<'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::LessEq(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('<'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Greater(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('>'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::GreaterEq(l, r) => CstExpr::Binary {
                left: Box::new(CstExpr::from(l)),
                op: punct('>'),
                right: Box::new(CstExpr::from(r)),
            },
            crate::Expr::Match { value, arms } => CstExpr::Match {
                match_kw: kw("match"),
                value: Box::new(CstExpr::from(value)),
                lbrace: punct('{'),
                arms: arms.iter().map(|a| CstMatchArm::from(a)).collect(),
                rbrace: punct('}'),
            },
        }
    }
}

impl From<&crate::MatchArm> for CstMatchArm {
    fn from(ast: &crate::MatchArm) -> Self {
        CstMatchArm {
            pattern: CstPattern::from(&ast.pattern),
            fat_arrow: punct('='),
            body: CstBlock::from(&ast.body),
        }
    }
}

impl From<&crate::Pattern> for CstPattern {
    fn from(ast: &crate::Pattern) -> Self {
        match ast {
            crate::Pattern::Var(s) => CstPattern::Var(tk_ident(s)),
            crate::Pattern::Number(n) => match n {
                crate::Number::Int(i) => {
                    CstPattern::Number(Token::Number(i.to_string(), Span(0, 0)))
                }
                crate::Number::Float(f) => {
                    CstPattern::Number(Token::Number(f.to_string(), Span(0, 0)))
                }
            },
            crate::Pattern::Wildcard => CstPattern::Wildcard(punct('_')),
            crate::Pattern::Some(p) => CstPattern::Some(kw("Some"), Box::new(CstPattern::from(p))),
            crate::Pattern::None => CstPattern::None(kw("None")),
            crate::Pattern::Variant(name, p) => {
                CstPattern::Variant(tk_ident(name), Box::new(CstPattern::from(p)))
            }
        }
    }
}

impl From<&crate::TypeExpr> for CstType {
    fn from(ast: &crate::TypeExpr) -> Self {
        match ast {
            crate::TypeExpr::Int64 => CstType::Simple(kw("i64")),
            crate::TypeExpr::Int32 => CstType::Simple(kw("i32")),
            crate::TypeExpr::Float64 => CstType::Simple(kw("f64")),
            crate::TypeExpr::Float32 => CstType::Simple(kw("f32")),
            crate::TypeExpr::Bool => CstType::Simple(kw("bool")),
            crate::TypeExpr::String => CstType::Simple(kw("String")),
            crate::TypeExpr::Custom(s) => CstType::Simple(tk_ident(s)),
            crate::TypeExpr::Generic(base, params) => CstType::Generic {
                base: tk_ident(base),
                lt: punct('<'),
                params: params.iter().map(|p| CstType::from(p)).collect(),
                gt: punct('>'),
            },
            crate::TypeExpr::MutableReference(inner) => CstType::MutRef {
                amp: punct('&'),
                mut_kw: kw("mut"),
                inner: Box::new(CstType::from(inner)),
            },
            crate::TypeExpr::Reference(inner) => CstType::Ref {
                amp: punct('&'),
                inner: Box::new(CstType::from(inner)),
            },
            crate::TypeExpr::Tuple(elems) => CstType::Tuple {
                lparen: punct('('),
                elems: elems.iter().map(|e| CstType::from(e)).collect(),
                rparen: punct(')'),
            },
        }
    }
}

impl From<&crate::PathExpr> for CstImport {
    fn from(ast: &crate::PathExpr) -> Self {
        let mut parts = Vec::new();
        fn collect(p: &crate::PathExpr, out: &mut Vec<Token>) {
            match p {
                crate::PathExpr::Single(s) => out.push(tk_ident(s)),
                crate::PathExpr::Nested(parent, name) => {
                    collect(parent, out);
                    out.push(tk_ident(name));
                }
            }
        }
        collect(ast, &mut parts);
        CstImport {
            import_kw: kw("import"),
            path: parts,
        }
    }
}

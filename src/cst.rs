// TODO: Implement fully fleshed out CST based on the AST and Lalrpop Grammar
use crate::ast::*;
use std::convert::TryFrom;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn join(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentToken(pub String, pub Span);
#[derive(Debug, Clone)]
pub struct IntToken(pub i64, pub Span);
#[derive(Debug, Clone)]
pub struct FloatToken(pub f64, pub Span);
#[derive(Debug, Clone)]
pub struct StrToken(pub String, pub Span);
#[derive(Debug, Clone)]
pub struct BoolToken(pub bool, pub Span);

#[derive(Debug, Clone)]
pub struct CstError {
    pub msg: String,
    pub span: Option<Span>,
}

impl CstError {
    fn new<S: Into<String>>(msg: S, span: Option<Span>) -> Self {
        Self {
            msg: msg.into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CProgram {
    pub decls: Vec<CDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CDecl {
    Function {
        name: IdentToken,
        params: Vec<CParam>,
        ret_ty: Option<CTypeExpr>,
        body: CBlock,
        span: Span,
        is_async: bool,
    },
    Struct {
        name: IdentToken,
        members: Vec<CStructMember>,
        span: Span,
    },
    Enum {
        name: IdentToken,
        variants: Vec<CEnumVariant>,
        span: Span,
    },
    Trait {
        name: IdentToken,
        methods: Vec<CTraitMethod>,
        span: Span,
    },
    Impl {
        name: IdentToken,
        items: Vec<CDecl>,
        span: Span,
    },
    ImplTrait {
        trait_name: IdentToken,
        for_name: IdentToken,
        items: Vec<CDecl>,
        span: Span,
    },
    Let {
        name: IdentToken,
        ty: Option<CTypeExpr>,
        value: CExpr,
        span: Span,
        is_mut: bool,
    },
    Import {
        path: CPath,
        span: Span,
    },
    DerivedStruct {
        derives: Vec<IdentToken>,
        struct_decl: Box<CDecl>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct CPath {
    pub parts: Vec<IdentToken>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CStructMember {
    pub name: IdentToken,
    pub ty: CTypeExpr,
    pub init: Option<CExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CEnumVariant {
    pub name: IdentToken,
    pub payload: Option<Vec<CTypeExpr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CTraitMethod {
    pub name: IdentToken,
    pub params: Vec<CParam>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CParam {
    pub name: IdentToken,
    pub ty: CTypeExpr,
    pub is_mut: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CBlock {
    pub statements: Vec<CStmt>,
    pub implicit_return: Option<CExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CStmt {
    LetDecl(Box<CDecl>),
    ExprStmt(CExpr),
    Return(CExpr),
    If(CIfStmt),
    While(CExpr, CBlock),
    Loop(CBlock),
    Break(Span),
    For {
        name: IdentToken,
        target: CExpr,
        body: CBlock,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct CIfStmt {
    pub cond: Box<CExpr>,
    pub then_branch: CBlock,
    pub else_branch: Option<CBlock>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CExpr {
    Identifier(IdentToken),
    NumberInt(IntToken),
    NumberFloat(FloatToken),
    String(StrToken),
    Bool(BoolToken),
    Match {
        value: Box<CExpr>,
        arms: Vec<CMatchArm>,
        span: Span,
    },
    Call {
        name: IdentToken,
        args: Vec<CExpr>,
        span: Span,
    },
    AssocCall {
        ty: IdentToken,
        method: IdentToken,
        args: Vec<CExpr>,
        span: Span,
    },
    MethodCall {
        target: Box<CExpr>,
        method: IdentToken,
        args: Vec<CExpr>,
        span: Span,
    },
    StructLit {
        name: IdentToken,
        fields: Vec<(IdentToken, CExpr)>,
        span: Span,
    },
    Closure {
        params: Vec<IdentToken>,
        body: Box<CExpr>,
        span: Span,
    },
    Add(Box<CExpr>, Box<CExpr>, Span),
    Sub(Box<CExpr>, Box<CExpr>, Span),
    Mul(Box<CExpr>, Box<CExpr>, Span),
    Div(Box<CExpr>, Box<CExpr>, Span),
    Concat(Box<CExpr>, Box<CExpr>, Span),
    Equal(Box<CExpr>, Box<CExpr>, Span),
    NotEqual(Box<CExpr>, Box<CExpr>, Span),
    Less(Box<CExpr>, Box<CExpr>, Span),
    LessEq(Box<CExpr>, Box<CExpr>, Span),
    Greater(Box<CExpr>, Box<CExpr>, Span),
    GreaterEq(Box<CExpr>, Box<CExpr>, Span),
}

#[derive(Debug, Clone)]
pub struct CMatchArm {
    pub pattern: CPattern,
    pub body: CBlock,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum CPattern {
    Var(IdentToken),
    NumberInt(IntToken),
    NumberFloat(FloatToken),
    Wildcard(Span),
    Some(Box<CPattern>, Span),
    None(Span),
    Variant(IdentToken, Box<CPattern>, Span),
}

#[derive(Debug, Clone)]
pub enum CTypeExpr {
    Int64(Span),
    Int32(Span),
    Float64(Span),
    Bool(Span),
    String(Span),
    Custom(IdentToken),
    Generic {
        id: IdentToken,
        params: Vec<CTypeExpr>,
        span: Span,
    },
    Paren(Box<CTypeExpr>, Span),
}

impl TryFrom<CProgram> for Program {
    type Error = CstError;
    fn try_from(cprog: CProgram) -> Result<Self, Self::Error> {
        let mut decls = Vec::with_capacity(cprog.decls.len());
        for d in cprog.decls {
            decls.push(d.try_into()?);
        }
        Ok(Program(decls))
    }
}

impl TryFrom<CDecl> for Decl {
    type Error = CstError;
    fn try_from(c: CDecl) -> Result<Self, Self::Error> {
        use CDecl::*;
        match c {
            Function {
                name,
                params,
                ret_ty,
                body,
                is_async,
                ..
            } => {
                let ast_params = params
                    .into_iter()
                    .map(|p| p.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                let ast_body = body.try_into()?;
                let ast_ret = match ret_ty {
                    None => None,
                    Some(t) => Some(t.try_into()?),
                };
                if is_async {
                    Ok(Decl::AsyncFunction(name.0, ast_params, ast_ret, ast_body))
                } else {
                    Ok(Decl::Function(name.0, ast_params, ast_ret, ast_body))
                }
            }
            Struct { name, members, .. } => {
                let ast_members = members
                    .into_iter()
                    .map(|m| m.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Decl::Struct(name.0, ast_members))
            }
            Enum { name, variants, .. } => {
                let ast_variants = variants
                    .into_iter()
                    .map(|v| v.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Decl::Enum(name.0, ast_variants))
            }
            Trait { name, methods, .. } => {
                let ast_methods = methods
                    .into_iter()
                    .map(|m| m.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Decl::Trait(name.0, ast_methods))
            }
            Impl { name, items, .. } => {
                let ast_items = items
                    .into_iter()
                    .map(|it| it.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Decl::Impl(name.0, ast_items))
            }
            ImplTrait {
                trait_name,
                for_name,
                items,
                ..
            } => {
                let ast_items = items
                    .into_iter()
                    .map(|it| it.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Decl::ImplTrait(trait_name.0, for_name.0, ast_items))
            }
            Let {
                name,
                ty,
                value,
                is_mut,
                ..
            } => {
                let expr = value.try_into()?;
                let ty_expr = match ty {
                    None => None,
                    Some(t) => Some(t.try_into()?),
                };
                if is_mut {
                    Ok(Decl::LetMut(name.0, ty_expr, expr))
                } else {
                    Ok(Decl::Let(name.0, ty_expr, expr))
                }
            }
            Import { path, .. } => Ok(Decl::Import(path.try_into()?)),
            DerivedStruct {
                derives,
                struct_decl,
                ..
            } => {
                let inner = (*struct_decl).try_into()?;
                let derives_str = derives.into_iter().map(|d| d.0).collect();
                Ok(Decl::DerivedStruct(derives_str, Box::new(inner)))
            }
        }
    }
}

impl TryFrom<Box<CDecl>> for Decl {
    type Error = CstError;
    fn try_from(c: Box<CDecl>) -> Result<Self, Self::Error> {
        (*c).try_into()
    }
}

impl TryFrom<CParam> for Param {
    type Error = CstError;
    fn try_from(c: CParam) -> Result<Self, Self::Error> {
        Ok(Param {
            name: c.name.0,
            ty: c.ty.try_into()?,
            is_mut: c.is_mut,
        })
    }
}

impl TryFrom<CStructMember> for StructMember {
    type Error = CstError;
    fn try_from(c: CStructMember) -> Result<Self, Self::Error> {
        let init = match c.init {
            None => None,
            Some(e) => Some(e.try_into()?),
        };
        Ok(StructMember {
            name: c.name.0,
            ty: c.ty.try_into()?,
            init,
        })
    }
}

impl TryFrom<CEnumVariant> for (String, Option<Vec<TypeExpr>>) {
    type Error = CstError;
    fn try_from(c: CEnumVariant) -> Result<Self, Self::Error> {
        let payload = match c.payload {
            None => None,
            Some(vec) => Some(
                vec.into_iter()
                    .map(|t| t.try_into())
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        };
        Ok((c.name.0, payload))
    }
}

impl TryFrom<CTraitMethod> for TraitMethod {
    type Error = CstError;
    fn try_from(c: CTraitMethod) -> Result<Self, Self::Error> {
        let params = c
            .params
            .into_iter()
            .map(|p| p.try_into())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(TraitMethod::sig(c.name.0, params))
    }
}

impl TryFrom<CBlock> for Block {
    type Error = CstError;
    fn try_from(c: CBlock) -> Result<Self, Self::Error> {
        let stmts = c
            .statements
            .into_iter()
            .map(|s| s.try_into())
            .collect::<Result<Vec<_>, _>>()?;
        let implicit_return = match c.implicit_return {
            None => None,
            Some(e) => Some(e.try_into()?),
        };
        Ok(Block {
            statements: stmts,
            implicit_return,
        })
    }
}

impl TryFrom<CStmt> for Stmt {
    type Error = CstError;
    fn try_from(c: CStmt) -> Result<Self, Self::Error> {
        match c {
            CStmt::LetDecl(boxed) => Ok(Stmt::LetDecl(boxed.try_into()?)),
            CStmt::ExprStmt(e) => Ok(Stmt::ExprStmt(e.try_into()?)),
            CStmt::Return(e) => Ok(Stmt::Return(e.try_into()?)),
            CStmt::If(ifc) => Ok(Stmt::If(ifc.try_into()?)),
            CStmt::While(cond, body) => Ok(Stmt::While(cond.try_into()?, body.try_into()?)),
            CStmt::Loop(b) => Ok(Stmt::Loop(b.try_into()?)),
            CStmt::Break(_) => Ok(Stmt::Break),
            CStmt::For {
                name, target, body, ..
            } => Ok(Stmt::For(name.0, target.try_into()?, body.try_into()?)),
        }
    }
}

impl TryFrom<CIfStmt> for IfStmt {
    type Error = CstError;
    fn try_from(c: CIfStmt) -> Result<Self, Self::Error> {
        Ok(IfStmt {
            cond: Box::new(c.cond.try_into()?),
            then_branch: c.then_branch.try_into()?,
            else_branch: match c.else_branch {
                None => None,
                Some(b) => Some(b.try_into()?),
            },
        })
    }
}

impl TryFrom<CMatchArm> for MatchArm {
    type Error = CstError;
    fn try_from(c: CMatchArm) -> Result<Self, Self::Error> {
        Ok(MatchArm {
            pattern: c.pattern.try_into()?,
            body: c.body.try_into()?,
        })
    }
}

impl TryFrom<CPattern> for Pattern {
    type Error = CstError;
    fn try_from(c: CPattern) -> Result<Self, Self::Error> {
        match c {
            CPattern::Var(id) => Ok(Pattern::Var(id.0)),
            CPattern::NumberInt(i) => Ok(Pattern::Number(Number::Int(i.0))),
            CPattern::NumberFloat(f) => Ok(Pattern::Number(Number::Float(f.0))),
            CPattern::Wildcard(_) => Ok(Pattern::Wildcard),
            CPattern::Some(inner, _) => Ok(Pattern::Some(Box::new((*inner).try_into()?))),
            CPattern::None(_) => Ok(Pattern::None),
            CPattern::Variant(id, inner, _) => {
                Ok(Pattern::Variant(id.0, Box::new((*inner).try_into()?)))
            }
        }
    }
}

impl TryFrom<CTypeExpr> for TypeExpr {
    type Error = CstError;
    fn try_from(c: CTypeExpr) -> Result<Self, Self::Error> {
        match c {
            CTypeExpr::Int64(_) => Ok(TypeExpr::Int64),
            CTypeExpr::Int32(_) => Ok(TypeExpr::Int32),
            CTypeExpr::Float64(_) => Ok(TypeExpr::Float64),
            CTypeExpr::Bool(_) => Ok(TypeExpr::Bool),
            CTypeExpr::String(_) => Ok(TypeExpr::String),
            CTypeExpr::Custom(id) => Ok(TypeExpr::Custom(id.0)),
            CTypeExpr::Generic { id, params, .. } => {
                let out = params
                    .into_iter()
                    .map(|p| p.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(TypeExpr::Generic(id.0, out))
            }
            CTypeExpr::Paren(inner, _) => (*inner).try_into(),
        }
    }
}

impl TryFrom<CPath> for Path {
    type Error = CstError;
    fn try_from(c: CPath) -> Result<Self, Self::Error> {
        let mut out_parts = c.parts.into_iter().map(|p| p.0).collect::<Vec<_>>();
        if out_parts.is_empty() {
            return Err(CstError::new("empty import path", Some(c.span)));
        }
        let first = out_parts.remove(0);
        let mut cur = PathExpr::Single(first);
        for part in out_parts {
            cur = PathExpr::Nested(Box::new(cur), part);
        }
        Ok(cur)
    }
}

impl TryFrom<CExpr> for Expr {
    type Error = CstError;
    fn try_from(c: CExpr) -> Result<Self, Self::Error> {
        use CExpr::*;
        match c {
            Identifier(id) => Ok(Expr::Identifier(id.0)),
            NumberInt(i) => Ok(Expr::Number(Number::Int(i.0))),
            NumberFloat(f) => Ok(Expr::Number(Number::Float(f.0))),
            String(s) => Ok(Expr::String(s.0)),
            Bool(b) => {
                let name = if b.0 {
                    "true".to_string()
                } else {
                    "false".to_string()
                };
                Ok(Expr::Identifier(name))
            }
            Match { value, arms, .. } => {
                let val = (*value).try_into()?;
                let ast_arms = arms
                    .into_iter()
                    .map(|a| a.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Match {
                    value: Box::new(val),
                    arms: ast_arms,
                })
            }
            Call { name, args, .. } => {
                let ast_args = args
                    .into_iter()
                    .map(|a| a.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Call(name.0, ast_args))
            }
            AssocCall {
                ty, method, args, ..
            } => {
                let ast_args = args
                    .into_iter()
                    .map(|a| a.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::AssocCall(ty.0, method.0, ast_args))
            }
            MethodCall {
                target,
                method,
                args,
                ..
            } => {
                let t = (*target).try_into()?;
                let ast_args = args
                    .into_iter()
                    .map(|a| a.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::MethodCall(Box::new(t), method.0, ast_args))
            }
            StructLit { name, fields, .. } => {
                let ast_fields = fields
                    .into_iter()
                    .map(|(id, e)| Ok((id.0, e.try_into()?)))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::StructLit(name.0, ast_fields))
            }
            Closure { params, body, .. } => {
                let names = params.into_iter().map(|p| p.0).collect();
                let body_ast = (*body).try_into()?;
                Ok(Expr::Closure(names, Box::new(body_ast)))
            }
            Add(l, r, _) => Ok(Expr::Add(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Sub(l, r, _) => Ok(Expr::Sub(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Mul(l, r, _) => Ok(Expr::Mul(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Div(l, r, _) => Ok(Expr::Div(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Concat(l, r, _) => Ok(Expr::Concat(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Equal(l, r, _) => Ok(Expr::Equal(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            NotEqual(l, r, _) => Ok(Expr::NotEqual(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Less(l, r, _) => Ok(Expr::Less(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            LessEq(l, r, _) => Ok(Expr::LessEq(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            Greater(l, r, _) => Ok(Expr::Greater(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
            GreaterEq(l, r, _) => Ok(Expr::GreaterEq(
                Box::new((*l).try_into()?),
                Box::new((*r).try_into()?),
                Span { start: 0, end: 0 },
            )),
        }
    }
}

impl From<IntToken> for i64 {
    fn from(i: IntToken) -> Self {
        i.0
    }
}
impl From<FloatToken> for f64 {
    fn from(f: FloatToken) -> Self {
        f.0
    }
}
impl From<StrToken> for String {
    fn from(s: StrToken) -> Self {
        s.0
    }
}
impl From<IdentToken> for String {
    fn from(i: IdentToken) -> Self {
        i.0
    }
}

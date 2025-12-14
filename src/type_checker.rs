use crate::ast::*;
use std::collections::HashMap;

#[derive(Clone)]
struct VarInfo {
    ty: CanonType,
    is_mut: bool,
}

#[derive(Clone)]
struct FnSig {
    params: Vec<CanonType>,
    ret: Option<CanonType>,
    is_async: bool,
}

#[derive(Clone)]
struct StructInfo {
    fields: HashMap<String, CanonType>,
}

#[derive(Clone)]
struct EnumInfo {
    variants: HashMap<String, Option<CanonType>>,
}

#[derive(Clone)]
struct TraitInfo {
    methods: HashMap<String, Vec<CanonType>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CanonType {
    Int,
    Float,
    Bool,
    String,
    Unit,
    Tuple(Vec<CanonType>),
    Ref(Box<CanonType>, bool),
    Struct(String),
    Enum(String),
    Generic(String),
    Unknown,
}

pub struct TypeEnv {
    vars: Vec<HashMap<String, VarInfo>>,
    functions: HashMap<String, FnSig>,
    structs: HashMap<String, StructInfo>,
    enums: HashMap<String, EnumInfo>,
    traits: HashMap<String, TraitInfo>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            vars: vec![HashMap::new()],
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            traits: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn insert_var(&mut self, name: String, info: VarInfo) {
        self.vars.last_mut().unwrap().insert(name, info);
    }

    fn lookup_var(&self, name: &str) -> Option<&VarInfo> {
        self.vars.iter().rev().find_map(|s| s.get(name))
    }
}

#[derive(Debug)]
pub enum TypeError {
    DuplicateFunction(String),
    UnknownIdentifier(String),
    TypeMismatch { expected: TypeExpr, found: TypeExpr },
    MutabilityViolation(String),
    InvalidCall(String),
    InvalidReturn,
    NonBoolCondition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(usize);

#[derive(Debug)]
pub struct TypedProgram {
    pub program: Program,
    pub expr_types: HashMap<ExprId, TypeExpr>,
}

struct TypeCtx {
    env: TypeEnv,
    expr_types: HashMap<ExprId, TypeExpr>,
    next_expr_id: usize,
}

impl TypeCtx {
    fn alloc_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    fn record(&mut self, ty: CanonType) -> CanonType {
        let id = self.alloc_expr_id();
        self.expr_types.insert(id, uncanon_type(&ty));
        ty
    }
}

pub fn collect_decls(program: &Program) -> Result<TypeEnv, Vec<TypeError>> {
    let mut env = TypeEnv::new();
    let mut errors = vec![];

    for decl in &program.decls {
        match decl {
            Decl::Function(name, params, ret, _) | Decl::AsyncFunction(name, params, ret, _) => {
                if env.functions.contains_key(name) {
                    errors.push(TypeError::DuplicateFunction(name.clone()));
                    continue;
                }

                let flat_params = params
                    .iter()
                    .flatten()
                    .map(|p| canon_type(p.ty.as_ref().unwrap()))
                    .collect();

                env.functions.insert(
                    name.clone(),
                    FnSig {
                        params: flat_params,
                        ret: ret.as_ref().map(canon_type),
                        is_async: matches!(decl, Decl::AsyncFunction(..)),
                    },
                );
            }

            Decl::Struct(name, _, members) => {
                let mut fields = HashMap::new();
                for m in members {
                    fields.insert(m.name.clone(), canon_type(&m.ty));
                }
                env.structs.insert(name.clone(), StructInfo { fields });
            }

            Decl::Enum(name, variants) => {
                let mut map = HashMap::new();
                for (vname, payload) in variants {
                    let ty = payload.as_ref().map(|v| {
                        if v.len() == 1 {
                            canon_type(&v[0])
                        } else {
                            CanonType::Tuple(v.iter().map(canon_type).collect())
                        }
                    });
                    map.insert(vname.clone(), ty);
                }
                env.enums.insert(name.clone(), EnumInfo { variants: map });
            }

            Decl::Trait(name, methods) => {
                let mut map = HashMap::new();
                for m in methods {
                    let tys = m
                        .params
                        .iter()
                        .map(|p| canon_type(p.ty.as_ref().unwrap()))
                        .collect();
                    map.insert(m.name.clone(), tys);
                }
                env.traits.insert(name.clone(), TraitInfo { methods: map });
            }

            _ => {}
        }
    }

    if errors.is_empty() {
        Ok(env)
    } else {
        Err(errors)
    }
}

pub fn type_check(program: &Program) -> Result<TypedProgram, Vec<TypeError>> {
    let env = collect_decls(program)?;
    let mut ctx = TypeCtx {
        env,
        expr_types: HashMap::new(),
        next_expr_id: 0,
    };

    let mut errors = vec![];

    for decl in &program.decls {
        if let Decl::Function(_, params, ret, body) | Decl::AsyncFunction(_, params, ret, body) =
            decl
        {
            ctx.env.push_scope();

            for group in params {
                for p in group {
                    ctx.env.insert_var(
                        p.name.clone(),
                        VarInfo {
                            ty: canon_type(p.ty.as_ref().unwrap()),
                            is_mut: p.is_mut,
                        },
                    );
                }
            }

            if let Err(e) = check_block(body, &mut ctx, &ret.as_ref().map(canon_type)) {
                errors.push(e);
            }

            ctx.env.pop_scope();
        }
    }

    if errors.is_empty() {
        Ok(TypedProgram {
            program: program.clone(),
            expr_types: ctx.expr_types,
        })
    } else {
        Err(errors)
    }
}

fn check_expr(expr: &Expr, ctx: &mut TypeCtx) -> Result<CanonType, TypeError> {
    use Expr::*;

    let ty = match expr {
        Identifier(name) => ctx
            .env
            .lookup_var(name)
            .ok_or_else(|| TypeError::UnknownIdentifier(name.clone()))?
            .ty
            .clone(),

        Number(crate::ast::Number::Int(_)) => CanonType::Int,
        Number(crate::ast::Number::Float(_)) => CanonType::Float,
        String(_) => CanonType::String,

        Reference(e) => CanonType::Ref(Box::new(check_expr(e, ctx)?), false),
        MutReference(e) => CanonType::Ref(Box::new(check_expr(e, ctx)?), true),

        Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => {
            let ta = check_expr(a, ctx)?;
            let tb = check_expr(b, ctx)?;
            if ta != tb {
                return Err(TypeError::TypeMismatch {
                    expected: uncanon_type(&ta),
                    found: uncanon_type(&tb),
                });
            }
            ta
        }

        Call(callee, args) => {
            let Identifier(name) = &**callee else {
                return Err(TypeError::InvalidCall("<expr>".into()));
            };

            let sig = ctx
                .env
                .functions
                .get(name)
                .cloned()
                .ok_or_else(|| TypeError::InvalidCall(name.clone()))?;

            if sig.params.len() != args.len() {
                return Err(TypeError::InvalidCall(name.clone()));
            }

            for (arg, param_ty) in args.iter().zip(&sig.params) {
                let at = check_expr(arg, ctx)?;
                if at != *param_ty {
                    return Err(TypeError::TypeMismatch {
                        expected: uncanon_type(param_ty),
                        found: uncanon_type(&at),
                    });
                }
            }

            sig.ret.clone().unwrap_or(CanonType::Unit)
        }

        StructLit(name, fields) => {
            let info = ctx
                .env
                .structs
                .get(name)
                .cloned()
                .ok_or_else(|| TypeError::UnknownIdentifier(name.clone()))?;

            for (fname, fexpr) in fields {
                let expected = info
                    .fields
                    .get(fname)
                    .ok_or_else(|| TypeError::UnknownIdentifier(fname.clone()))?;
                let found = check_expr(fexpr, ctx)?;
                if &found != expected {
                    return Err(TypeError::TypeMismatch {
                        expected: uncanon_type(expected),
                        found: uncanon_type(&found),
                    });
                }
            }

            CanonType::Struct(name.clone())
        }

        FieldAccess(expr, field) => {
            let ty = check_expr(expr, ctx)?;
            let CanonType::Struct(name) = ty else {
                return Err(TypeError::InvalidCall("field access".into()));
            };

            ctx.env
                .structs
                .get(&name)
                .and_then(|s| s.fields.get(field))
                .cloned()
                .ok_or_else(|| TypeError::UnknownIdentifier(field.clone()))?
        }

        Expr::Variant(name, payload) => {
            let enums: Vec<(std::string::String, EnumInfo)> = ctx
                .env
                .enums
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            let mut found = None;

            for (enum_name, info) in enums {
                if let Some(expected) = info.variants.get(name) {
                    match (expected, payload) {
                        (None, None) => {
                            found = Some(CanonType::Enum(enum_name));
                            break;
                        }
                        (Some(et), Some(expr)) => {
                            let ft = check_expr(expr, ctx)?;
                            let expected_ct = canon_type(&uncanon_type(et));

                            if ft != expected_ct {
                                return Err(TypeError::TypeMismatch {
                                    expected: (uncanon_type(et)).clone(),
                                    found: uncanon_type(&ft),
                                });
                            }

                            found = Some(CanonType::Enum(enum_name));
                            break;
                        }
                        _ => {
                            return Err(TypeError::InvalidCall(name.clone()));
                        }
                    }
                }
            }

            found.ok_or_else(|| TypeError::UnknownIdentifier(name.clone()))?
        }

        _ => CanonType::Unknown,
    };

    Ok(ctx.record(ty))
}

fn check_stmt(
    stmt: &Stmt,
    ctx: &mut TypeCtx,
    expected_ret: &Option<CanonType>,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::LetDecl(Decl::Let(name, ty, expr)) | Stmt::LetDecl(Decl::LetMut(name, ty, expr)) => {
            let expr_ty = check_expr(expr, ctx)?;
            let final_ty = match ty {
                Some(t) => {
                    let ct = canon_type(t);
                    if ct != expr_ty {
                        return Err(TypeError::TypeMismatch {
                            expected: t.clone(),
                            found: uncanon_type(&expr_ty),
                        });
                    }
                    ct
                }
                None => expr_ty,
            };

            ctx.env.insert_var(
                name.clone(),
                VarInfo {
                    ty: final_ty,
                    is_mut: matches!(stmt, Stmt::LetDecl(Decl::LetMut(..))),
                },
            );
        }

        Stmt::ExprStmt(expr) => {
            check_expr(expr, ctx)?;
        }

        Stmt::Return(expr) => {
            let ty = check_expr(expr, ctx)?;
            if expected_ret.as_ref() != Some(&ty) {
                return Err(TypeError::InvalidReturn);
            }
        }

        Stmt::If(ifstmt) => {
            let cond_ty = check_expr(&ifstmt.cond, ctx)?;
            if cond_ty != CanonType::Bool {
                return Err(TypeError::NonBoolCondition);
            }

            ctx.env.push_scope();
            check_block(&ifstmt.then_branch, ctx, expected_ret)?;
            ctx.env.pop_scope();

            if let Some(else_b) = &ifstmt.else_branch {
                ctx.env.push_scope();
                check_block(else_b, ctx, expected_ret)?;
                ctx.env.pop_scope();
            }
        }

        _ => {}
    }

    Ok(())
}

fn check_block(
    block: &Block,
    ctx: &mut TypeCtx,
    expected_ret: &Option<CanonType>,
) -> Result<(), TypeError> {
    for stmt in &block.statements {
        check_stmt(stmt, ctx, expected_ret)?;
    }

    if let Some(expr) = &block.implicit_return {
        let ty = check_expr(expr, ctx)?;
        if expected_ret.as_ref() != Some(&ty) {
            return Err(TypeError::InvalidReturn);
        }
    }

    Ok(())
}

fn canon_type(ty: &TypeExpr) -> CanonType {
    match ty {
        TypeExpr::Int64 => CanonType::Int,
        TypeExpr::Float64 => CanonType::Float,
        TypeExpr::Bool => CanonType::Bool,
        TypeExpr::String => CanonType::String,
        TypeExpr::Tuple(ts) => CanonType::Tuple(ts.iter().map(canon_type).collect()),
        TypeExpr::Reference(t) => CanonType::Ref(Box::new(canon_type(t)), false),
        TypeExpr::MutableReference(t) => CanonType::Ref(Box::new(canon_type(t)), true),
        TypeExpr::Custom(name) => CanonType::Struct(name.clone()),
        TypeExpr::Generic(name, _) => CanonType::Generic(name.clone()),
        _ => CanonType::Unknown,
    }
}

fn uncanon_type(ty: &CanonType) -> TypeExpr {
    match ty {
        CanonType::Int => TypeExpr::Int64,
        CanonType::Float => TypeExpr::Float64,
        CanonType::Bool => TypeExpr::Bool,
        CanonType::String => TypeExpr::String,
        CanonType::Unit => TypeExpr::Tuple(vec![]),
        CanonType::Tuple(ts) => TypeExpr::Tuple(ts.iter().map(uncanon_type).collect()),
        CanonType::Ref(t, false) => TypeExpr::Reference(Box::new(uncanon_type(t))),
        CanonType::Ref(t, true) => TypeExpr::MutableReference(Box::new(uncanon_type(t))),
        CanonType::Struct(n) => TypeExpr::Custom(n.clone()),
        CanonType::Enum(n) => TypeExpr::Custom(n.clone()),
        CanonType::Generic(n) => TypeExpr::Custom(n.clone()),
        CanonType::Unknown => TypeExpr::Custom("_".into()),
    }
}

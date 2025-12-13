use crate::ast::*;
use std::collections::HashMap;

// Symbol Tables
#[derive(Clone)]
struct VarInfo {
    ty: TypeExpr,
    is_mut: bool,
}

#[derive(Clone)]
struct FnSig {
    params: Vec<TypeExpr>,
    ret: Option<TypeExpr>,
    is_async: bool,
}

struct TypeEnv {
    vars: Vec<HashMap<String, VarInfo>>, // scoped
    functions: HashMap<String, FnSig>,
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

pub fn collect_decls(program: &Program) -> Result<TypeEnv, Vec<TypeError>> {
    let mut env = TypeEnv::new();
    let mut errors = vec![];

    for decl in &program.decls {
        if let Decl::Function(name, params, ret, _) | Decl::AsyncFunction(name, params, ret, _) =
            decl
        {
            if env.functions.contains_key(name) {
                errors.push(TypeError::DuplicateFunction(name.clone()));
                continue;
            }

            let flat_params = params
                .iter()
                .flatten()
                .map(|p| p.ty.clone().unwrap_or(TypeExpr::Custom("_".into())))
                .collect();

            env.functions.insert(
                name.clone(),
                FnSig {
                    params: flat_params,
                    ret: ret.clone(),
                    is_async: matches!(decl, Decl::AsyncFunction(..)),
                },
            );
        }
    }

    if errors.is_empty() {
        Ok(env)
    } else {
        Err(errors)
    }
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            vars: vec![HashMap::new()],
            functions: HashMap::new(),
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

pub fn type_check(program: &Program) -> Result<(), Vec<TypeError>> {
    let mut env = collect_decls(program)?;
    let mut errors = vec![];

    for decl in &program.decls {
        if let Decl::Function(_, params, ret, body) | Decl::AsyncFunction(_, params, ret, body) =
            decl
        {
            env.push_scope();

            for group in params {
                for param in group {
                    env.insert_var(
                        param.name.clone(),
                        VarInfo {
                            ty: param.ty.clone().unwrap(),
                            is_mut: param.is_mut,
                        },
                    );
                }
            }

            if let Err(e) = check_block(body, &mut env, ret) {
                errors.push(e);
            }

            env.pop_scope();
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_expr(expr: &Expr, env: &mut TypeEnv) -> Result<TypeExpr, TypeError> {
    use Expr::*;

    Ok(match expr {
        Identifier(name) => env
            .lookup_var(name)
            .ok_or_else(|| TypeError::UnknownIdentifier(name.clone()))?
            .ty
            .clone(),

        Expr::Number(crate::ast::Number::Int(_)) => TypeExpr::Int64,
        Expr::Number(crate::ast::Number::Float(_)) => TypeExpr::Float64,
        String(_) => TypeExpr::String,

        Reference(e) => TypeExpr::Reference(Box::new(check_expr(e, env)?)),
        MutReference(e) => TypeExpr::MutableReference(Box::new(check_expr(e, env)?)),

        Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => {
            let ta = check_expr(a, env)?;
            let tb = check_expr(b, env)?;
            if ta == tb {
                ta
            } else {
                return Err(TypeError::TypeMismatch {
                    expected: ta,
                    found: tb,
                });
            }
        }

        Call(callee, args) => {
            if let Expr::Identifier(name) = &**callee {
                let sig = env
                    .functions
                    .get(name)
                    .cloned()
                    .ok_or_else(|| TypeError::InvalidCall(name.clone()))?;

                if sig.params.len() != args.len() {
                    return Err(TypeError::InvalidCall(name.clone()));
                }

                for (arg, param_ty) in args.iter().zip(&sig.params) {
                    let arg_ty = check_expr(arg, env)?;
                    if &arg_ty != param_ty {
                        return Err(TypeError::TypeMismatch {
                            expected: param_ty.clone(),
                            found: arg_ty,
                        });
                    }
                }

                sig.ret.clone().unwrap_or(TypeExpr::Tuple(vec![]))
            } else {
                return Err(TypeError::InvalidCall("<expr>".into()));
            }
        }

        _ => TypeExpr::Custom("_".into()), // placeholder for future features
    })
}

fn check_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    expected_ret: &Option<TypeExpr>,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::LetDecl(Decl::Let(name, ty, expr)) => {
            let expr_ty = check_expr(expr, env)?;
            let final_ty = ty.clone().unwrap_or(expr_ty.clone());

            env.insert_var(
                name.clone(),
                VarInfo {
                    ty: final_ty,
                    is_mut: false,
                },
            );
        }

        Stmt::LetDecl(Decl::LetMut(name, ty, expr)) => {
            let expr_ty = check_expr(expr, env)?;
            let final_ty = ty.clone().unwrap_or(expr_ty.clone());

            env.insert_var(
                name.clone(),
                VarInfo {
                    ty: final_ty,
                    is_mut: true,
                },
            );
        }

        Stmt::ExprStmt(expr) => {
            check_expr(expr, env)?;
        }

        Stmt::Return(expr) => {
            let ty = check_expr(expr, env)?;
            if expected_ret.as_ref() != Some(&ty) {
                return Err(TypeError::InvalidReturn);
            }
        }

        Stmt::If(ifstmt) => {
            let cond_ty = check_expr(&ifstmt.cond, env)?;
            if cond_ty != TypeExpr::Bool {
                return Err(TypeError::NonBoolCondition);
            }

            env.push_scope();
            check_block(&ifstmt.then_branch, env, expected_ret)?;
            env.pop_scope();

            if let Some(else_b) = &ifstmt.else_branch {
                env.push_scope();
                check_block(else_b, env, expected_ret)?;
                env.pop_scope();
            }
        }

        _ => {}
    }

    Ok(())
}

fn check_block(
    block: &Block,
    env: &mut TypeEnv,
    expected_ret: &Option<TypeExpr>,
) -> Result<(), TypeError> {
    for stmt in &block.statements {
        check_stmt(stmt, env, expected_ret)?;
    }

    if let Some(expr) = &block.implicit_return {
        let ty = check_expr(expr, env)?;
        if expected_ret.as_ref() != Some(&ty) {
            return Err(TypeError::InvalidReturn);
        }
    }

    Ok(())
}

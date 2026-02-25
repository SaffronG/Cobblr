use crate::ast::Decl;
use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeEnum {
    Fn,
    Atomic,
    Expression,
    Statement,
    Decl,
    Trait,
    Import,
    Undeclared,
}

struct TcEnv {
    TypeScopes: HashMap<String, Option<TypeEnum>>,
    UserTypes: HashMap<String, TypeEnum>,
}

impl TcEnv {
    fn init() -> TcEnv {
        TcEnv {
            TypeScopes: HashMap::new(),
            UserTypes: HashMap::new(),
        }
    }
    fn drop_scope(&mut self) {
        self.TypeScopes.clear();
    }
    fn push_scope(&mut self, name: String) {
        self.TypeScopes.insert(name, None);
    }
    fn push_name(&mut self, name: String, ty: TypeEnum) {
        match self.TypeScopes.insert(name, Some(ty)) {
            Some(_) => panic!("Name already exists in the current scope"),
            None => {}
        }
    }
    fn lookup_name(&self, name: &String) -> Option<TypeEnum> {
        self.TypeScopes.get(name).cloned().flatten()
    }
    // Tries to represent inference rules Γ⊢x: t | Γ(x)=t
    fn it_follows_that(&self, name: &String, has_type: TypeEnum) -> bool { 
        // |- : it follows that expression has type b -> T or F
        match self.lookup_name(name) {
            Some(ty) => ty == has_type,
            None => false,
        } 
    }
    fn define_axiom(&mut self, name: String, ty: TypeEnum) {
        self.UserTypes.insert(name, ty);
    }
}

pub fn type_check(root: &Program) -> Program {
    let mut type_env: TcEnv = TcEnv::init();
    let prog = Program { decls: Vec::new() };
    check_decl(&mut type_env, &root.decls);
    prog
}

fn check_decl(type_env: &mut TcEnv, decls: &Vec<Decl>) {
    for declaration in decls {
        match declaration {
            Decl::Function(name, params, opt_ret, fn_body) => {
                check_fn(type_env, name.clone(), &params, &opt_ret, &fn_body)
            }
            //Decl::AsyncFunction(name, params, opt_ret, fn_body) => {}
            //Decl::Struct(name, ty_params, members) => {}
            //Decl::Enum(name, fields) => {}
            //Decl::Trait(name, methods) => {}
            //Decl::Impl(Name, decls) => {}
            //Decl::ImplTrait(trait_name, target, decls) => {}
            //Decl::Let(name, opt_type_expr, expr) => {}
            //Decl::LetMut(name, opt_type_expr, expr) => {}
            //Decl::Import(path) => {}
            //Decl::DerivedStruct(names, decls) => {}
            _ => {}
        }
    }
}

fn check_fn(
    scoped_env: &mut TcEnv,
    name: String,
    param_type: &Vec<Vec<Param>>,
    ret_expr: &Option<TypeExpr>,
    body: &Block,
) -> () {
    match ret_expr {
        Some(type_expr) => {
            scoped_env.push_name(name, eval(&type_expr));
        }
        None => {}
    }
}

fn eval(_type_expr: &TypeExpr) -> TypeEnum {
    TypeEnum::Undeclared
}

//fn check_struct(struct_repr: Decl::Struct) {}

//fn check_enum(enum_repr: Decl::Enum) {}

//fn check_trait(trait_repr: Decl::Trait) {}

//fn check_impl(implement: Decl::Impl) {}

//fn check_impl_trait(impl_trait: Decl::ImplTrait) {}

//fn check_let(let_repr: Decl::Let) {}

//fn check_let_mut(let_repr: Decl::LetMut) {}

//fn check_import(import: Decl::Import) {}

//fn check_derivert(derived: Decl::DerivedStruct) {}

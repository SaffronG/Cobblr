pub mod ast;
#[allow(warnings)]
pub mod cobblr;
pub mod lexer;

#[allow(clippy::all)]
//use cobblr::ExprParser;
//use logos::Lexer;

pub fn parse_source(source: &str) -> Result<crate::ast::Expr, String> {
    let lexer = logos: Lexer::new(source);
    let parser = ExprParser::new();

    parser
        .parse(source)
        .map_err(|e| format!("Parse error: {:?}", e))
}

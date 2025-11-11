mod ast;
mod lexer;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub cobblr);

use crate::ast::*;
use cobblr::ProgramParser;

fn main() {
    let input = r#"
        let x = 5;
        let y = Some(x + 3);
    "#;

    let lexer = Lexer::Lexer::new(input);

    let tokens: Vec<_> = lexer.filter_map(Result::ok).collect();

    match cobblr::ProgramParser::new().parse(tokens) {
        Ok(ast) => println!("AST:\n{ast:#?}"),
        Err(e) => eprintln!("Parse error: {e:?}"),
    }
}

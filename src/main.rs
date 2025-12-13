pub mod ast;
pub mod lexer;
pub mod tokens;
pub mod type_checker;

#[macro_use]
extern crate lalrpop_util;

use lexer::Lexer;
use std::env;
use std::fs;
use std::process;

lalrpop_mod!(cobblr);

fn main() {
    let args: Vec<String> = env::args().collect();

    let source = if args.len() > 1 {
        let path = &args[1];
        match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Failed to read file '{}': {}", path, e);
                process::exit(1);
            }
        }
    } else {
        println!("Enter Cobblr code, then press Ctrl+D:");
        let mut buf = String::new();
        if let Err(e) = std::io::stdin().read_line(&mut buf) {
            eprintln!("Failed to read stdin: {}", e);
            process::exit(1);
        }
        buf
    };

    println!("--- Lexing & Parsing ---");

    let parser = cobblr::ProgramParser::new();
    let lexer = Lexer::new(&source); // produces Iterator<Item=(usize, Result<Token, LexicalError>, usize)>

    match parser.parse(lexer) {
        Ok(program) => {
            println!("--- Parse Success ---");
            println!("{:#?}", program);
        }
        Err(err) => {
            println!("--- Parse Error ---");
            eprintln!("{:?}", err);
            process::exit(1);
        }
    }
}

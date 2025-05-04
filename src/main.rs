mod ast;
mod lexer;
mod parser;

use std::path::PathBuf;
use std::fs;
use std::env;
use std::process;

use lexer::Lexer;
use parser::Parser;


fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 1 || args.len() > 2 {
        println!("Usage: {} <entry_file>", args[0]);
        process::exit(64);
    }
    
    let entry_file = &args[1];
    
    match run::<str>(entry_file) {
        Ok(_) => {
            println!("Successfully parsed {}", entry_file);
        },
        Err(error) => {
            eprintln!("Error: {}", error);
            process::exit(70);
        }
    }
}

fn run<T: ?Sized>(file: &str) -> Result<(), Box<dyn std::error::Error>> where str: AsRef<T> {

    let path = PathBuf::from(file);

    let source =  fs::read_to_string(path)?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.scan_tokens()?;
    
    let mut parser = Parser::new(tokens);
    parser.attach_source(&source);
    let ast = parser.parse()?;
    
    println!("AST: {:#?}", ast);
    
    Ok(())
}

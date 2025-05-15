mod ast;
mod lexer;
mod parser;
mod unparser;
mod grammar;

use std::path::PathBuf;
use std::fs;
use std::env;
use std::process;

use lexer::Lexer;
use parser::Parser;
use unparser::{Unparser, FormatStyle};

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 1 || args.len() > 2 {
        println!("Usage: {} <entry_file>", args[0]);
        process::exit(64);
    }
    
    let entry_file = &args[1];
    
    match run::<str>(entry_file) {
        Ok(_) => {
            println!("Successfully parsed and unparsed {}", entry_file);
        },
        Err(error) => {
            eprintln!("Error: {}", error);
            process::exit(70);
        }
    }
}

fn run<T: ?Sized>(file: &str) -> Result<(), Box<dyn std::error::Error>> where str: AsRef<T> {
    let path = PathBuf::from(file);
    let source = fs::read_to_string(path)?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.scan_tokens()?;

    println!("Tokens: {:#?}", tokens);

    let mut parser = Parser::new(&tokens);
    parser.attach_source(&source);
    let ast = parser.parse_script()?;

    println!("AST: {:#?}", ast);

    let mut pretty_unparser = Unparser::new(FormatStyle::Pretty { indent_size: 2 });

    let pretty_code = pretty_unparser.unparse_script(&ast);

    let mut compact_unparser = Unparser::new(FormatStyle::Compact);

    let compact_code = compact_unparser.unparse_script(&ast);
    
    println!("\nPretty JavaScript code:");
    println!("{}", pretty_code);
    
    println!("\nCompact JavaScript code:");
    println!("{}", compact_code);

    println!("\n\n");
    
    Ok(())
}

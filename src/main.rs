mod core;
mod lexer;
mod parse;
use std::env;
use std::fs;
use std::io::{self, Write};

use anyhow::Result;
use parse::Parser;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return Ok(());
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            // Uncomment this block to pass the first stage
            if !file_contents.is_empty() {
                let (token_stream, errors) = lexer::scan(&file_contents);
                println!("{}", token_stream);
                if !errors.is_empty() {
                    for e in errors {
                        eprintln!("{}", e);
                    }
                    std::process::exit(65);
                }
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });
            let (tokens, errors) = lexer::scan(&file_contents);
            if !errors.is_empty() {
                for e in errors {
                    eprintln!("{}", e);
                }
                std::process::exit(65);
            }
            match Parser::new(tokens).parse_all() {
                Ok((errors, ast)) => {
                    println!("{}", ast);
                    for e in &errors {
                        eprintln!("{}", e);
                    }
                    if !errors.is_empty() {
                        std::process::exit(65);
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(65);
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return Ok(());
        }
    }
    Ok(())
}

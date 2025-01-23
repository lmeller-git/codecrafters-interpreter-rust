mod core;
mod eval;
mod lexer;
mod parse;
use anyhow::Result;
use eval::tree_walk::TreeWalker;
use eval::Interpreter;
use parse::Parser;
use std::env;
use std::fs;

//TODO rewrite this
fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return Ok(());
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
                eprintln!("Failed to read file {}", filename);
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
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
                    for e in &errors {
                        eprintln!("{}", e);
                    }
                    if !errors.is_empty() {
                        std::process::exit(65);
                    }
                    // evaluate
                    let (res, errs) = Interpreter::new(ast, TreeWalker::new()).interpret()?;
                    for r in res {
                        println!("{}", r);
                    }
                    for e in &errs {
                        eprintln!("{}", e);
                    }
                    if !errs.is_empty() {
                        std::process::exit(70);
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(70);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return Ok(());
        }
    }
    Ok(())
}

pub mod ast;
pub mod expr;
use std::fmt::Display;

use crate::lexer::lexing_utils::{Token, TokenStream};

// takes a TokenStream and parses it into an AST via recursive descent
pub struct Parser<T: Iterator<Item = Token>> {
    tokens: std::iter::Peekable<TokenStream<T>>,
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken(Token),
    UnexpectedNone,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidToken(token) => {
                write!(f, "[line {}] Error at '{:?}'", token.line(), token.kind)
            }
            ParseError::UnexpectedNone => write!(f, "Unexpected None"),
        }
    }
}

impl std::error::Error for ParseError {}

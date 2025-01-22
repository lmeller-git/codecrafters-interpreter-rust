pub mod ast;
pub mod expr;
use crate::lexer::lexing_utils::{Token, TokenStream};
use anyhow::Result;
use thiserror::Error;

// takes a TokenStream and parses it into an AST via recursive descent
pub struct Parser<T: Iterator<Item = Token>> {
    tokens: std::iter::Peekable<TokenStream<T>>,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid token {0:?}")]
    InvalidToken(Token),
    #[error("Unexpected None")]
    UnexpectedNone,
}

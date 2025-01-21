use lexing_utils::{Token, TokenStream, TokenType};
use thiserror::Error;
mod lexing_utils;

pub fn scan(data: &str) -> (TokenStream, Vec<LexingError>) {
    let mut tokens = TokenStream::new();
    let mut errors: Vec<LexingError> = Vec::new();
    let mut chars = data.chars().peekable();
    let mut line = 1;
    while let Some(next_char) = chars.next() {
        match next_char {
            '\n' => line += 1,
            '(' => tokens.push(Token::new(TokenType::OpenParen, line)),
            ')' => tokens.push(Token::new(TokenType::CloseParen, line)),
            '[' => tokens.push(Token::new(TokenType::OpenBracket, line)),
            ']' => tokens.push(Token::new(TokenType::CloseBracket, line)),
            '{' => tokens.push(Token::new(TokenType::OpenBrace, line)),
            '}' => tokens.push(Token::new(TokenType::CloseBrace, line)),
            '+' => match chars.peek() {
                Some('=') => errors.push(LexingError::NotImplemented("add assign".into())),
                _ => tokens.push(Token::new(TokenType::Plus, line)),
            },
            '-' => match chars.peek() {
                Some('=') => errors.push(LexingError::NotImplemented("sub assign".into())),
                _ => tokens.push(Token::new(TokenType::Minus, line)),
            },

            '*' => match chars.peek() {
                Some('=') => errors.push(LexingError::NotImplemented("mul assign".into())),
                _ => tokens.push(Token::new(TokenType::Star, line)),
            },
            ',' => tokens.push(Token::new(TokenType::Comma, line)),
            '.' => tokens.push(Token::new(TokenType::Dot, line)),
            ';' => tokens.push(Token::new(TokenType::Semi, line)),
            '=' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token::new(TokenType::EqEq, line))
                }
                _ => tokens.push(Token::new(TokenType::Eq, line)),
            },
            c if c.is_whitespace() => {}
            _ => errors.push(LexingError::UnknownCharacter(
                line,
                TokenType::Unknown(next_char),
            )),
        }
    }
    tokens.push(Token::new(TokenType::Eof, line));
    (tokens, errors)
}

#[derive(Error, Debug)]
pub enum LexingError {
    #[error("[line {0:?}] Error: Unexpected character: {1:?}")]
    UnknownCharacter(usize, TokenType),
    #[error("{0} not implemented")]
    NotImplemented(String),
}

use crate::core::types::{LoxString, Number};
use anyhow::Result;
use lexing_utils::{LiteralKind, Token, TokenStream, TokenType};
use std::str::{Chars, FromStr};
use thiserror::Error;
mod lexing_utils;

pub fn scan(data: &str) -> (TokenStream, Vec<anyhow::Error>) {
    let mut tokens = TokenStream::new();
    let mut errors: Vec<anyhow::Error> = Vec::new();
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
                Some('=') => errors.push(LexingError::NotImplemented("add assign".into()).into()),
                _ => tokens.push(Token::new(TokenType::Plus, line)),
            },
            '-' => match chars.peek() {
                Some('=') => errors.push(LexingError::NotImplemented("sub assign".into()).into()),
                _ => tokens.push(Token::new(TokenType::Minus, line)),
            },

            '*' => match chars.peek() {
                Some('=') => errors.push(LexingError::NotImplemented("mul assign".into()).into()),
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
            '!' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token::new(TokenType::NEq, line))
                }
                _ => tokens.push(Token::new(TokenType::Bang, line)),
            },
            '>' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token::new(TokenType::GtEq, line))
                }
                _ => tokens.push(Token::new(TokenType::Gt, line)),
            },
            '<' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    tokens.push(Token::new(TokenType::LtEq, line))
                }
                _ => tokens.push(Token::new(TokenType::Lt, line)),
            },
            '/' => match chars.peek() {
                Some('/') => {
                    chars.next();
                    let comment = parse_comment(&mut chars, false, &mut line);
                    tokens.push(Token::new(TokenType::SingleLineComment(comment), line))
                }
                Some('*') => {
                    chars.next();
                    let comment = parse_comment(&mut chars, true, &mut line);
                    tokens.push(Token::new(TokenType::MultiLineComment(comment), line))
                }
                _ => tokens.push(Token::new(TokenType::Slash, line)),
            },
            '\"' => match parse_string_lit(&mut chars, &mut line) {
                Err(e) => errors.push(e),
                Ok(s) => tokens.push(Token::new(TokenType::Literal(LiteralKind::String(s)), line)),
            },
            c if c.is_whitespace() => {}
            c if c.is_ascii_digit() => match parse_num(&mut chars, c) {
                Err(e) => errors.push(e),
                Ok((n, s)) => tokens.push(Token::new(
                    TokenType::Literal(LiteralKind::Number(n, s)),
                    line,
                )),
            },
            _ => errors
                .push(LexingError::UnknownCharacter(line, TokenType::Unknown(next_char)).into()),
        }
    }
    tokens.push(Token::new(TokenType::Eof, line));
    (tokens, errors)
}

fn parse_comment(
    chars: &mut std::iter::Peekable<Chars>,
    is_multiline: bool,
    line: &mut usize,
) -> String {
    let mut comment = String::new();
    while let Some(c) = chars.next() {
        match c {
            '\n' => {
                *line += 1;
                if !is_multiline {
                    return comment;
                }
                comment.push('\n');
            }
            '*' => {
                if is_multiline {
                    if let Some(n) = chars.next() {
                        if n == '/' {
                            return comment;
                        }
                        comment.push(n);
                    }
                } else {
                    comment.push(c);
                }
            }
            _ => comment.push(c),
        }
    }
    comment
}

fn parse_string_lit(chars: &mut std::iter::Peekable<Chars>, line: &mut usize) -> Result<LoxString> {
    let mut str_lit = String::new();
    for c in chars {
        match c {
            '\"' => return Ok(LoxString::from_str(&str_lit)?),
            '\n' => *line += 1,
            _ => str_lit.push(c),
        }
    }
    Err(LexingError::UnterminatedSequence(*line, "string".into()))?
}

fn parse_num(chars: &mut std::iter::Peekable<Chars>, first: char) -> Result<(Number, String)> {
    let mut acc = String::new();
    acc.push(first);
    while let Some(next) = chars.peek() {
        match next {
            '.' => {
                let mut peekable = chars.clone();
                peekable.next();
                match peekable.peek() {
                    Some(c) if c.is_ascii_digit() => {
                        acc.push(chars.next().unwrap());
                        acc.push(chars.next().unwrap());
                    }
                    _ => return Ok((Number::from_str(&acc)?, acc)),
                }
            }
            c if c.is_ascii_digit() => acc.push(chars.next().unwrap()),
            _ => return Ok((Number::from_str(&acc)?, acc)),
        }
    }
    Ok((Number::from_str(&acc)?, acc))
}

#[derive(Error, Debug)]
pub enum LexingError {
    #[error("[line {0:?}] Error: Unexpected character: {1:?}")]
    UnknownCharacter(usize, TokenType),
    #[error("{0} not implemented")]
    NotImplemented(String),
    #[error("[line {0:?}] Error: Unterminated {1}.")]
    UnterminatedSequence(usize, String),
}

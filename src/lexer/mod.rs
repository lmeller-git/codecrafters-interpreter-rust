use anyhow::{bail, Result};
use lexing_utils::{Token, TokenStream, TokenType};

mod lexing_utils;

pub fn scan(data: &str) -> Result<TokenStream> {
    let mut tokens = TokenStream::new();
    let mut chars = data.chars().peekable();
    let mut line = 0;
    while let Some(next_char) = chars.next() {
        match next_char {
            '\n' => line += 1,
            '(' => tokens.push(Token::new(TokenType::OpenParen, line)),
            ')' => tokens.push(Token::new(TokenType::CloseParen, line)),
            '[' => tokens.push(Token::new(TokenType::OpenBracket, line)),
            ']' => tokens.push(Token::new(TokenType::CloseBracket, line)),
            '{' => tokens.push(Token::new(TokenType::OpenBrace, line)),
            '}' => tokens.push(Token::new(TokenType::CloseBrace, line)),
            c if c.is_whitespace() => {}
            _ => bail!("unexpected char {}", next_char),
        }
    }
    tokens.push(Token::new(TokenType::Eof, line));
    Ok(tokens)
}

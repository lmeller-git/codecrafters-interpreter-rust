#![allow(dead_code)]

use std::{
    fmt::{Debug, Display},
    vec,
};

pub const EOF_CHAR: char = '\0';

#[derive(Debug)]
pub struct Token {
    kind: TokenType,
    line: usize,
}

impl Token {
    pub fn new(kind: TokenType, line: usize) -> Self {
        Self { kind, line }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenType::Unknown(_) => write!(
                f,
                "[line {}] Error: UNexpected character: {:?}",
                self.line, self.kind
            ),
            _ => write!(f, "{} null", self.kind),
        }
    }
}

pub enum TokenType {
    Ident,
    Literal(LiteralKind),
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `@`
    At,
    /// `#`
    Pound,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,
    /// '=='
    EqEq,
    /// ' '
    WhiteSpace,
    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown(char),
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpenParen => write!(f, "LEFT_PAREN ("),
            Self::CloseParen => write!(f, "RIGHT_PAREN )"),
            Self::OpenBrace => write!(f, "LEFT_BRACE {{"),
            Self::CloseBrace => write!(f, "RIGHT_BRACE }}"),
            Self::OpenBracket => write!(f, "LEFT_BRACKET ["),
            Self::CloseBracket => write!(f, "RIGHT_BRACKET ]"),
            Self::Semi => write!(f, "SEMICOLON ;"),
            Self::Plus => write!(f, "PLUS +"),
            Self::Minus => write!(f, "MINUS -"),
            Self::Star => write!(f, "STAR *"),
            Self::Dot => write!(f, "DOT ."),
            Self::Comma => write!(f, "COMMA ,"),
            Self::Eof => write!(f, "EOF "),
            Self::EqEq => write!(f, "EQUAL_EQUAL =="),
            Self::Eq => write!(f, "EQUAL ="),
            _ => write!(f, "Not implemented"),
        }?;
        Ok(())
    }
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(c) => write!(f, "{}", c),
            _ => write!(f, "valid"),
        }
    }
}

#[derive(Debug)]
pub enum LiteralKind {
    Number(Number),
    String(LoxString),
}

#[derive(Debug)]
pub struct Number {
    value: String,
}

#[derive(Debug)]
pub struct LoxString {
    value: String,
}

pub struct TokenStream {
    tokens: Vec<Token>,
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = vec::IntoIter<Token>;
    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

impl TokenStream {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token)
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
            writeln!(f, "{}", token)?;
        }
        Ok(())
    }
}

/*
pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
        }
    }

    pub fn peek_nth(&self, n: usize) -> char {
        let mut chars = self.chars.clone();
        chars.nth(n).unwrap_or(EOF_CHAR)
    }

    pub fn next() {}

}
*/

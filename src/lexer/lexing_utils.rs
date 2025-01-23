#![allow(dead_code)]
use std::fmt::{Debug, Display};

use crate::core::types::{num::Number, string::LoxString};

pub const EOF_CHAR: char = '\0';

#[derive(Debug, Clone, Default)]
pub struct Token {
    pub kind: TokenType,
    line: usize,
}

impl Token {
    pub fn new(kind: TokenType, line: usize) -> Self {
        Self { kind, line }
    }
    pub fn line(&self) -> usize {
        let Self { kind: _, line } = self;
        *line
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
            TokenType::Literal(_) => write!(f, "{}", self.kind),
            TokenType::SingleLineComment(_) | TokenType::MultiLineComment(_) => Ok(()),
            _ => write!(f, "{} null", self.kind),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenType {
    Ident(String),
    Literal(LiteralKind),
    /// ';'
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
    /// '//'
    SingleLineComment(String),
    /// '/**/'
    MultiLineComment(String),
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
    /// '!='
    NEq,
    /// '<='
    LtEq,
    /// '>='
    GtEq,
    /// ' '
    WhiteSpace,
    Keyword(Keyword),
    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown(char),
    Eof,
}

impl Default for TokenType {
    fn default() -> Self {
        Self::Keyword(Keyword::default())
    }
}

//TODO swap Debug and Display
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
            Self::NEq => write!(f, "BANG_EQUAL !="),
            Self::Bang => write!(f, "BANG !"),
            Self::Lt => write!(f, "LESS <"),
            Self::Gt => write!(f, "GREATER >"),
            Self::GtEq => write!(f, "GREATER_EQUAL >="),
            Self::LtEq => write!(f, "LESS_EQUAL <="),
            Self::Slash => write!(f, "SLASH /"),
            Self::Ident(id) => write!(f, "IDENTIFIER {}", id),
            Self::Keyword(key) => write!(f, "{:?}", key),
            Self::Literal(lit) => write!(f, "{}", lit),
            Self::MultiLineComment(_) | Self::SingleLineComment(_) => Ok(()),
            _ => write!(f, "Not implemented"),
        }?;
        Ok(())
    }
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(c) => write!(f, "{}", c),
            Self::Keyword(key) => write!(f, "{}", key),
            Self::Literal(lit) => write!(f, "{:?}", lit),
            Self::Bang => write!(f, "!"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Plus => write!(f, "+"),
            Self::Gt => write!(f, ">"),
            Self::GtEq => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::LtEq => write!(f, "<="),
            Self::EqEq => write!(f, "=="),
            Self::NEq => write!(f, "!="),
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            _ => write!(f, "not implemented: {}", self),
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub enum Keyword {
    /// 'and'
    And,
    /// 'class'
    Class,
    /// 'else'
    Else,
    /// 'false'
    False,
    /// 'for'
    For,
    /// 'fun'
    Fun,
    /// 'if'
    If,
    /// 'nil'
    #[default]
    Nil,
    /// 'or'
    Or,
    /// 'print'
    Print,
    /// 'return'
    Return,
    /// 'super'
    Super,
    /// 'this'
    This,
    /// 'true'
    True,
    /// 'var'
    Var,
    /// 'while'
    While,
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::And => write!(f, "AND and"),
            Keyword::Class => write!(f, "CLASS class"),
            Keyword::Else => write!(f, "ELSE else"),
            Keyword::False => write!(f, "FALSE false"),
            Keyword::For => write!(f, "FOR for"),
            Keyword::Fun => write!(f, "FUN fun"),
            Keyword::If => write!(f, "IF if"),
            Keyword::Nil => write!(f, "NIL nil"),
            Keyword::Or => write!(f, "OR or"),
            Keyword::Print => write!(f, "PRINT print"),
            Keyword::Return => write!(f, "RETURN return"),
            Keyword::Super => write!(f, "SUPER super"),
            Keyword::This => write!(f, "THIS this"),
            Keyword::True => write!(f, "TRUE true"),
            Keyword::Var => write!(f, "VAR var"),
            Keyword::While => write!(f, "WHILE while"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::And => write!(f, "and"),
            Keyword::Class => write!(f, "class"),
            Keyword::Else => write!(f, "else"),
            Keyword::False => write!(f, "false"),
            Keyword::For => write!(f, "for"),
            Keyword::Fun => write!(f, "fun"),
            Keyword::If => write!(f, "if"),
            Keyword::Nil => write!(f, "nil"),
            Keyword::Or => write!(f, "or"),
            Keyword::Print => write!(f, "print"),
            Keyword::Return => write!(f, "return"),
            Keyword::Super => write!(f, "super"),
            Keyword::This => write!(f, "this"),
            Keyword::True => write!(f, "true"),
            Keyword::Var => write!(f, "var"),
            Keyword::While => write!(f, "while"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum LiteralKind {
    Number(Number, String),
    String(LoxString),
}

//TODO swap Display and Debug
impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num, s) => write!(f, r#"NUMBER {} {}"#, s, num.as_floating_point_str())?,
            Self::String(string) => write!(f, r#"STRING "{}" {}"#, string, string)?,
        }
        Ok(())
    }
}

impl Debug for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num, _) => write!(f, "{}", num.as_floating_point_str())?,
            Self::String(s) => write!(f, "{}", s)?,
        }
        Ok(())
    }
}

// wrapper around an Iterator over Tokens
#[derive(Clone, Debug, Default)]
pub struct TokenStream<T: Iterator<Item = Token>> {
    tokens: T,
}

impl<T: Iterator<Item = Token>> TokenStream<T> {
    pub fn new(tokens: T) -> Self {
        Self { tokens }
    }
}

impl<T: Iterator<Item = Token>> Iterator for TokenStream<T> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl<T: Iterator<Item = Token>> Display for TokenStream<T>
where
    T: Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.tokens.clone() {
            let format = format!("{}", token);
            if !format.is_empty() {
                writeln!(f, "{}", format)?;
            }
        }
        Ok(())
    }
}

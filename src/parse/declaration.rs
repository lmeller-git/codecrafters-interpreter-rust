use std::fmt::Display;

use crate::lexer::lexing_utils::{Keyword, Token, TokenType};

use super::{
    ast::{Parseable, Visitable, Visitor},
    expr::Expr,
    stmt::Stmt,
    ParseError,
};

#[derive(Debug)]
pub enum Declaration {
    Var(VarDecl),
    Stmt(Stmt),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(v) => write!(f, "{}", v),
            Self::Stmt(s) => write!(f, "{}", s),
        }
    }
}

impl Default for Declaration {
    fn default() -> Self {
        Self::Stmt(Stmt::default())
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for Declaration {
    fn try_parse(stream: &mut crate::lexer::lexing_utils::TokenStream<T>) -> anyhow::Result<Self> {
        match stream.peek() {
            Some(token) => match token.kind {
                TokenType::Keyword(Keyword::Var) => {
                    stream.next();
                    Ok(Self::Var(VarDecl::try_parse(stream)?))
                }
                _ => Ok(Self::Stmt(Stmt::try_parse(stream)?)),
            },
            None => Err(ParseError::UnexpectedNone.into()),
        }
    }
}

impl<V: Visitor> Visitable<V> for Declaration {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_declaration(self)
    }
}

#[derive(Default, Debug)]
pub struct VarDecl {
    pub ident: String,
    pub value: Option<Expr>,
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {:#?}", self.ident, self.value)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for VarDecl {
    fn try_parse(stream: &mut crate::lexer::lexing_utils::TokenStream<T>) -> anyhow::Result<Self> {
        let ident = match stream.peek() {
            Some(token) => match &token.kind {
                TokenType::Ident(ident) => {
                    let ident = ident.clone();
                    _ = stream.next().unwrap();
                    match stream.peek() {
                        Some(token) if token.kind == TokenType::Eq => _ = stream.next(),
                        Some(token) if token.kind == TokenType::Semi => {}
                        Some(token) => return Err(ParseError::InvalidToken(token.clone()).into()),
                        None => return Err(ParseError::UnexpectedNone.into()),
                    }
                    ident
                }
                _ => return Err(ParseError::InvalidToken(token.clone()).into()),
            },
            None => return Err(ParseError::UnexpectedNone.into()),
        };
        let expr = match Expr::try_parse(stream) {
            Ok(expr) => Some(expr),
            Err(_) => None,
        };
        match stream.peek() {
            Some(token) if token.kind == TokenType::Semi => _ = stream.next(),
            _ => {}
        }
        Ok(Self { ident, value: expr })
    }
}

impl<V: Visitor> Visitable<V> for VarDecl {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_vardecl(self)
    }
}

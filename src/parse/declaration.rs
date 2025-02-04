use std::fmt::Display;

use crate::lexer::lexing_utils::{Keyword, Token, TokenType};

use super::{
    ast::{Parseable, Visitable, Visitor},
    expr::Expr,
    stmt::Stmt,
    ParseError,
};

#[derive(Debug, Clone)]
pub enum Declaration {
    Var(VarDecl),
    Stmt(Stmt),
    Fn(FnDecl),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(v) => write!(f, "{}", v),
            Self::Stmt(s) => write!(f, "{}", s),
            Self::Fn(fun) => write!(f, "{}", fun),
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
                TokenType::Keyword(Keyword::Fun) => {
                    stream.next();
                    Ok(Self::Fn(FnDecl::try_parse(stream)?))
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

#[derive(Default, Debug, Clone)]
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

#[derive(Debug, Default, Clone)]
pub struct FnDecl {
    pub ident: String,
    pub args: Vec<String>,
    pub body: Box<Declaration>,
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.ident)
    }
}

impl<V: Visitor> Visitable<V> for FnDecl {
    fn accept(&self, visitor: &mut V) -> <V as Visitor>::Output {
        visitor.visit_fndecl(self)
    }
}

impl<T: Iterator<Item = Token> + Clone> Parseable<T> for FnDecl {
    fn try_parse(stream: &mut crate::lexer::lexing_utils::TokenStream<T>) -> anyhow::Result<Self> {
        let ident = if let Some(t) = stream.peek() {
            match t.kind {
                TokenType::Ident(s) => {
                    stream.next();
                    s.to_string()
                }
                _ => return Err(ParseError::InvalidToken(t).into()),
            }
        } else {
            return Err(ParseError::UnexpectedNone.into());
        };
        if let Some(t) = stream.peek() {
            if t.kind == TokenType::OpenParen {
                _ = stream.next();
            }
        }
        let mut args = Vec::new();
        while let Some(t) = stream.peek() {
            match t.kind {
                TokenType::Ident(s) => {
                    stream.next();
                    args.push(s.to_string());
                    if let Some(n) = stream.peek() {
                        if n.kind != TokenType::Comma && n.kind != TokenType::CloseParen {
                            return Err(ParseError::InvalidToken(n.clone()).into());
                        }
                    }
                }
                TokenType::CloseParen => {
                    stream.next();
                    break;
                }
                TokenType::Comma => _ = stream.next(),
                _ => return Err(ParseError::InvalidToken(t).into()),
            }
        }
        if let Some(t) = stream.peek() {
            if t.kind != TokenType::OpenBrace {
                return Err(ParseError::InvalidToken(t.clone()).into());
            }
        }
        let body = Box::new(Declaration::try_parse(stream)?);
        Ok(Self { ident, args, body })
    }
}

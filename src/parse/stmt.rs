use std::fmt::Display;

use crate::lexer::lexing_utils::{Keyword, Token, TokenType};

use super::{
    ast::{Parseable, Visitable, Visitor},
    expr::Expr,
    ParseError,
};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(PrintStmt),
}

impl Default for Stmt {
    fn default() -> Self {
        Self::Expr(Expr::default())
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(e) => write!(f, "{}", e),
            Self::Print(p) => write!(f, "{}", p),
        }
    }
}

impl<T: Iterator<Item = Token>> Parseable<T> for Stmt {
    fn try_parse(
        stream: &mut std::iter::Peekable<crate::lexer::lexing_utils::TokenStream<T>>,
    ) -> anyhow::Result<Self> {
        match stream.peek() {
            None => Err(ParseError::UnexpectedNone.into()),
            Some(t) => match t.kind {
                TokenType::Keyword(k) => match k {
                    Keyword::Print => {
                        stream.next();
                        Ok(Self::Print(PrintStmt::try_parse(stream)?))
                    }
                    Keyword::False | Keyword::Nil | Keyword::True => {
                        let expr = Self::Expr(Expr::try_parse(stream)?);
                        if let Some(token) = stream.peek() {
                            if token.kind == TokenType::Semi {
                                stream.next();
                            }
                        }
                        Ok(expr)
                    }
                    _ => Err(ParseError::InvalidToken(t.clone()).into()),
                },
                _ => {
                    let expr = Self::Expr(Expr::try_parse(stream)?);
                    if let Some(token) = stream.peek() {
                        if token.kind == TokenType::Semi {
                            stream.next();
                        }
                    }
                    Ok(expr)
                }
            },
        }
    }
}

impl<V: Visitor> Visitable<V> for Stmt {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_stmt(self)
    }
}

#[derive(Default, Debug)]
pub struct PrintStmt {
    pub args: Expr,
}

impl Display for PrintStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.args)
    }
}

impl<T: Iterator<Item = Token>> Parseable<T> for PrintStmt {
    fn try_parse(
        stream: &mut std::iter::Peekable<crate::lexer::lexing_utils::TokenStream<T>>,
    ) -> anyhow::Result<Self> {
        let expr = Expr::try_parse(stream)?;
        if let Some(token) = stream.peek() {
            if token.kind == TokenType::Semi {
                stream.next();
            } else {
                return Err(ParseError::InvalidToken(token.clone()).into());
            }
        } else {
            return Err(ParseError::UnexpectedNone.into());
        }
        let args = expr;
        Ok(Self { args })
    }
}

impl<V: Visitor> Visitable<V> for PrintStmt {
    fn accept(&self, visitor: &mut V) -> V::Output {
        visitor.visit_printstmt(self)
    }
}

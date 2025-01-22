use std::fmt::Display;

use super::{
    expr::{Comparison, Equality, Expr, Factor, Primary, Term, Unary},
    ParseError, Parser,
};
use crate::lexer::lexing_utils::{Token, TokenStream, TokenType};
use anyhow::Result;

#[derive(Default)]
pub struct Ast {
    exprs: Vec<Expr>,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for expr in &self.exprs {
            writeln!(f, "{}", expr)?;
        }
        Ok(())
    }
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: TokenStream<T>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }
    pub fn parse_one(&mut self) -> Result<Expr> {
        self.parse_expr()
    }
    pub fn parse_all(&mut self) -> Result<(Vec<anyhow::Error>, Ast)> {
        let mut errors = Vec::new();
        let mut ast = Ast::default();
        while let Some(next_token) = self.tokens.peek() {
            if next_token.kind == TokenType::Eof {
                break;
            }
            match self.parse_one() {
                Ok(expr) => ast.exprs.push(expr),
                Err(e) => errors.push(e),
            }
        }
        Ok((errors, ast))
    }
    fn parse_expr(&mut self) -> Result<Expr> {
        Expr::try_parse(&mut self.tokens)
    }
    fn sync(&mut self) {}
}

pub trait Visitable<V: Visitor>: Sized {
    fn accept(&mut self, visitor: &mut V);
}

pub trait Parseable<T>: Sized + Default
where
    T: Iterator<Item = Token>,
{
    fn try_parse(stream: &mut std::iter::Peekable<TokenStream<T>>) -> Result<Self>;
    fn parse_or_default(stream: &mut std::iter::Peekable<TokenStream<T>>) -> Self {
        Self::try_parse(stream).unwrap_or_else(|_| Self::default())
    }
}

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &Expr);
    fn visit_equality(&mut self, eq: &Equality);
    fn visit_comparison(&mut self, comp: &Comparison);
    fn visit_term(&mut self, term: &Term);
    fn visit_factor(&mut self, factor: &Factor);
    fn visit_unary(&mut self, unary: &Unary);
    fn visit_primary(&mut self, primary: &Primary);
}

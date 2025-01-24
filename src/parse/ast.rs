#![allow(dead_code)]
use std::fmt::Display;

use super::{
    declaration::{Declaration, VarDecl},
    expr::{Assignment, Comparison, Equality, Expr, Factor, Primary, Term, Unary},
    stmt::{PrintStmt, Stmt},
    Parser,
};
use crate::lexer::lexing_utils::{Keyword, Token, TokenStream, TokenType};
use anyhow::Result;

#[derive(Default)]
pub struct Ast {
    pub prog: Vec<Declaration>,
}

impl IntoIterator for Ast {
    type Item = Declaration;
    type IntoIter = std::vec::IntoIter<Declaration>;
    fn into_iter(self) -> Self::IntoIter {
        self.prog.into_iter()
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.prog {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl<T: Iterator<Item = Token> + Clone> Parser<T> {
    pub fn new(tokens: TokenStream<T>) -> Self {
        Self { tokens }
    }
    pub fn parse_one(&mut self) -> Result<Declaration> {
        self.parse_decl()
    }
    pub fn parse_all(&mut self) -> Result<(Vec<anyhow::Error>, Ast)> {
        let mut errors = Vec::new();
        let mut ast = Ast::default();
        while let Some(next_token) = self.tokens.peek() {
            if next_token.kind == TokenType::Eof {
                break;
            }
            match self.parse_one() {
                Ok(stmt) => {
                    ast.prog.push(stmt);
                }
                Err(e) => {
                    errors.push(e);
                    self.sync();
                }
            }
        }
        Ok((errors, ast))
    }
    fn parse_decl(&mut self) -> Result<Declaration> {
        Declaration::try_parse(&mut self.tokens)
    }
    fn parse_stmt(&mut self) -> Result<Stmt> {
        Stmt::try_parse(&mut self.tokens)
    }
    fn parse_expr(&mut self) -> Result<Expr> {
        Expr::try_parse(&mut self.tokens)
    }
    fn sync(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TokenType::Semi => {
                    self.tokens.next();
                    break;
                }
                TokenType::Keyword(
                    Keyword::Class
                    | Keyword::For
                    | Keyword::If
                    | Keyword::Fun
                    | Keyword::Print
                    | Keyword::Return
                    | Keyword::While
                    | Keyword::Var,
                ) => {
                    break;
                }
                TokenType::Eof => break,
                _ => _ = self.tokens.next(),
            }
        }
    }
}

pub trait Visitable<V: Visitor>: Sized {
    fn accept(&self, visitor: &mut V) -> V::Output;
}

pub trait Parseable<T>: Sized + Default
where
    T: Iterator<Item = Token> + Clone,
{
    fn try_parse(stream: &mut TokenStream<T>) -> Result<Self>;
    fn parse_or_default(stream: &mut TokenStream<T>) -> Self {
        Self::try_parse(stream).unwrap_or_else(|_| Self::default())
    }
}

pub trait Visitor: Sized {
    type Output;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_equality(&mut self, eq: &Equality) -> Self::Output;
    fn visit_comparison(&mut self, comp: &Comparison) -> Self::Output;
    fn visit_term(&mut self, term: &Term) -> Self::Output;
    fn visit_factor(&mut self, factor: &Factor) -> Self::Output;
    fn visit_unary(&mut self, unary: &Unary) -> Self::Output;
    fn visit_primary(&mut self, primary: &Primary) -> Self::Output;
    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output;
    fn visit_printstmt(&mut self, p_stmt: &PrintStmt) -> Self::Output;
    fn visit_declaration(&mut self, decl: &Declaration) -> Self::Output;
    fn visit_vardecl(&mut self, var_decl: &VarDecl) -> Self::Output;
    fn visit_assignment(&mut self, assignment: &Assignment) -> Self::Output;
}

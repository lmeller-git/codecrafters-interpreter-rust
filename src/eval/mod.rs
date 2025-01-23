pub mod tree_walk;
use anyhow::Result;
use thiserror::Error;
mod ops;
use crate::{
    core::types::LoxType,
    lexer::lexing_utils::TokenType,
    parse::{
        ast::{Ast, Visitor},
        stmt::Stmt,
    },
};
//TODO refactor parser and evaluator to use some kind of BinaryOp/UnaryOp enum instead of Token!!!!
pub struct Interpreter<T>
where
    T: Evaluator,
{
    ast: Ast,
    evaluator: T,
}

impl<T> Interpreter<T>
where
    T: Evaluator,
{
    pub fn interpret(&mut self) -> Result<(Vec<LoxType>, Vec<anyhow::Error>)> {
        let mut res = Vec::new();
        let mut errs = Vec::new();
        for stmt in &self.ast.prog {
            match self.evaluator.evaluate(stmt) {
                Ok(r) => res.push(r),
                Err(e) => {
                    errs.push(e);
                    // ???????????
                    return Ok((res, errs));
                }
            }
        }
        Ok((res, errs))
    }

    pub fn new(ast: Ast, evaluator: T) -> Self {
        Self { ast, evaluator }
    }
}

pub trait Evaluator: Sized + Visitor<Output = Result<LoxType>> {
    fn resolve_err(&mut self);
    fn evaluate(&mut self, stmt: &Stmt) -> Result<LoxType>;
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Operation {0} not supported between types")]
    IllegalOp(String),
    #[error("Operation {0} is not possible in this context")]
    ImpossibleOP(TokenType),
}

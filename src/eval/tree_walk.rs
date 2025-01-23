use super::{
    ops::{binary_ops, unary_ops},
    Evaluator, RuntimeError,
};
use crate::{
    core::types::LoxType,
    lexer::lexing_utils::{Keyword, LiteralKind, TokenType},
    parse::{
        ast::{Visitable, Visitor},
        expr::{Primary, Unary},
    },
};
use anyhow::Result;

pub struct TreeWalker {}

impl Evaluator for TreeWalker {
    fn resolve_err(&mut self) {}

    fn evaluate(
        &mut self,
        expression: &crate::parse::expr::Expr,
    ) -> anyhow::Result<crate::core::types::LoxType> {
        match expression.accept(self) {
            Ok(r) => Ok(r),
            //TODO
            Err(e) => Err(e), //Ok(LoxType::default()),
        }
    }
}

impl Visitor for TreeWalker {
    type Output = Result<LoxType>;
    fn visit_expr(&mut self, expr: &crate::parse::expr::Expr) -> Self::Output {
        expr.eq.accept(self)
    }
    fn visit_equality(&mut self, eq: &crate::parse::expr::Equality) -> Self::Output {
        let rhs: LoxType = eq.rhs.accept(self)?;
        match &eq.lhs {
            None => Ok(rhs),
            Some((op, lhs)) => match op.kind {
                TokenType::NEq => binary_ops::not_equal(&lhs.accept(self)?, &rhs),
                TokenType::EqEq => binary_ops::equal_equal(&lhs.accept(self)?, &rhs),
                _ => Err(RuntimeError::ImpossibleOP(op.kind.clone()).into()),
            },
        }
    }
    fn visit_comparison(&mut self, comp: &crate::parse::expr::Comparison) -> Self::Output {
        let rhs: LoxType = comp.rhs.accept(self)?;
        match &comp.lhs {
            None => Ok(rhs),
            Some((op, lhs)) => match op.kind {
                TokenType::Gt => binary_ops::gt(&lhs.accept(self)?, &rhs),
                TokenType::GtEq => binary_ops::gt_eq(&lhs.accept(self)?, &rhs),
                TokenType::Lt => binary_ops::lt(&lhs.accept(self)?, &rhs),
                TokenType::LtEq => binary_ops::lt_eq(&lhs.accept(self)?, &rhs),
                _ => Err(RuntimeError::ImpossibleOP(op.kind.clone()).into()),
            },
        }
    }
    fn visit_term(&mut self, term: &crate::parse::expr::Term) -> Self::Output {
        let rhs: LoxType = term.rhs.accept(self)?;
        match &term.lhs {
            None => Ok(rhs),
            Some((op, lhs)) => match op.kind {
                TokenType::Plus => binary_ops::add(&lhs.accept(self)?, &rhs),
                TokenType::Minus => binary_ops::sub(&lhs.accept(self)?, &rhs),
                _ => Err(RuntimeError::ImpossibleOP(op.kind.clone()).into()),
            },
        }
    }

    fn visit_factor(&mut self, factor: &crate::parse::expr::Factor) -> Self::Output {
        let rhs: LoxType = factor.rhs.accept(self)?;
        match &factor.lhs {
            None => Ok(rhs),
            Some((op, lhs)) => match op.kind {
                TokenType::Star => binary_ops::mul(&lhs.accept(self)?, &rhs),
                TokenType::Slash => binary_ops::div(&lhs.accept(self)?, &rhs),
                _ => Err(RuntimeError::ImpossibleOP(op.kind.clone()).into()),
            },
        }
    }
    fn visit_unary(&mut self, unary: &crate::parse::expr::Unary) -> Self::Output {
        match unary {
            Unary::Unary(op, unary) => match op.kind {
                TokenType::Bang => unary_ops::not(&unary.accept(self)?),
                TokenType::Minus => unary_ops::neg(&unary.accept(self)?),
                _ => Err(RuntimeError::ImpossibleOP(op.kind.clone()).into()),
            },
            Unary::Primary(primary) => primary.accept(self),
        }
    }
    fn visit_primary(&mut self, primary: &crate::parse::expr::Primary) -> Self::Output {
        match primary {
            Primary::Token(token) => match token.kind {
                TokenType::Literal(_) | TokenType::Keyword(_) => Ok(token.kind.clone().into()),
                _ => Err(RuntimeError::ImpossibleOP(token.kind.clone()).into()),
            },
            Primary::Grouping(expr) => expr.accept(self),
        }
    }
}

impl TreeWalker {
    pub fn new() -> Self {
        Self {}
    }
}

impl From<TokenType> for LoxType {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Literal(lit) => match lit {
                LiteralKind::Number(num, _) => Self::Number(num),
                LiteralKind::String(s) => Self::String(s),
            },
            TokenType::Keyword(key) => match key {
                Keyword::False => Self::Bool(false),
                Keyword::True => Self::Bool(true),
                _ => Self::Nil,
            },
            _ => Self::Nil,
        }
    }
}

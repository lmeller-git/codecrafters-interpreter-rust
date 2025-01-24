use super::{
    environment::Environment,
    ops::{binary_ops, unary_ops},
    Evaluator, RuntimeError,
};
use crate::{
    core::types::LoxType,
    lexer::lexing_utils::{Keyword, LiteralKind, TokenType},
    parse::{
        ast::{Visitable, Visitor},
        declaration::Declaration,
        expr::{Primary, Unary},
        stmt::Stmt,
    },
};
use anyhow::Result;

pub struct TreeWalker {
    env: Environment,
}

impl Evaluator for TreeWalker {
    fn resolve_err(&mut self) {}

    fn evaluate(&mut self, decl: &Declaration) -> anyhow::Result<crate::core::types::LoxType> {
        match decl.accept(self) {
            Ok(r) => Ok(r),
            //TODO
            Err(e) => Err(e), //Ok(LoxType::default()),
        }
    }
}

impl Visitor for TreeWalker {
    type Output = Result<LoxType>;

    fn visit_declaration(&mut self, decl: &crate::parse::declaration::Declaration) -> Self::Output {
        match decl {
            Declaration::Var(v) => v.accept(self),
            Declaration::Stmt(s) => s.accept(self),
        }
    }

    fn visit_vardecl(&mut self, var_decl: &crate::parse::declaration::VarDecl) -> Self::Output {
        let res = if let Some(expr) = &var_decl.value {
            expr.accept(self)?
        } else {
            LoxType::Nil
        };
        self.env.define(var_decl.ident.clone(), res);
        Ok(LoxType::default())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        match stmt {
            Stmt::Expr(e) => e.accept(self),
            Stmt::Print(p) => p.accept(self),
        }
    }

    fn visit_printstmt(&mut self, p_stmt: &crate::parse::stmt::PrintStmt) -> Self::Output {
        let res = p_stmt.args.accept(self)?;
        println!("{}", res);
        Ok(LoxType::default())
    }

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
                TokenType::Ident(ref ident) => self.env.get(ident).cloned(),
                TokenType::Literal(_) | TokenType::Keyword(_) => Ok(token.kind.clone().into()),
                _ => Err(RuntimeError::ImpossibleOP(token.kind.clone()).into()),
            },
            Primary::Grouping(expr) => expr.accept(self),
        }
    }
}

impl TreeWalker {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
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

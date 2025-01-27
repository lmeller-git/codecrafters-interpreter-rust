use std::{cell::RefCell, rc::Rc, str::FromStr};

use super::{
    environment::{Environment, FuncEnv},
    ops::{
        binary_ops,
        unary_ops::{self, is_true},
    },
    Evaluator, RuntimeError,
};
use crate::{
    core::types::{string::LoxString, LoxType},
    lexer::lexing_utils::{Keyword, LiteralKind, TokenType},
    lox_std::{exe_builtin, is_builtin, print},
    parse::{
        ast::{Visitable, Visitor},
        declaration::Declaration,
        expr::{Assignment, Primary, Unary},
        stmt::Stmt,
    },
};
use anyhow::Result;

pub struct TreeWalker {
    env: Rc<RefCell<Environment>>,
    func_env: FuncEnv,
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

    fn visit_block(&mut self, block: &crate::parse::stmt::Block) -> Self::Output {
        let prev_env_state = self.env.clone();
        self.env = Rc::new(RefCell::new(
            Environment::new().with_parent(prev_env_state.clone()),
        ));

        for decl in &block.decls {
            self.evaluate(decl)?;
        }

        self.env = prev_env_state;
        Ok(LoxType::default())
    }

    fn visit_declaration(&mut self, decl: &crate::parse::declaration::Declaration) -> Self::Output {
        match decl {
            Declaration::Var(v) => v.accept(self),
            Declaration::Stmt(s) => s.accept(self),
            Declaration::Fn(f) => f.accept(self),
        }
    }

    fn visit_fndecl(&mut self, fndecl: &crate::parse::declaration::FnDecl) -> Self::Output {
        self.func_env.define(fndecl.clone());
        Ok(LoxType::default())
    }

    fn visit_vardecl(&mut self, var_decl: &crate::parse::declaration::VarDecl) -> Self::Output {
        let res = if let Some(expr) = &var_decl.value {
            expr.accept(self)?
        } else {
            LoxType::Nil
        };
        self.env.borrow_mut().define(var_decl.ident.clone(), res);
        Ok(LoxType::default())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        match stmt {
            Stmt::Expr(e) => e.accept(self),
            Stmt::Print(p) => p.accept(self),
            Stmt::Block(b) => b.accept(self),
            Stmt::Cond(i) => i.accept(self),
            Stmt::While(w) => w.accept(self),
            Stmt::For(fo) => fo.accept(self),
        }
    }

    fn visit_for(&mut self, for_stmt: &crate::parse::stmt::ForStmt) -> Self::Output {
        for_stmt.init.accept(self)?;
        while is_true(&for_stmt.cond.accept(self)?)? {
            for_stmt.body.accept(self)?;
            for_stmt.incr.accept(self)?;
        }
        Ok(LoxType::default())
    }

    fn visit_while(&mut self, while_stmt: &crate::parse::stmt::WhileStmt) -> Self::Output {
        while is_true(&while_stmt.cond.accept(self)?)? {
            while_stmt.body.accept(self)?;
        }
        Ok(LoxType::default())
    }

    fn visit_if_stmt(&mut self, if_stmt: &crate::parse::stmt::IfStmt) -> Self::Output {
        if is_true(&if_stmt.cond.accept(self)?)? {
            if_stmt.then_branch.accept(self)
        } else if let Some(cond) = &if_stmt.else_branch {
            cond.accept(self)
        } else {
            Ok(LoxType::default())
        }
    }

    fn visit_printstmt(&mut self, p_stmt: &crate::parse::stmt::PrintStmt) -> Self::Output {
        let res = p_stmt.args.accept(self)?;
        print(&format!("{}\n", res));
        Ok(LoxType::default())
    }

    fn visit_expr(&mut self, expr: &crate::parse::expr::Expr) -> Self::Output {
        expr.assignment.accept(self)
    }

    fn visit_logic_or(&mut self, logic_or: &crate::parse::expr::LogicOr) -> Self::Output {
        if logic_or.rhs.is_empty() {
            return logic_or.lhs.accept(self);
        }
        let lhs = logic_or.lhs.accept(self)?;
        if is_true(&lhs)? {
            return Ok(lhs);
        }
        for v in &logic_or.rhs {
            let v = v.accept(self)?;
            if is_true(&v)? {
                return Ok(v);
            }
        }

        Ok(LoxType::Bool(false))
    }

    fn visit_logic_and(&mut self, logic_and: &crate::parse::expr::LogicAnd) -> Self::Output {
        if logic_and.rhs.is_empty() {
            return logic_and.lhs.accept(self);
        }
        if !is_true(&logic_and.lhs.accept(self)?)? {
            return Ok(LoxType::Bool(false));
        }
        for v in &logic_and.rhs {
            if !is_true(&v.accept(self)?)? {
                return Ok(LoxType::Bool(false));
            }
        }

        logic_and.rhs.last().unwrap().accept(self)
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
                TokenType::Ident(ref ident) => {
                    if is_builtin(ident) {
                        return Ok(LoxType::String(
                            LoxString::from_str(&format!("<fn {}>", ident)).unwrap(),
                        ));
                    }
                    if self.func_env.get(ident).is_ok() {
                        return Ok(LoxType::String(
                            LoxString::from_str(&format!("<fn {}>", ident)).unwrap(),
                        ));
                    }

                    self.env.borrow().get(ident)
                }
                TokenType::Literal(_) | TokenType::Keyword(_) => Ok(token.kind.clone().into()),
                _ => Err(RuntimeError::ImpossibleOP(token.kind.clone()).into()),
            },
            Primary::Call(funccall) => {
                if let Ok(res) = exe_builtin(&funccall.ident) {
                    return Ok(res);
                }
                let r = self.func_env.get(&funccall.ident)?.clone();
                // execute function
                // define new scope => call block.accept self in new scope => restore old scope
                let prev_env_state = self.env.clone();
                let new_env = Rc::new(RefCell::new(
                    Environment::new().with_parent(prev_env_state.clone()),
                ));
                for (ident, val) in r.args.iter().zip(funccall.args.iter()) {
                    new_env
                        .borrow_mut()
                        .define(ident.clone(), val.accept(self)?);
                }
                self.env = new_env;
                let res = r.body.accept(self);

                self.env = prev_env_state;
                res
            }
            Primary::Grouping(expr) => expr.accept(self),
        }
    }

    fn visit_assignment(&mut self, assignment: &crate::parse::expr::Assignment) -> Self::Output {
        match assignment {
            Assignment::Assignment(ident, to) => {
                let to = to.accept(self)?;
                self.env.borrow_mut().assign(ident, to.clone())?;
                Ok(to)
            }
            Assignment::Or(eq) => eq.accept(self),
        }
    }
}

impl TreeWalker {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
            func_env: FuncEnv::new(),
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

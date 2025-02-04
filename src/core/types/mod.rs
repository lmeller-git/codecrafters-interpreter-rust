pub mod bool;
pub mod nil;
pub mod num;
pub mod string;

use std::fmt::Display;

use num::Number;
use string::LoxString;
//somehow remove this
use crate::parse::declaration::FnDecl;

#[derive(Debug, Default, Clone)]
pub enum LoxType {
    Number(Number),
    String(LoxString),
    Bool(bool),
    Nil,
    #[default]
    Unit,
    Fn(String, FnDecl),
}

impl PartialEq for LoxType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Number(n1), Self::Number(n2)) => n1 == n2,
            (Self::Fn(f1, _), Self::Fn(f2, _)) => f1 == f2,
            (Self::String(s1), Self::String(s2)) => s1 == s2,
            (Self::Unit, Self::Unit) => true,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            _ => false,
        }
    }
}

impl Eq for LoxType {}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil")?,
            Self::Number(num) => write!(f, "{}", num)?,
            Self::String(s) => write!(f, "{}", s)?,
            Self::Bool(b) => write!(f, "{}", b)?,
            Self::Unit => write!(f, "()")?,
            Self::Fn(ident, _) => write!(f, "<fn {}>", ident)?,
        }
        Ok(())
    }
}

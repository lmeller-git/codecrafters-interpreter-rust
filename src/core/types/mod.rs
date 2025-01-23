pub mod bool;
pub mod nil;
pub mod num;
pub mod string;

use std::fmt::Display;

use num::Number;
use string::LoxString;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum LoxType {
    Number(Number),
    String(LoxString),
    Bool(bool),
    Nil,
    #[default]
    Unit,
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil")?,
            Self::Number(num) => write!(f, "{}", num)?,
            Self::String(s) => write!(f, "{}", s)?,
            Self::Bool(b) => write!(f, "{}", b)?,
            Self::Unit => write!(f, "()")?,
        }
        Ok(())
    }
}

use std::ops::Not;

use anyhow::Result;

use crate::{
    core::{ops::LoxNeg, types::LoxType},
    eval::RuntimeError,
};

pub fn neg(val: &LoxType) -> Result<LoxType> {
    let LoxType::Number(num) = val else {
        return Err(RuntimeError::IllegalOp("Neg".into()).into());
    };
    Ok(LoxType::Number(num.negate()))
}

pub fn not(val: &LoxType) -> Result<LoxType> {
    match val {
        LoxType::Number(_) | LoxType::String(_) => Ok(LoxType::Bool(false)),
        LoxType::Nil => Ok(LoxType::Bool(true)),
        LoxType::Bool(b) => Ok(LoxType::Bool(b.not())),
        LoxType::Unit => Ok(LoxType::Unit),
        _ => Ok(LoxType::Unit),
    }
}

pub fn is_true(val: &LoxType) -> Result<bool> {
    match val {
        LoxType::Number(_) | LoxType::String(_) => Ok(true),
        LoxType::Nil => Ok(false),
        LoxType::Bool(b) => Ok(*b),
        LoxType::Unit => {
            Err(RuntimeError::IllegalOp("unit value neither true nor false".into()).into())
        }
        _ => Ok(false),
    }
}

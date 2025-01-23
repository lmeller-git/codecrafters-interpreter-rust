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
    let LoxType::Bool(b) = val else {
        return Err(RuntimeError::IllegalOp("Not on non bool".into()).into());
    };
    Ok(LoxType::Bool(b.not()))
}

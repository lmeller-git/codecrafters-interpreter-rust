use anyhow::Result;

use crate::{
    core::{
        ops::{LoxAdd, LoxDiv, LoxEq, LoxGt, LoxLt, LoxMul, LoxSub},
        types::LoxType,
    },
    eval::RuntimeError,
};

use super::unary_ops::not;

pub fn equal_equal(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    match (lhs, rhs) {
        (LoxType::String(s1), LoxType::String(s2)) => Ok(LoxType::Bool(s1.equal(s2))),
        (LoxType::Number(n1), LoxType::Number(n2)) => Ok(LoxType::Bool(n1.equal(n2))),
        (LoxType::Bool(b1), LoxType::Bool(b2)) => Ok(LoxType::Bool(b1.equal(b2))),
        (LoxType::Nil, LoxType::Nil) => Ok(LoxType::Bool(true)),
        _ => Ok(LoxType::Bool(false)),
    }
}

pub fn not_equal(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    not(&equal_equal(lhs, rhs)?)
}
pub fn add(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    match (lhs, rhs) {
        (LoxType::String(s1), LoxType::String(s2)) => Ok(LoxType::String(s1.add(s2))),
        (LoxType::Number(n1), LoxType::Number(n2)) => Ok(LoxType::Number(n1.add(n2))),
        _ => Err(RuntimeError::IllegalOp("add on false types".into()).into()),
    }
}
pub fn sub(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    let (LoxType::Number(num1), LoxType::Number(num2)) = (lhs, rhs) else {
        return Err(RuntimeError::IllegalOp("sub on non num".into()).into());
    };
    Ok(LoxType::Number(num1.subtract(num2)))
}
pub fn mul(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    let (LoxType::Number(num1), LoxType::Number(num2)) = (lhs, rhs) else {
        return Err(RuntimeError::IllegalOp("mul on non num".into()).into());
    };
    Ok(LoxType::Number(num1.multiply(num2)))
}
pub fn div(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    let (LoxType::Number(num1), LoxType::Number(num2)) = (lhs, rhs) else {
        return Err(RuntimeError::IllegalOp("div on non num".into()).into());
    };
    Ok(LoxType::Number(num1.divide(num2)))
}
pub fn gt_eq(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    let gt = gt(lhs, rhs)?;
    let eq = equal_equal(lhs, rhs)?;
    let (LoxType::Bool(b1), LoxType::Bool(b2)) = (gt, eq) else {
        return Err(RuntimeError::IllegalOp("impossible".into()).into());
    };
    Ok(LoxType::Bool(b1 || b2))
}
pub fn lt_eq(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    let lt = lt(lhs, rhs)?;
    let eq = equal_equal(lhs, rhs)?;
    let (LoxType::Bool(b1), LoxType::Bool(b2)) = (lt, eq) else {
        return Err(RuntimeError::IllegalOp("impossible".into()).into());
    };
    Ok(LoxType::Bool(b1 || b2))
}
pub fn gt(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    match (lhs, rhs) {
        (LoxType::Number(n1), LoxType::Number(n2)) => Ok(LoxType::Bool(n1.greater_than(n2))),
        _ => Err(RuntimeError::IllegalOp("gt on non num".into()).into()),
    }
}
pub fn lt(lhs: &LoxType, rhs: &LoxType) -> Result<LoxType> {
    match (lhs, rhs) {
        (LoxType::Number(n1), LoxType::Number(n2)) => Ok(LoxType::Bool(n1.less_than(n2))),
        _ => Err(RuntimeError::IllegalOp("lt on non num".into()).into()),
    }
}

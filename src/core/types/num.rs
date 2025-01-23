#![allow(dead_code, private_bounds)]

use std::{fmt::Display, str::FromStr};

use anyhow::Context;

use crate::core::ops::{
    LoxAdd, LoxDiv, LoxEq, LoxGt, LoxGtEq, LoxLt, LoxLtEq, LoxMul, LoxNEq, LoxNeg, LoxSub,
};

#[derive(Debug, Clone)]
enum NumericValue {
    Float(f64),
    Int(i64),
}

impl PartialEq for NumericValue {
    fn eq(&self, other: &Self) -> bool {
        //TODO implement float, int cmp
        match (self, other) {
            (Self::Int(i), Self::Int(i2)) => i == i2,
            (Self::Float(i), Self::Float(i2)) => i == i2 && !i.is_nan() && !i2.is_nan(),
            _ => false,
        }
    }
}

impl Eq for NumericValue {}

impl NumericValue {
    fn inner<T: FromNumericValue>(&self) -> T {
        T::from_numeric_value(self).unwrap()
    }
}

impl Default for NumericValue {
    fn default() -> Self {
        Self::Int(0)
    }
}

trait FromNumericValue {
    fn from_numeric_value(value: &NumericValue) -> Option<Self>
    where
        Self: Sized;
}

impl FromNumericValue for f64 {
    fn from_numeric_value(value: &NumericValue) -> Option<Self> {
        if let NumericValue::Float(num) = value {
            Some(*num)
        } else {
            None
        }
    }
}

impl FromNumericValue for i64 {
    fn from_numeric_value(value: &NumericValue) -> Option<Self> {
        if let NumericValue::Int(num) = value {
            Some(*num)
        } else {
            None
        }
    }
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct Number {
    value: NumericValue,
}

impl FromStr for Number {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let num: NumericValue = if s.contains('.') {
            NumericValue::Float(
                f64::from_str(s).with_context(|| format!("could not parse {} into float", s))?,
            )
        } else {
            NumericValue::Int(
                i64::from_str(s).with_context(|| format!("could not parse {} into int", s))?,
            )
        };
        Ok(Self { value: num })
    }
}
impl From<f64> for NumericValue {
    fn from(value: f64) -> Self {
        NumericValue::Float(value)
    }
}

impl From<i64> for NumericValue {
    fn from(value: i64) -> Self {
        NumericValue::Int(value)
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            NumericValue::Float(num) => write!(f, "{}", num)?,
            NumericValue::Int(num) => write!(f, "{}", num)?,
        }
        Ok(())
    }
}

impl Number {
    pub fn to_float(&self) -> Self {
        Self {
            value: NumericValue::Float(self.value.inner::<i64>() as f64),
        }
    }

    pub fn to_int(&self) -> Self {
        Self {
            value: NumericValue::Int(self.value.inner::<f64>() as i64),
        }
    }

    pub fn as_floating_point_str(&self) -> String {
        match self.value {
            NumericValue::Float(num) => {
                if (-f64::EPSILON..f64::EPSILON).contains(&(num - ((num as i64) as f64))) {
                    format!("{:.1}", num)
                } else {
                    format!("{}", num)
                }
            }
            NumericValue::Int(num) => format!("{:.1}", num as f64),
        }
    }

    pub fn new<T>(value: T) -> Self
    where
        T: Into<NumericValue>,
    {
        Number {
            value: value.into(),
        }
    }
}

impl LoxAdd for NumericValue {
    type Output = Self;
    fn add(&self, rhs: &Self) -> Self::Output {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => NumericValue::Float(f + f2),
            (NumericValue::Int(f), NumericValue::Int(f2)) => NumericValue::Int(f + f2),
            (NumericValue::Float(f), NumericValue::Int(i)) => NumericValue::Float(f + *i as f64),
            (NumericValue::Int(i), NumericValue::Float(f)) => NumericValue::Float(f + *i as f64),
        }
    }
}

impl LoxAdd for Number {
    type Output = Self;
    fn add(&self, rhs: &Self) -> Self::Output {
        Self {
            value: self.value.add(&rhs.value),
        }
    }
}

impl LoxSub for NumericValue {
    type Output = Self;

    fn subtract(&self, rhs: &Self) -> Self::Output {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => NumericValue::Float(f - f2),
            (NumericValue::Int(i), NumericValue::Int(i2)) => NumericValue::Int(i - i2),
            (NumericValue::Float(f), NumericValue::Int(i)) => NumericValue::Float(f - *i as f64),
            (NumericValue::Int(i), NumericValue::Float(f)) => NumericValue::Float(*i as f64 - f),
        }
    }
}

impl LoxSub for Number {
    type Output = Self;

    fn subtract(&self, rhs: &Self) -> Self::Output {
        Self {
            value: self.value.subtract(&rhs.value),
        }
    }
}

impl LoxMul for NumericValue {
    type Output = Self;

    fn multiply(&self, rhs: &Self) -> Self::Output {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => NumericValue::Float(f * f2),
            (NumericValue::Int(i), NumericValue::Int(i2)) => NumericValue::Int(i * i2),
            (NumericValue::Float(f), NumericValue::Int(i)) => NumericValue::Float(f * *i as f64),
            (NumericValue::Int(i), NumericValue::Float(f)) => NumericValue::Float(*i as f64 * f),
        }
    }
}

impl LoxMul for Number {
    type Output = Self;

    fn multiply(&self, rhs: &Self) -> Self::Output {
        Self {
            value: self.value.multiply(&rhs.value),
        }
    }
}

//TODO handle 0 div
impl LoxDiv for NumericValue {
    type Output = Self;

    fn divide(&self, rhs: &Self) -> Self::Output {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => NumericValue::Float(f / f2),
            (NumericValue::Int(i), NumericValue::Int(i2)) => NumericValue::Int(i / i2),
            (NumericValue::Float(f), NumericValue::Int(i)) => NumericValue::Float(f / *i as f64),
            (NumericValue::Int(i), NumericValue::Float(f)) => NumericValue::Float(*i as f64 / f),
        }
    }
}

impl LoxDiv for Number {
    type Output = Self;

    fn divide(&self, rhs: &Self) -> Self::Output {
        Self {
            value: self.value.divide(&rhs.value),
        }
    }
}

impl LoxNeg for Number {
    fn negate(&self) -> Self {
        Self {
            value: match self.value {
                NumericValue::Float(f) => NumericValue::Float(-f),
                NumericValue::Int(i) => NumericValue::Int(-i),
            },
        }
    }
}

impl LoxEq for NumericValue {
    fn equal(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => {
                (-f64::EPSILON..=f64::EPSILON).contains(&(f - f2))
            }
            (NumericValue::Int(i), NumericValue::Int(i2)) => i == i2,
            (NumericValue::Float(f), NumericValue::Int(i)) => {
                (-f64::EPSILON..=f64::EPSILON).contains(&(*f - *i as f64))
            }
            (NumericValue::Int(i), NumericValue::Float(f)) => {
                (-f64::EPSILON..=f64::EPSILON).contains(&(*f - *i as f64))
            }
        }
    }
}

impl LoxEq for Number {
    fn equal(&self, rhs: &Self) -> bool {
        self.value.equal(&rhs.value)
    }
}

impl LoxNEq for Number {
    fn not_equal(&self, rhs: &Self) -> bool {
        !self.equal(rhs)
    }
}

impl LoxGt for NumericValue {
    fn greater_than(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => f > f2,
            (NumericValue::Int(i), NumericValue::Int(i2)) => i > i2,
            (NumericValue::Float(f), NumericValue::Int(i)) => *f > *i as f64,
            (NumericValue::Int(i), NumericValue::Float(f)) => *f < *i as f64,
        }
    }
}

impl LoxGt for Number {
    fn greater_than(&self, rhs: &Self) -> bool {
        self.value.greater_than(&rhs.value)
    }
}

impl LoxGtEq for Number {
    fn greater_or_equal(&self, rhs: &Self) -> bool {
        self.greater_than(rhs) || self.equal(rhs)
    }
}

impl LoxLt for NumericValue {
    fn less_than(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (NumericValue::Float(f), NumericValue::Float(f2)) => f < f2,
            (NumericValue::Int(i), NumericValue::Int(i2)) => i < i2,
            (NumericValue::Float(f), NumericValue::Int(i)) => *f < *i as f64,
            (NumericValue::Int(i), NumericValue::Float(f)) => *f > *i as f64,
        }
    }
}

impl LoxLt for Number {
    fn less_than(&self, rhs: &Self) -> bool {
        self.value.less_than(&rhs.value)
    }
}

impl LoxLtEq for Number {
    fn less_or_equal(&self, rhs: &Self) -> bool {
        self.less_than(rhs) || self.equal(rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numeric_value_sub() {
        let a = NumericValue::Int(10);
        let b = NumericValue::Int(5);
        assert_eq!(a.subtract(&b), NumericValue::Int(5));

        let c = NumericValue::Float(3.5);
        let d = NumericValue::Float(2.5);
        assert_eq!(c.subtract(&d), NumericValue::Float(1.0));
    }

    #[test]
    fn test_numeric_value_mul() {
        let a = NumericValue::Int(10);
        let b = NumericValue::Int(5);
        assert_eq!(a.multiply(&b), NumericValue::Int(50));

        let c = NumericValue::Float(3.5);
        let d = NumericValue::Float(2.0);
        assert_eq!(c.multiply(&d), NumericValue::Float(7.0));
    }

    #[test]
    fn test_numeric_value_div() {
        let a = NumericValue::Int(10);
        let b = NumericValue::Int(5);
        assert_eq!(a.divide(&b), NumericValue::Int(2));

        let c = NumericValue::Float(10.0);
        let d = NumericValue::Float(2.0);
        assert_eq!(c.divide(&d), NumericValue::Float(5.0));
    }
}

#![allow(dead_code, private_bounds)]
use std::{
    convert::Infallible,
    fmt::{Debug, Display},
    str::FromStr,
};

use anyhow::Context;

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoxString {
    value: String,
}

impl Display for LoxString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}

impl FromStr for LoxString {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            value: s.to_string(),
        })
    }
}

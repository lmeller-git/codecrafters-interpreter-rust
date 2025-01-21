#![allow(dead_code)]
use std::{
    convert::Infallible,
    fmt::{Debug, Display},
    str::FromStr,
};

use anyhow::Context;

#[derive(Debug)]
enum NumericValue {
    Float(f64),
    Int(i64),
}

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

#[derive(Debug, Default)]
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
}

#[derive(Debug)]
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

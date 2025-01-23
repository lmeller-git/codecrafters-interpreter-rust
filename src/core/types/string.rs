use std::{convert::Infallible, fmt::Display, str::FromStr};

use crate::core::ops::{LoxAdd, LoxEq, LoxNEq};

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

impl LoxEq for LoxString {
    fn equal(&self, rhs: &Self) -> bool {
        self.value == rhs.value
    }
}

impl LoxNEq for LoxString {
    fn not_equal(&self, rhs: &Self) -> bool {
        !self.equal(rhs)
    }
}

impl LoxAdd for LoxString {
    type Output = Self;
    fn add(&self, rhs: &Self) -> Self::Output {
        let mut value = self.value.clone();
        value.push_str(&rhs.value);
        Self { value }
    }
}

use std::collections::HashMap;

use anyhow::Result;

use crate::core::types::LoxType;

use super::RuntimeError;

pub struct Environment {
    values: HashMap<String, LoxType>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, ident: String, value: LoxType) {
        _ = self.values.insert(ident, value)
    }

    pub fn get(&self, ident: &str) -> Result<&LoxType> {
        match self.values.get(ident) {
            None => Err(RuntimeError::UnknownVar(ident.into()).into()),
            Some(val) => Ok(val),
        }
    }
}

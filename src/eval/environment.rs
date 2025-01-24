use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::Result;

use crate::core::types::LoxType;

use super::RuntimeError;

pub struct Environment {
    values: HashMap<String, LoxType>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(mut self, parent: Rc<RefCell<Environment>>) -> Self {
        self.parent = Some(parent.clone());
        self
    }

    pub fn define(&mut self, ident: String, value: LoxType) {
        _ = self.values.insert(ident, value)
    }

    pub fn assign(&mut self, ident: &str, value: LoxType) -> Result<()> {
        if self.values.contains_key(ident) {
            *self.values.get_mut(ident).unwrap() = value;
            Ok(())
        } else {
            match &self.parent {
                Some(p) => p.borrow_mut().assign(ident, value),
                None => Err(RuntimeError::UnknownVar(ident.into()).into()),
            }
        }
    }

    pub fn get(&self, ident: &str) -> Result<LoxType> {
        match self.values.get(ident) {
            None => match &self.parent {
                Some(p) => p.borrow().get(ident),
                None => Err(RuntimeError::UnknownVar(ident.into()).into()),
            },
            Some(val) => Ok(val.clone()),
        }
    }
}

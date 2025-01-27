use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::Result;
use thiserror::Error;

use crate::core::types::{num::Number, LoxType};

pub fn is_builtin(t: &str) -> bool {
    false
}

pub fn exe_builtin(t: &str) -> Result<LoxType> {
    match t {
        "clock" => Ok(clock()),
        _ => Err(StdErr::NotABuiltin(t.to_string()).into()),
    }
}

pub fn clock() -> LoxType {
    LoxType::Number(Number::new(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as i64,
    ))
}

pub fn print(args: &str) {
    print!("{}", args)
}

#[derive(Error, Debug)]
pub enum StdErr {
    #[error("{0} is not a builtin")]
    NotABuiltin(String),
}

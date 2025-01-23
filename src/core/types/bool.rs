use crate::core::ops::{LoxEq, LoxNEq, LoxNot};

impl LoxNEq for bool {
    fn not_equal(&self, rhs: &Self) -> bool {
        self != rhs
    }
}

impl LoxEq for bool {
    fn equal(&self, rhs: &Self) -> bool {
        self == rhs
    }
}

impl LoxNot for bool {
    fn logical_not(&self) -> bool {
        !self
    }
}

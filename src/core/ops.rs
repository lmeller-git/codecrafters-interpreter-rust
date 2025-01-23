use super::types::LoxType;

pub trait LoxNeg {
    fn negate(&self) -> Self;
}

pub trait LoxNot {
    fn logical_not(&self) -> bool;
}

pub trait LoxAdd<Rhs = Self> {
    type Output;
    fn add(&self, rhs: &Rhs) -> Self::Output;
}

pub trait LoxSub<Rhs = Self> {
    type Output;
    fn subtract(&self, rhs: &Rhs) -> Self::Output;
}

pub trait LoxMul<Rhs = Self> {
    type Output;
    fn multiply(&self, rhs: &Rhs) -> Self::Output;
}
pub trait LoxDiv<Rhs = Self> {
    type Output;
    fn divide(&self, rhs: &Rhs) -> Self::Output;
}

pub trait LoxEq<Rhs = Self> {
    fn equal(&self, rhs: &Rhs) -> bool;
}

pub trait LoxNEq<Rhs = Self> {
    fn not_equal(&self, rhs: &Rhs) -> bool;
}

pub trait LoxGt<Rhs = Self> {
    fn greater_than(&self, rhs: &Rhs) -> bool;
}

pub trait LoxLt<Rhs = Self> {
    fn less_than(&self, rhs: &Rhs) -> bool;
}

pub trait LoxGtEq<Rhs = Self> {
    fn greater_or_equal(&self, rhs: &Rhs) -> bool;
}

pub trait LoxLtEq<Rhs = Self> {
    fn less_or_equal(&self, rhs: &Rhs) -> bool;
}

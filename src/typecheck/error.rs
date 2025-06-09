use thiserror::Error;

use crate::typecheck::{Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum TypeError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),

    #[error("Type mismatch: {0} and {1}")]
    TypeMismatch(Box<Type>, Box<Type>),

    #[error("Occurs check failed: type variable {0} occurs in type {1}")]
    OccursCheckFailed(TypeVar, Type),
}

impl TypeError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        Self::UnboundVariable(name.into())
    }

    pub fn type_mismatch(ty1: Type, ty2: Type) -> Self {
        Self::TypeMismatch(Box::new(ty1), Box::new(ty2))
    }

    pub fn occurs_check_failed(var: TypeVar, ty: Type) -> Self {
        Self::OccursCheckFailed(var, ty)
    }
}

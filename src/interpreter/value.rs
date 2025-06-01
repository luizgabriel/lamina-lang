use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt::Display;
use std::rc::Rc;

use crate::lexer::Spanned;
use crate::parser::{AstExpr, AstLiteral};

use super::{Environment, InterpreterError};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Num(f64),
    Bool(bool),
    Tuple(im::Vector<Value>),
    Closure {
        arg_name: Spanned<String>,
        body: Spanned<AstExpr>,
        env: Rc<RefCell<Environment>>,
    },
    BuiltInFn(String),
}

impl Value {
    pub fn is_builtin(&self) -> bool {
        matches!(self, Value::BuiltInFn(_))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Value::Unit)
    }

    pub fn tuple(items: impl IntoIterator<Item = Value>) -> Self {
        Value::Tuple(items.into_iter().collect())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Num(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Closure { arg_name, body, .. } => write!(f, "({} -> {})", arg_name.0, body.0),
            Value::BuiltInFn(name) => write!(f, "builtin ({})", name),
        }
    }
}

impl From<AstLiteral> for Value {
    fn from(literal: AstLiteral) -> Self {
        match literal {
            AstLiteral::Unit => Value::Unit,
            AstLiteral::Num(n) => Value::Num(n),
            AstLiteral::Bool(b) => Value::Bool(b),
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Num(n)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl TryFrom<Value> for f64 {
    type Error = InterpreterError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Num(n) => Ok(n),
            _ => Err(InterpreterError::type_error("Expected number")),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = InterpreterError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(InterpreterError::type_error("Expected boolean")),
        }
    }
}

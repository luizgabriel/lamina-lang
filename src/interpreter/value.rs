use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt::Display;
use std::rc::Rc;

use crate::{
    lexer::Spanned,
    parser::{AstExpr, Literal},
};

use super::{Environment, InterpreterError};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Literal(Literal),
    Tuple(im::Vector<Value>),
    Closure {
        param: Spanned<String>,
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
        matches!(self, Value::Literal(Literal::Unit))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Value::Literal(_))
    }

    pub fn tuple(items: impl IntoIterator<Item = Value>) -> Self {
        Value::Tuple(items.into_iter().collect())
    }

    pub fn builtin(name: impl Into<String>) -> Self {
        Value::BuiltInFn(name.into())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Literal(lit) => write!(f, "{}", lit),
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
            Value::Closure { param, body, .. } => {
                write!(f, "({} -> {})", param.0, body.0)
            }
            Value::BuiltInFn(name) => write!(f, "builtin ({})", name),
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Unit => Value::Literal(Literal::Unit),
            Literal::Num(n) => Value::Literal(n.into()),
            Literal::Bool(b) => Value::Literal(b.into()),
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Literal(n.into())
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Literal(b.into())
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Literal(Literal::Unit)
    }
}

impl TryFrom<Value> for f64 {
    type Error = InterpreterError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Literal(Literal::Num(n)) => Ok(n),
            _ => Err(InterpreterError::type_error("Expected number")),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = InterpreterError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Literal(Literal::Bool(b)) => Ok(b),
            _ => Err(InterpreterError::type_error("Expected boolean")),
        }
    }
}

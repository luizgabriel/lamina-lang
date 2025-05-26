use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt::Display;
use std::rc::Rc;

use crate::syntax::AstLiteral;
use crate::vm::VMError;

use super::{Instruction, VmEnv};

#[derive(Clone, Debug, PartialEq)]
pub enum VmValue {
    Unit,
    Num(f64),
    Bool(bool),
    Tuple(Vec<VmValue>),
    Closure {
        arg_name: String,
        body: Vec<Instruction>,
        env: Rc<RefCell<VmEnv>>,
    },
    BuiltInFn(String),
}

impl VmValue {
    pub fn is_builtin(&self) -> bool {
        matches!(self, VmValue::BuiltInFn(_))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, VmValue::Unit)
    }
}

impl Display for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmValue::Unit => write!(f, "()"),
            VmValue::Num(n) => write!(f, "{}", n),
            VmValue::Bool(b) => write!(f, "{}", b),
            VmValue::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            VmValue::Closure { arg_name, .. } => write!(f, "<closure λ{}>", arg_name),
            VmValue::BuiltInFn(name) => write!(f, "<native {}>", name),
        }
    }
}

impl From<AstLiteral> for VmValue {
    fn from(literal: AstLiteral) -> Self {
        match literal {
            AstLiteral::Unit => VmValue::Unit,
            AstLiteral::Num(n) => VmValue::Num(n),
            AstLiteral::Bool(b) => VmValue::Bool(b),
        }
    }
}

impl From<f64> for VmValue {
    fn from(n: f64) -> Self {
        VmValue::Num(n)
    }
}

impl From<bool> for VmValue {
    fn from(b: bool) -> Self {
        VmValue::Bool(b)
    }
}

impl TryFrom<VmValue> for f64 {
    type Error = VMError;
    fn try_from(value: VmValue) -> Result<Self, Self::Error> {
        match value {
            VmValue::Num(n) => Ok(n),
            _ => Err(VMError::type_error("Expected number")),
        }
    }
}

impl TryFrom<VmValue> for bool {
    type Error = VMError;
    fn try_from(value: VmValue) -> Result<Self, Self::Error> {
        match value {
            VmValue::Bool(b) => Ok(b),
            _ => Err(VMError::type_error("Expected boolean")),
        }
    }
}

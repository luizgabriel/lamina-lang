use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::syntax::AstLiteral;

use super::{Instruction, VmEnv};

#[derive(Clone, Debug, PartialEq)]
pub enum VmValue {
    Unit,
    Num(f64),
    Bool(bool),
    Tuple(Vec<VmValue>),
    Closure {
        fn_name: Option<String>,
        arg_name: String,
        body: Vec<Instruction>,
        env: Rc<RefCell<super::VmEnv>>,
    },
    NativeFn(String),
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
            VmValue::Closure {
                fn_name, arg_name, ..
            } => match fn_name {
                Some(name) => write!(f, "<recursive closure {} λ{}>", name, arg_name),
                None => write!(f, "<closure λ{}>", arg_name),
            },
            VmValue::NativeFn(name) => write!(f, "<native {}>", name),
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

use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid operation: {0} on {1}")]
    InvalidOperation(String, String),
}

impl InterpreterError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        InterpreterError::UnboundVariable(name.into())
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        InterpreterError::TypeError(message.into())
    }

    pub fn invalid_operation(op: impl Into<String>, operand: impl Into<String>) -> Self {
        InterpreterError::InvalidOperation(op.into(), operand.into())
    }
}

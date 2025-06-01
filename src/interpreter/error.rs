use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Stack overflow")]
    StackOverflow,

    #[error("Invalid operation: {0} on {1}")]
    InvalidOperation(String, String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),
}

impl InterpreterError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        InterpreterError::UnboundVariable(name.into())
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        InterpreterError::TypeError(message.into())
    }

    pub fn stack_overflow() -> Self {
        InterpreterError::StackOverflow
    }

    pub fn invalid_operation(op: impl Into<String>, operand: impl Into<String>) -> Self {
        InterpreterError::InvalidOperation(op.into(), operand.into())
    }

    pub fn runtime_error(message: impl Into<String>) -> Self {
        InterpreterError::RuntimeError(message.into())
    }
}

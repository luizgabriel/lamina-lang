use thiserror::Error;

#[derive(Debug, Error)]
pub enum VMError {
    #[error("Stack underflow: attempted to pop from empty stack")]
    StackUnderflow,

    #[error("Unbound variable: '{name}'")]
    UnboundVariable { name: String },

    #[error("Type error: {message}")]
    TypeError { message: String },

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid operation: {operation} cannot be applied to {operand_type}")]
    InvalidOperation {
        operation: String,
        operand_type: String,
    },
}

impl VMError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        VMError::UnboundVariable { name: name.into() }
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        VMError::TypeError {
            message: message.into(),
        }
    }

    pub fn invalid_operation(
        operation: impl Into<String>,
        operand_type: impl Into<String>,
    ) -> Self {
        VMError::InvalidOperation {
            operation: operation.into(),
            operand_type: operand_type.into(),
        }
    }
}

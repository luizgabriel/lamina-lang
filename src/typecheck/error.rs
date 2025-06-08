use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum TypeError {
    #[error("Unbound variable: {0}")]
    UnboundVariable(String),
}

impl TypeError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        Self::UnboundVariable(name.into())
    }
}

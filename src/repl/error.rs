use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseCommandError {
    #[error("Usage: {0} <expression>")]
    InvalidExpressionCommand(&'static str),

    #[error("Unknown command: {0}. Type :help for available commands.")]
    UnknownCommand(String),
}

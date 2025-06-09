use rustyline::DefaultEditor;

use crate::{
    interpreter::Environment,
    typecheck::{TypeEnvironment, TypeVarContext},
};

pub struct ReplState {
    pub rl: DefaultEditor,
    pub env: Environment,
    pub type_ctx: TypeVarContext,
    pub type_env: TypeEnvironment,
}

impl ReplState {
    pub fn new() -> anyhow::Result<Self> {
        let mut type_ctx = TypeVarContext::default();
        let type_env = TypeEnvironment::builtins(&mut type_ctx);
        Ok(ReplState {
            rl: DefaultEditor::new()?,
            env: Environment::builtins(),
            type_ctx,
            type_env,
        })
    }
}

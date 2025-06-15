use rustyline::DefaultEditor;

use crate::{
    interpreter::ValueEnv,
    typecheck::{TypeEnv, TypeVarContext},
};

pub struct ReplState {
    pub rl: DefaultEditor,
    pub env: ValueEnv,
    pub type_ctx: TypeVarContext,
    pub type_env: TypeEnv,
}

impl ReplState {
    pub fn new() -> anyhow::Result<Self> {
        let mut type_ctx = TypeVarContext::default();
        let type_env = TypeEnv::builtins(&mut type_ctx);
        Ok(ReplState {
            rl: DefaultEditor::new()?,
            env: ValueEnv::builtins(),
            type_ctx,
            type_env,
        })
    }
}

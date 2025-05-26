use super::VmValue;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct VmEnv {
    pub bindings: im::HashMap<String, VmValue>,
}

impl VmEnv {
    pub fn builtins() -> Self {
        let mut env = VmEnv::default();

        // Add built-in functions as native functions
        env.bindings
            .insert("+".to_string(), VmValue::NativeFn("+".to_string()));
        env.bindings
            .insert("-".to_string(), VmValue::NativeFn("-".to_string()));
        env.bindings
            .insert("*".to_string(), VmValue::NativeFn("*".to_string()));
        env.bindings
            .insert("/".to_string(), VmValue::NativeFn("/".to_string()));
        env.bindings
            .insert("==".to_string(), VmValue::NativeFn("==".to_string()));
        env.bindings
            .insert("<".to_string(), VmValue::NativeFn("<".to_string()));
        env.bindings
            .insert(">".to_string(), VmValue::NativeFn(">".to_string()));
        env.bindings
            .insert("&&".to_string(), VmValue::NativeFn("&&".to_string()));
        env.bindings
            .insert("||".to_string(), VmValue::NativeFn("||".to_string()));
        env.bindings
            .insert("!".to_string(), VmValue::NativeFn("!".to_string()));

        env
    }

    pub fn get(&self, name: &str) -> Option<&VmValue> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: String, value: VmValue) {
        self.bindings.insert(name, value);
    }

    pub fn extend(&self, name: String, value: VmValue) -> Self {
        let mut new_env = self.clone();
        new_env.set(name, value);
        new_env
    }
}

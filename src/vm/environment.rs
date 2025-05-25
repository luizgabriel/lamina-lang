use std::collections::HashMap;

use super::VmValue;

#[derive(Clone, Debug, PartialEq)]
pub struct VmEnv {
    pub bindings: HashMap<String, VmValue>,
}

impl Default for VmEnv {
    fn default() -> Self {
        let mut env = VmEnv {
            bindings: HashMap::new(),
        };

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
}

impl VmEnv {
    pub fn new() -> Self {
        VmEnv::default()
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

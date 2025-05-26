use super::VmValue;

#[derive(Clone, Debug, PartialEq)]
pub struct VmEnv {
    bindings: im::HashMap<String, VmValue>,
}

impl VmEnv {
    pub fn empty() -> Self {
        Self {
            bindings: im::HashMap::new(),
        }
    }

    pub fn builtins() -> Self {
        let mut env = VmEnv::empty();

        // Add built-in functions as native functions
        env.bindings
            .insert("+".to_string(), VmValue::BuiltInFn("+".to_string()));
        env.bindings
            .insert("-".to_string(), VmValue::BuiltInFn("-".to_string()));
        env.bindings
            .insert("*".to_string(), VmValue::BuiltInFn("*".to_string()));
        env.bindings
            .insert("/".to_string(), VmValue::BuiltInFn("/".to_string()));
        env.bindings
            .insert("==".to_string(), VmValue::BuiltInFn("==".to_string()));
        env.bindings
            .insert("<".to_string(), VmValue::BuiltInFn("<".to_string()));
        env.bindings
            .insert(">".to_string(), VmValue::BuiltInFn(">".to_string()));
        env.bindings
            .insert("&&".to_string(), VmValue::BuiltInFn("&&".to_string()));
        env.bindings
            .insert("||".to_string(), VmValue::BuiltInFn("||".to_string()));
        env.bindings
            .insert("!".to_string(), VmValue::BuiltInFn("!".to_string()));

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

impl<'a> IntoIterator for &'a VmEnv {
    type Item = (&'a String, &'a VmValue);
    type IntoIter = im::hashmap::Iter<'a, String, VmValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.bindings.iter()
    }
}

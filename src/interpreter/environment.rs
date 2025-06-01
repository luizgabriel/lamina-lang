use super::Value;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    bindings: im::HashMap<String, Value>,
}

impl Environment {
    pub fn empty() -> Self {
        Self {
            bindings: im::HashMap::new(),
        }
    }

    pub fn builtins() -> Self {
        let mut env = Environment::empty();

        // Add built-in functions as native functions
        env.bindings
            .insert("+".to_string(), Value::BuiltInFn("+".to_string()));
        env.bindings
            .insert("-".to_string(), Value::BuiltInFn("-".to_string()));
        env.bindings
            .insert("*".to_string(), Value::BuiltInFn("*".to_string()));
        env.bindings
            .insert("/".to_string(), Value::BuiltInFn("/".to_string()));
        env.bindings
            .insert("==".to_string(), Value::BuiltInFn("==".to_string()));
        env.bindings
            .insert("<".to_string(), Value::BuiltInFn("<".to_string()));
        env.bindings
            .insert(">".to_string(), Value::BuiltInFn(">".to_string()));
        env.bindings
            .insert("&&".to_string(), Value::BuiltInFn("&&".to_string()));
        env.bindings
            .insert("||".to_string(), Value::BuiltInFn("||".to_string()));
        env.bindings
            .insert("!".to_string(), Value::BuiltInFn("!".to_string()));

        env
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn extend(&self, name: String, value: Value) -> Self {
        let mut new_env = self.clone();
        new_env.set(name, value);
        new_env
    }
}

impl<'a> IntoIterator for &'a Environment {
    type Item = (&'a String, &'a Value);
    type IntoIter = im::hashmap::Iter<'a, String, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.bindings.iter()
    }
}

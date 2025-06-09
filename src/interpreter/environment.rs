use super::Value;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment(im::HashMap<String, Value>);

impl Environment {
    pub fn empty() -> Self {
        Self(im::HashMap::new())
    }

    pub fn builtins() -> Self {
        let mut env = Environment::empty();

        // Add built-in functions as native functions
        env.set("+", Value::BuiltInFn("+".to_string()));
        env.set("-", Value::BuiltInFn("-".to_string()));
        env.set("*", Value::BuiltInFn("*".to_string()));
        env.set("/", Value::BuiltInFn("/".to_string()));
        env.set("==", Value::BuiltInFn("==".to_string()));
        env.set("<", Value::BuiltInFn("<".to_string()));
        env.set(">", Value::BuiltInFn(">".to_string()));
        env.set("&&", Value::BuiltInFn("&&".to_string()));
        env.set("||", Value::BuiltInFn("||".to_string()));
        env.set("!", Value::BuiltInFn("!".to_string()));

        env
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.0.get(name)
    }

    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        self.0.insert(name.into(), value);
    }

    pub fn extend(&self, name: impl Into<String>, value: Value) -> Self {
        let mut new_env = self.clone();
        new_env.set(name.into(), value);
        new_env
    }

    pub fn iter(&self) -> im::hashmap::Iter<String, Value> {
        self.0.iter()
    }
}

impl IntoIterator for Environment {
    type Item = (String, Value);
    type IntoIter = im::hashmap::ConsumingIter<(String, Value)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

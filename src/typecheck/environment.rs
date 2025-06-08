use crate::{
    lexer::Spanned,
    parser::{parse_type, AstType},
};
use im::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct TyEnvironment {
    bindings: HashMap<String, Spanned<AstType>>,
}

impl TyEnvironment {
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn builtins() -> Self {
        let mut env = Self::empty();
        env.set("+", parse_type("num -> num -> num").unwrap());
        env.set("-", parse_type("num -> num -> num").unwrap());
        env.set("*", parse_type("num -> num -> num").unwrap());
        env.set("/", parse_type("num -> num -> num").unwrap());
        env.set("==", parse_type("num -> num -> bool").unwrap());
        env.set("<", parse_type("num -> num -> bool").unwrap());
        env.set(">", parse_type("num -> num -> bool").unwrap());
        env.set("&&", parse_type("bool -> bool -> bool").unwrap());
        env.set("||", parse_type("bool -> bool -> bool").unwrap());
        env.set("!", parse_type("bool -> bool").unwrap());
        env
    }

    pub fn get(&self, name: impl Into<String>) -> Option<&Spanned<AstType>> {
        self.bindings.get(&name.into())
    }

    pub fn set(&mut self, name: impl Into<String>, value: Spanned<AstType>) {
        self.bindings.insert(name.into(), value);
    }

    pub fn extend(&self, name: impl Into<String>, value: Spanned<AstType>) -> Self {
        let mut new_env = self.clone();
        new_env.set(name, value);
        new_env
    }
}

impl<'a> IntoIterator for &'a TyEnvironment {
    type Item = (&'a String, &'a Spanned<AstType>);
    type IntoIter = im::hashmap::Iter<'a, String, Spanned<AstType>>;

    fn into_iter(self) -> Self::IntoIter {
        self.bindings.iter()
    }
}

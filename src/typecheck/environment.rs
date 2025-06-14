use crate::typecheck::{Subst, Type, TypeVarContext};
use im::HashMap;

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct TypeEnvironment(HashMap<String, Type>);

impl TypeEnvironment {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn get(&self, name: impl Into<String>) -> Option<&Type> {
        self.0.get(&name.into())
    }

    pub fn set(&mut self, name: impl Into<String>, value: Type) {
        self.0.insert(name.into(), value);
    }

    pub fn extend(&self, name: impl Into<String>, value: Type) -> Self {
        let mut new_env = self.clone();
        new_env.set(name, value);
        new_env
    }

    pub fn iter(&self) -> im::hashmap::Iter<String, Type> {
        self.0.iter()
    }

    pub fn builtins(ctx: &mut TypeVarContext) -> Self {
        let mut env = Self::empty();
        env.set("Num", Type::Num);
        env.set("Bool", Type::Bool);

        for name in ["+", "-", "*", "/"] {
            env.set(name, Type::nary_func([Type::Num, Type::Num], Type::Num));
        }
        for name in [">", "<", ">=", "<="] {
            env.set(name, Type::nary_func([Type::Num, Type::Num], Type::Bool));
        }
        for name in ["==", "!="] {
            let a = ctx.fresh();
            env.set(
                name,
                Type::nary_func([Type::Var(a), Type::Var(a)], Type::Bool),
            );
        }
        for name in ["&&", "||"] {
            env.set(name, Type::nary_func([Type::Bool, Type::Bool], Type::Bool));
        }
        env
    }

    pub fn apply(&self, subst: &Subst) -> Self {
        Self(
            self.0
                .iter()
                .map(|(name, ty)| (name.clone(), subst.apply(ty.clone())))
                .collect(),
        )
    }
}

impl IntoIterator for TypeEnvironment {
    type Item = (String, Type);
    type IntoIter = im::hashmap::ConsumingIter<(String, Type)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

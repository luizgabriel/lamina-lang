use std::fmt::Display;

use im::HashSet;

use crate::parser::Literal;

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(pub usize);

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl From<usize> for TypeVar {
    fn from(n: usize) -> Self {
        TypeVar(n)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Num,
    Bool,
    Var(TypeVar),
    Fn(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Num => write!(f, "Num"),
            Type::Bool => write!(f, "Bool"),
            Type::Var(var) => write!(f, "{}", var),
            Type::Fn(lhs, rhs) => write!(f, "{} -> {}", lhs, rhs),
            Type::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    pub fn func(arg: Type, ret: Type) -> Self {
        Type::Fn(Box::new(arg), Box::new(ret))
    }

    pub fn nary_func(
        args: impl IntoIterator<Item = Type, IntoIter = impl DoubleEndedIterator<Item = Type>>,
        ret: Type,
    ) -> Self {
        args.into_iter()
            .rev()
            .fold(ret, |ret, arg| Type::func(arg, ret))
    }

    pub fn tuple(items: impl IntoIterator<Item = Type>) -> Self {
        Type::Tuple(items.into_iter().collect())
    }

    pub fn var(n: usize) -> Self {
        Type::Var(TypeVar(n))
    }
}

pub trait FreeVars {
    fn free_vars(&self) -> HashSet<TypeVar>;
}

impl FreeVars for Type {
    fn free_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Unit | Type::Num | Type::Bool => HashSet::new(),
            Type::Var(var) => HashSet::unit(*var),
            Type::Fn(lhs, rhs) => lhs.free_vars().union(rhs.free_vars()),
            Type::Tuple(items) => items.iter().flat_map(|t| t.free_vars()).collect(),
        }
    }
}

impl From<&Literal> for Type {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Unit => Type::Unit,
            Literal::Num(_) => Type::Num,
            Literal::Bool(_) => Type::Bool,
        }
    }
}

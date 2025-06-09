use std::fmt::Display;

use crate::lexer::Spanned;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstType {
    Unit,
    Cons(String),
    Tuple(Vec<Spanned<AstType>>),
    Fn(Box<Spanned<AstType>>, Box<Spanned<AstType>>),
}

impl AstType {
    pub fn cons(name: impl Into<String>) -> Self {
        AstType::Cons(name.into())
    }

    pub fn func(arg: Spanned<AstType>, ret: Spanned<AstType>) -> Self {
        AstType::Fn(Box::new(arg), Box::new(ret))
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<AstType>>) -> Self {
        AstType::Tuple(items.into_iter().collect())
    }
}

impl Display for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::Unit => write!(f, "()"),
            AstType::Cons(name) => write!(f, "{}", name),
            AstType::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|a| a.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AstType::Fn(arg, ret) => write!(f, "({} -> {})", arg.0, ret.0),
        }
    }
}

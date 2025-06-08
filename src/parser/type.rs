use std::fmt::Display;

use crate::{lexer::Spanned, parser::Literal};

#[derive(Clone, Debug)]
pub enum AstType {
    Unit,
    Var(usize),
    Num,
    Bool,
    Tuple(Vec<Spanned<AstType>>),
    Fn(Box<Spanned<AstType>>, Box<Spanned<AstType>>),
}

impl PartialEq for AstType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AstType::Unit, AstType::Unit) => true,
            (AstType::Var(a), AstType::Var(b)) => a == b,
            (AstType::Num, AstType::Num) => true,
            (AstType::Bool, AstType::Bool) => true,
            (AstType::Tuple(a), AstType::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.0 == b.0)
            }
            (AstType::Fn(a, b), AstType::Fn(c, d)) => a.0 == c.0 && b.0 == d.0,
            _ => false,
        }
    }
}

impl Eq for AstType {}

impl AstType {
    pub fn var(n: usize) -> Self {
        AstType::Var(n)
    }

    pub fn num() -> Self {
        AstType::Num
    }

    pub fn bool() -> Self {
        AstType::Bool
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
            AstType::Unit => write!(f, "unit"),
            AstType::Var(n) => write!(f, "{}", n),
            AstType::Num => write!(f, "num"),
            AstType::Bool => write!(f, "bool"),
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

impl From<Literal> for AstType {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Unit => AstType::Unit,
            Literal::Num(_) => AstType::Num,
            Literal::Bool(_) => AstType::Bool,
        }
    }
}

use std::fmt::Display;

use crate::{lexer::Spanned, syntax::AstLiteral};

#[derive(Clone, Debug, PartialEq)]
pub enum IrExpr<'src> {
    Ident(&'src str),
    Literal(AstLiteral),
    Tuple(Vec<Spanned<IrExpr<'src>>>),
    Lambda {
        arg: Spanned<&'src str>,
        body: Box<Spanned<IrExpr<'src>>>,
    },
    Let {
        name: Spanned<&'src str>,
        rhs: Box<Spanned<IrExpr<'src>>>,
        then: Box<Spanned<IrExpr<'src>>>,
    },
    FnApp {
        lhs: Box<Spanned<IrExpr<'src>>>,
        rhs: Box<Spanned<IrExpr<'src>>>,
    },
    If {
        condition: Box<Spanned<IrExpr<'src>>>,
        then_branch: Box<Spanned<IrExpr<'src>>>,
        else_branch: Box<Spanned<IrExpr<'src>>>,
    },
}

impl Display for IrExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrExpr::Ident(name) => write!(f, "{}", name),
            IrExpr::Literal(literal) => write!(f, "{}", literal),
            IrExpr::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|item| item.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IrExpr::Lambda { arg, body } => write!(f, "λ{} -> ({})", arg.0, body.0),
            IrExpr::Let { name, rhs, then } => {
                write!(f, "let {} = {} in {}", name.0, rhs.0, then.0)
            }
            IrExpr::FnApp { lhs, rhs } => write!(f, "{} {}", lhs.0, rhs.0),
            IrExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {} then {} else {}",
                    condition.0, then_branch.0, else_branch.0
                )
            }
        }
    }
}

impl<'src> IrExpr<'src> {
    pub fn ident(name: &'src str) -> Self {
        IrExpr::Ident(name)
    }

    pub fn literal(literal: impl Into<AstLiteral>) -> Self {
        IrExpr::Literal(literal.into())
    }

    pub fn tuple(items: Vec<Spanned<IrExpr<'src>>>) -> Self {
        IrExpr::Tuple(items)
    }

    pub fn lambda(arg: Spanned<&'src str>, body: Spanned<IrExpr<'src>>) -> Self {
        IrExpr::Lambda {
            arg,
            body: Box::new(body),
        }
    }

    pub fn let_binding(
        name: Spanned<&'src str>,
        rhs: Spanned<IrExpr<'src>>,
        then: Spanned<IrExpr<'src>>,
    ) -> Self {
        IrExpr::Let {
            name,
            rhs: Box::new(rhs),
            then: Box::new(then),
        }
    }

    pub fn fn_app(lhs: Spanned<IrExpr<'src>>, rhs: Spanned<IrExpr<'src>>) -> Self {
        IrExpr::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn if_expr(
        condition: Spanned<IrExpr<'src>>,
        then_branch: Spanned<IrExpr<'src>>,
        else_branch: Spanned<IrExpr<'src>>,
    ) -> Self {
        IrExpr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }
}

impl From<()> for IrExpr<'_> {
    fn from(_: ()) -> Self {
        IrExpr::literal(())
    }
}

impl From<f64> for IrExpr<'_> {
    fn from(n: f64) -> Self {
        IrExpr::literal(n)
    }
}

impl From<bool> for IrExpr<'_> {
    fn from(b: bool) -> Self {
        IrExpr::literal(b)
    }
}

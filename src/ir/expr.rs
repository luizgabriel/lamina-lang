use std::fmt::Display;

use crate::{lexer::Spanned, syntax::AstLiteral};

#[derive(Clone, Debug, PartialEq)]
pub enum IrExprNode<'src, T> {
    Ident(&'src str),
    Literal(AstLiteral),
    Tuple(Vec<Spanned<T>>),
    Lambda {
        arg: Spanned<&'src str>,
        body: Box<Spanned<T>>,
    },
    Let {
        name: Spanned<&'src str>,
        rhs: Box<Spanned<T>>,
        then: Box<Spanned<T>>,
    },
    FnApp {
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    If {
        condition: Box<Spanned<T>>,
        then_branch: Box<Spanned<T>>,
        else_branch: Box<Spanned<T>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrExpr<'src>(pub IrExprNode<'src, IrExpr<'src>>);

impl<'src> AsRef<IrExprNode<'src, IrExpr<'src>>> for IrExpr<'src> {
    fn as_ref(&self) -> &IrExprNode<'src, IrExpr<'src>> {
        &self.0
    }
}

impl Display for IrExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            IrExprNode::Ident(name) => write!(f, "{}", name),
            IrExprNode::Literal(literal) => write!(f, "{}", literal),
            IrExprNode::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|item| item.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IrExprNode::Lambda { arg, body } => write!(f, "λ{} -> ({})", arg.0, body.0),
            IrExprNode::Let { name, rhs, then } => {
                write!(f, "let {} = {} in {}", name.0, rhs.0, then.0)
            }
            IrExprNode::FnApp { lhs, rhs } => write!(f, "{} {}", lhs.0, rhs.0),
            IrExprNode::If {
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
        IrExpr(IrExprNode::Ident(name))
    }

    pub fn literal(literal: impl Into<AstLiteral>) -> Self {
        IrExpr(IrExprNode::Literal(literal.into()))
    }

    pub fn tuple(items: Vec<Spanned<IrExpr<'src>>>) -> Self {
        IrExpr(IrExprNode::Tuple(items))
    }

    pub fn lambda(arg: Spanned<&'src str>, body: Spanned<IrExpr<'src>>) -> Self {
        IrExpr(IrExprNode::Lambda {
            arg,
            body: Box::new(body),
        })
    }

    pub fn let_binding(
        name: Spanned<&'src str>,
        rhs: Spanned<IrExpr<'src>>,
        then: Spanned<IrExpr<'src>>,
    ) -> Self {
        IrExpr(IrExprNode::Let {
            name,
            rhs: Box::new(rhs),
            then: Box::new(then),
        })
    }

    pub fn fn_app(lhs: Spanned<IrExpr<'src>>, rhs: Spanned<IrExpr<'src>>) -> Self {
        IrExpr(IrExprNode::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn if_expr(
        condition: Spanned<IrExpr<'src>>,
        then_branch: Spanned<IrExpr<'src>>,
        else_branch: Spanned<IrExpr<'src>>,
    ) -> Self {
        IrExpr(IrExprNode::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
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

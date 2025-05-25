use std::fmt::Display;

use chumsky::span::SimpleSpan;

use crate::{
    lexer::Spanned,
    syntax::{Expr, Literal, Stmt},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CoreLang<'src> {
    Ident(&'src str),
    Literal(Literal),
    Tuple(Vec<Spanned<CoreLang<'src>>>),
    Lambda {
        arg: Spanned<&'src str>,
        body: Box<Spanned<CoreLang<'src>>>,
    },
    Let {
        name: Spanned<&'src str>,
        rhs: Box<Spanned<CoreLang<'src>>>,
        then: Box<Spanned<CoreLang<'src>>>,
    },
    FnApp {
        lhs: Box<Spanned<CoreLang<'src>>>,
        rhs: Box<Spanned<CoreLang<'src>>>,
    },
}

impl Display for CoreLang<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoreLang::Ident(name) => write!(f, "{}", name),
            CoreLang::Literal(literal) => write!(f, "{}", literal),
            CoreLang::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|item| item.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CoreLang::Lambda { arg, body } => write!(f, "λ{} -> ({})", arg.0, body.0),
            CoreLang::Let { name, rhs, then } => {
                write!(f, "let {} = {} in {}", name.0, rhs.0, then.0)
            }
            CoreLang::FnApp { lhs, rhs } => write!(f, "{} {}", lhs.0, rhs.0),
        }
    }
}

impl<'src> CoreLang<'src> {
    pub fn ident(name: &'src str) -> Self {
        CoreLang::Ident(name)
    }

    pub fn literal(literal: impl Into<Literal>) -> Self {
        CoreLang::Literal(literal.into())
    }

    pub fn tuple(items: Vec<Spanned<CoreLang<'src>>>) -> Self {
        CoreLang::Tuple(items)
    }

    pub fn lambda(arg: Spanned<&'src str>, body: Spanned<CoreLang<'src>>) -> Self {
        CoreLang::Lambda {
            arg,
            body: Box::new(body),
        }
    }

    pub fn let_binding(
        name: Spanned<&'src str>,
        rhs: Spanned<CoreLang<'src>>,
        then: Spanned<CoreLang<'src>>,
    ) -> Self {
        CoreLang::Let {
            name,
            rhs: Box::new(rhs),
            then: Box::new(then),
        }
    }

    pub fn fn_app(lhs: Spanned<CoreLang<'src>>, rhs: Spanned<CoreLang<'src>>) -> Self {
        CoreLang::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<()> for CoreLang<'_> {
    fn from(_: ()) -> Self {
        CoreLang::literal(())
    }
}

impl From<f64> for CoreLang<'_> {
    fn from(n: f64) -> Self {
        CoreLang::literal(n)
    }
}

impl From<bool> for CoreLang<'_> {
    fn from(b: bool) -> Self {
        CoreLang::literal(b)
    }
}

pub fn lowering_stmt<'src>(stmt: Stmt<'src>, then: Spanned<CoreLang<'src>>) -> CoreLang<'src> {
    match stmt {
        Stmt::Expr(expr) => CoreLang::let_binding(("_", expr.1), lowering_expr(expr), then),

        Stmt::Let { name, body } => CoreLang::let_binding(name, lowering_expr(body), then),

        Stmt::FnDef { name, args, body } => {
            let curried: Spanned<CoreLang<'_>> = args.into_iter().rfold(
                lowering_expr(body),
                |(body, body_span), (arg, arg_span)| {
                    let final_span = SimpleSpan::from(arg_span.start..body_span.end);
                    (
                        CoreLang::lambda((arg, arg_span), (body, body_span)),
                        final_span,
                    )
                },
            );

            CoreLang::let_binding(name, curried, then)
        }
    }
}

pub fn lowering_expr((expr, span): Spanned<Expr<'_>>) -> Spanned<CoreLang<'_>> {
    match expr {
        Expr::Ident(name) => (CoreLang::ident(name), span),
        Expr::Literal(literal) => (CoreLang::literal(literal), span),
        Expr::Tuple(items) => (
            CoreLang::tuple(
                items
                    .into_iter()
                    .map(|item| lowering_expr(item))
                    .collect::<Vec<_>>(),
            ),
            span,
        ),
        Expr::FnApp { lhs, rhs } => (
            CoreLang::fn_app(lowering_expr(*lhs), lowering_expr(*rhs)),
            span,
        ),
        Expr::Block { statements, expr } => {
            let expr: Spanned<CoreLang<'_>> = expr.map_or(
                (CoreLang::literal(()), SimpleSpan::from(span.end..span.end)),
                |b| lowering_expr(*b),
            );

            statements
                .into_iter()
                .rfold(expr, |acc, (stmt, s)| (lowering_stmt(stmt, acc), s))
        }
        Expr::OpApp { op, lhs, rhs } => {
            let op_ident: Spanned<CoreLang<'_>> = (CoreLang::ident(op.0), op.1);
            let op_app_span = SimpleSpan::from(lhs.1.start..op.1.end);

            let lhs = lowering_expr(*lhs);
            let rhs = lowering_expr(*rhs);

            let op_app = (CoreLang::fn_app(op_ident, lhs), op_app_span);
            (CoreLang::fn_app(op_app, rhs), span)
        }
    }
}

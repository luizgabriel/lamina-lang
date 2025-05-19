use crate::lexer::{Span, Spanned, Token};
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::pratt::{infix, left};
use chumsky::{Parser, prelude::*};
use std::fmt::Display;
use trait_set::trait_set;

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxNode<'src, T> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    Let {
        lhs: Spanned<&'src str>,
        rhs: Box<Spanned<T>>,
        then: Box<Spanned<T>>,
    },
    FnDef {
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Box<Spanned<T>>,
    },
    LambdaFn {
        args: Vec<Spanned<&'src str>>,
        body: Box<Spanned<T>>,
    },
    FnApp {
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    OpApp {
        op: Spanned<&'src str>,
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    Block {
        body: Vec<Spanned<T>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxTree<'src>(SyntaxNode<'src, SyntaxTree<'src>>);

impl<'src> SyntaxTree<'src> {
    pub fn var(name: &'src str) -> Self {
        Self(SyntaxNode::Var(name))
    }

    pub fn num(n: f64) -> Self {
        Self(SyntaxNode::Num(n))
    }

    pub fn bool(b: bool) -> Self {
        Self(SyntaxNode::Bool(b))
    }

    pub fn let_expr(lhs: Spanned<&'src str>, rhs: Spanned<Self>, then: Spanned<Self>) -> Self {
        Self(SyntaxNode::Let {
            lhs,
            rhs: Box::new(rhs),
            then: Box::new(then),
        })
    }

    pub fn fn_def(
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<Self>,
    ) -> Self {
        Self(SyntaxNode::FnDef {
            name,
            args,
            body: Box::new(body),
        })
    }

    pub fn lambda_fn(args: Vec<Spanned<&'src str>>, body: Spanned<Self>) -> Self {
        Self(SyntaxNode::LambdaFn {
            args,
            body: Box::new(body),
        })
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(SyntaxNode::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn op_app(op: Spanned<&'src str>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(SyntaxNode::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn block(body: Vec<Spanned<Self>>) -> Self {
        Self(SyntaxNode::Block { body })
    }
}

impl Display for SyntaxTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            SyntaxNode::Var(name) => write!(f, "{}", name),
            SyntaxNode::Num(n) => write!(f, "{}", n),
            SyntaxNode::Bool(b) => write!(f, "{}", b),
            SyntaxNode::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            SyntaxNode::OpApp { op, lhs, rhs } => write!(f, "({} {} {})", op.0, lhs.0, rhs.0),
            _ => todo!(),
        }
    }
}

trait_set! {
    pub trait TokenInput<'src> = chumsky::input::ValueInput<'src, Token = Token<'src>, Span = Span>;
    pub trait SyntaxParser<'src, I: TokenInput<'src>, T> = chumsky::Parser<'src, I, T, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone;
}

fn literal<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
    select! {
        Token::Num(n) => SyntaxTree::num(n),
        Token::True => SyntaxTree::bool(true),
        Token::False => SyntaxTree::bool(false),
    }
    .map_with(|s, e| (s, e.span()))
    .labelled("literal")
}

fn ident<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
    select! { Token::Ident(ident) => SyntaxTree::var(ident) }
        .map_with(|s, e| (s, e.span()))
        .labelled("identifier")
}

fn operator<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Operator(op) => op }
        .map_with(|s, e| (s, e.span()))
        .labelled("operator")
}

pub fn parser<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>>
{
    recursive(|expr| {
        let block = expr
            .clone()
            .separated_by(just(Token::Ctrl(';')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .map_with(|s, e| (SyntaxTree::block(s), e.span()))
            .labelled("block");

        let parens = expr
            .clone()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .labelled("parens");

        let atom = choice((parens, block, literal(), ident()));

        atom.pratt((
            infix(left(10), empty(), |lhs, _, rhs, e| {
                (SyntaxTree::fn_app(lhs, rhs), e.span())
            }),
            infix(left(1), operator(), |lhs, op, rhs, e| {
                (SyntaxTree::op_app(op, lhs, rhs), e.span())
            }),
        ))
    })
}

pub type ErrorReport<'a> = Report<'a, ((), std::ops::Range<usize>)>;

pub fn errors_to_report<'a>(errors: &[Rich<'a, impl Display>]) -> ErrorReport<'a> {
    let (first, rest) = errors.split_first().expect("Expected at least one error");

    Report::build(ReportKind::Error, ((), first.span().into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(first.to_string())
        .with_labels(rest.iter().map(|e| {
            Label::new(((), e.span().into_range()))
                .with_message(e.reason().to_string())
                .with_color(Color::Red)
        }))
        .finish()
}

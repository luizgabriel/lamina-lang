use crate::syntax::{Expr, Literal, Module, Span, Spanned, Stmt, Token};
use chumsky::pratt::{infix, left};
use chumsky::{Parser, prelude::*};
use trait_set::trait_set;

trait_set! {
    pub trait TokenInput<'src> = chumsky::input::ValueInput<'src, Token = Token<'src>, Span = Span>;
    pub trait SyntaxParser<'src, I: TokenInput<'src>, T> = chumsky::Parser<'src, I, T, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone;
}

fn literal<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Literal> {
    select! {
        Token::Num(n) => Literal::Num(n),
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
    }
    .labelled("literal")
}

fn ident<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Ident(ident) => ident }.map_with(|s, e| (s, e.span()))
}

fn literal_expr<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    literal().map_with(|l, e| (Expr::literal(l), e.span()))
}

fn identifier_expr<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    select! { Token::Ident(ident) => Expr::ident(ident) }
        .map_with(|s, e| (s, e.span()))
        .labelled("identifier")
}

fn operator<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Op(op) => op }
        .map_with(|s, e| (s, e.span()))
        .labelled("operator")
}

fn let_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Stmt<'src>>> {
    let var = ident().labelled("variable name");
    let eq = just(Token::Op("="));

    just(Token::Let)
        .ignore_then(group((var, eq, expr.clone())))
        .map_with(|(name, _, body), e| (Stmt::let_def(name, body), e.span()))
        .labelled("let definition")
}

pub fn statement<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Stmt<'src>>> {
    choice((
        let_def(expr.clone()),
        fn_def(expr.clone()),
        expr.clone().map_with(|s, e| (Stmt::expr(s), e.span())),
    ))
    .then_ignore(just(Token::Semi))
}

fn block<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    let open = just(Token::Ctrl('{'));
    let close = just(Token::Ctrl('}'));

    statement(expr.clone())
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.or_not())
        .map_with(|(stmt, expr), e| (Expr::block(stmt, expr), e.span()))
        .delimited_by(open, close)
        .labelled("block")
}

fn parens<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    let open = just(Token::Ctrl('('));
    let close = just(Token::Ctrl(')'));

    choice((
        open.clone()
            .then(close.clone())
            .labelled("empty tuple")
            .map_with(|_, e| (Expr::literal(()), e.span())),
        expr.clone()
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(open.clone(), close.clone())
            .map_with(|s, e| (Expr::tuple(s), e.span()))
            .labelled("tuple"),
        expr.delimited_by(open.clone(), close.clone())
            .labelled("parenthesized expression"),
    ))
}

pub fn expression<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    recursive(|expr| {
        // { <expr>; <expr>; <expr> }
        let block = block(expr.clone());

        // (<expr>)
        let parens = parens(expr.clone());

        let atom = choice((identifier_expr(), literal_expr(), parens, block));

        atom.pratt((
            infix(left(10), empty(), |lhs, _, rhs, e| {
                (Expr::fn_app(lhs, rhs), e.span())
            }),
            infix(left(1), operator(), |lhs, op, rhs, e| {
                (Expr::op_app(op, lhs, rhs), e.span())
            }),
        ))
    })
}

fn fn_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Stmt<'src>>> {
    let args = ident()
        .labelled("argument")
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>();

    just(Token::Fn)
        .ignore_then(group((
            ident().labelled("function name"),
            args,
            just(Token::Op("=")),
            expr,
        )))
        .map_with(|(name, args, _, body), e| (Stmt::fn_def(name, args, body), e.span()))
        .labelled("function definition")
}

pub fn module<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Module<'src>>> {
    let item = statement(expression());

    item.separated_by(just(Token::Semi))
        .allow_trailing()
        .collect::<Vec<_>>()
        .map_with(|items, e| (Module::new(items), e.span()))
        .labelled("module")
}

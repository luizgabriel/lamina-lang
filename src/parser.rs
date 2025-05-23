use crate::syntax::{Expr, Span, Spanned, Statement, Token};
use chumsky::pratt::{infix, left};
use chumsky::{Parser, prelude::*};
use trait_set::trait_set;

trait_set! {
    pub trait TokenInput<'src> = chumsky::input::ValueInput<'src, Token = Token<'src>, Span = Span>;
    pub trait SyntaxParser<'src, I: TokenInput<'src>, T> = chumsky::Parser<'src, I, T, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone;
}

fn literal<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    select! {
        Token::Num(n) => Expr::num(n),
        Token::True => Expr::bool(true),
        Token::False => Expr::bool(false),
    }
    .map_with(|s, e| (s, e.span()))
    .labelled("literal")
}

fn ident<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Ident(ident) => ident }.map_with(|s, e| (s, e.span()))
}

fn var<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    select! { Token::Ident(ident) => Expr::var(ident) }
        .map_with(|s, e| (s, e.span()))
        .labelled("identifier")
}

fn operator<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Op(op) => op }
        .map_with(|s, e| (s, e.span()))
        .labelled("operator")
}

fn block<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    let open = just(Token::Ctrl('{'));
    let close = just(Token::Ctrl('}'));
    let semicolon = just(Token::Ctrl(';'));

    statement(expr)
        .separated_by(semicolon)
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(open, close)
        .map_with(|s, e| (Expr::block(s), e.span()))
        .labelled("block")
}

fn parens<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    let open = just(Token::Ctrl('('));
    let close = just(Token::Ctrl(')'));

    expr.delimited_by(open, close)
        .labelled("parenthesized expression")
}

fn fn_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    let args = ident()
        .labelled("argument")
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>();

    just(Token::Fn)
        .ignore_then(group((
            ident().labelled("function name"),
            args,
            just(Token::Op("=")).ignore_then(expr.clone()),
        )))
        .map_with(|(name, args, body), e| (Expr::fn_def(name, args, body), e.span()))
        .labelled("function definition")
}

fn statement<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<Expr<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<Statement<'src>>> {
    let eq = just(Token::Op("="));

    choice((
        just(Token::Let)
            .ignore_then(group((
                ident().labelled("variable name"),
                eq.ignore_then(expr.clone()),
            )))
            .map(|(name, rhs)| Statement::assignment(name, rhs)),
        expr.map(Statement::expr),
    ))
    .map_with(|stmt, e| (stmt, e.span()))
}

pub fn parser<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<Expr<'src>>> {
    recursive(|expr| {
        // { <expr>; <expr>; <expr> }
        let block = block(expr.clone());

        // (<expr>)
        let parens = parens(expr.clone());

        // fn <ident> <args> = <expr>
        let fn_def = fn_def(expr.clone());

        let atom = choice((parens, block, fn_def, literal(), var()));

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

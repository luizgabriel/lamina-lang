use crate::syntax::{Span, Spanned, SyntaxTree, Token};
use chumsky::pratt::{infix, left};
use chumsky::{Parser, prelude::*};
use trait_set::trait_set;

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

fn ident<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Ident(ident) => ident }.map_with(|s, e| (s, e.span()))
}

fn var<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
    select! { Token::Ident(ident) => SyntaxTree::var(ident) }
        .map_with(|s, e| (s, e.span()))
        .labelled("identifier")
}

fn operator<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<&'src str>> {
    select! { Token::Op(op) => op }
        .map_with(|s, e| (s, e.span()))
        .labelled("operator")
}

fn block<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
    expr.separated_by(just(Token::Ctrl(';')))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
        .map_with(|s, e| (SyntaxTree::block(s), e.span()))
        .labelled("block")
}

fn parens<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
    expr.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("parenthesized expression")
}

fn fn_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>>,
) -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>> {
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
        .map_with(|(name, args, body), e| (SyntaxTree::fn_def(name, args, body), e.span()))
        .labelled("function definition")
}

pub fn parser<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<SyntaxTree<'src>>>
{
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
                (SyntaxTree::fn_app(lhs, rhs), e.span())
            }),
            infix(left(1), operator(), |lhs, op, rhs, e| {
                (SyntaxTree::op_app(op, lhs, rhs), e.span())
            }),
        ))
    })
}

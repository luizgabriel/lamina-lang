use crate::lexer::{Span, Spanned, Token};
use crate::parser::{AstExpr, AstLiteral, AstModule, AstStmt};
use chumsky::pratt::{infix, left};
use chumsky::{Parser, prelude::*};
use trait_set::trait_set;

trait_set! {
    pub trait TokenInput<'src> = chumsky::input::ValueInput<'src, Token = Token<'src>, Span = Span>;
    pub trait SyntaxParser<'src, I: TokenInput<'src>, O> = chumsky::Parser<'src, I, O, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone;
}

fn literal<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, AstLiteral> {
    select! {
        Token::Num(n) => n.into(),
        Token::True => true.into(),
        Token::False => false.into(),
    }
    .labelled("literal")
}

fn ident<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<String>> {
    select! { Token::Ident(ident) => ident.to_string() }.map_with(|s, e| (s, e.span()))
}

fn literal_expr<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    literal().map_with(|l, e| (AstExpr::literal(l), e.span()))
}

fn identifier_expr<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    let ident = select! { Token::Ident(ident) => ident }
        .map_with(|s, e| (AstExpr::ident(s), e.span()))
        .labelled("identifier");

    let open = just(Token::OpenCtrl('('));
    let close = just(Token::CloseCtrl(')'));

    let op = select! { Token::Op(op) => op }
        .delimited_by(open, close)
        .map_with(|s, e| (AstExpr::ident(s), e.span()))
        .labelled("operator");

    choice((ident, op))
}

fn operator<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<String>> {
    select! { Token::Op(op) => op.to_string() }
        .map_with(|s, e| (s, e.span()))
        .labelled("operator")
}

fn let_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstStmt>> {
    let var = ident().labelled("variable name");
    let eq = just(Token::Op("="));

    group((var, eq, expr.clone()))
        .map_with(|(name, _, body), e| (AstStmt::let_def(name, body), e.span()))
        .labelled("variable definition")
}

pub fn statement<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstStmt>> {
    choice((
        let_def(expr.clone()),
        fn_def(expr.clone()),
        expr.clone().map_with(|s, e| (AstStmt::expr(s), e.span())),
    ))
    .then_ignore(choice((just(Token::Semi), just(Token::VirtualSemi))))
}

fn block<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    let open = just(Token::OpenCtrl('{'));
    let close = just(Token::CloseCtrl('}'));

    statement(expr.clone())
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.or_not())
        .map_with(|(stmt, expr), e| (AstExpr::block(stmt, expr), e.span()))
        .delimited_by(open, close)
        .labelled("block")
}

fn parens<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    let open = just(Token::OpenCtrl('('));
    let close = just(Token::CloseCtrl(')'));

    choice((
        open.clone()
            .then(close.clone())
            .labelled("empty tuple")
            .map_with(|_, e| (AstExpr::literal(()), e.span())),
        expr.clone()
            .separated_by(just(Token::Comma))
            .at_least(2)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(open.clone(), close.clone())
            .map_with(|s, e| (AstExpr::tuple(s), e.span()))
            .labelled("tuple"),
        expr.delimited_by(open.clone(), close.clone())
            .labelled("parenthesized expression"),
    ))
}

fn if_expr<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    just(Token::If)
        .ignore_then(expr.clone())
        .then_ignore(just(Token::Then))
        .then(expr.clone())
        .then_ignore(just(Token::Else))
        .then(expr)
        .map_with(|((condition, then_branch), else_branch), e| {
            (
                AstExpr::if_expr(condition, then_branch, else_branch),
                e.span(),
            )
        })
        .labelled("if expression")
}

// identifier -> <expr>
fn lambda<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    let param = ident().labelled("parameter");
    let arrow = just(Token::Op("->"));

    group((param, arrow, expr))
        .map_with(|(name, _, body), e| (AstExpr::lambda(name, body), e.span()))
        .labelled("lambda expression")
}

pub fn expression<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<AstExpr>> {
    recursive(|expr| {
        // { <expr>; <expr>; <expr> }
        let block = block(expr.clone());

        // (<expr>)
        let parens = parens(expr.clone());

        // if <expr> then <expr> else <expr>
        let if_expr = if_expr(expr.clone());

        let lambda_expr = lambda(expr.clone());

        let atom = choice((
            block,
            parens,
            if_expr,
            lambda_expr,
            identifier_expr(),
            literal_expr(),
        ));

        atom.pratt((
            // function application
            infix(left(10), empty(), |lhs, _, rhs, e| {
                (AstExpr::fn_app(lhs, rhs), e.span())
            }),
            // operator application
            infix(left(1), operator(), |lhs, op, rhs, e| {
                (AstExpr::op_app(op, lhs, rhs), e.span())
            }),
        ))
    })
}

fn fn_def<'src, I: TokenInput<'src>>(
    expr: impl SyntaxParser<'src, I, Spanned<AstExpr>>,
) -> impl SyntaxParser<'src, I, Spanned<AstStmt>> {
    let args = ident()
        .labelled("argument")
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>();

    group((
        ident().labelled("function name"),
        args,
        just(Token::Op("=")),
        expr,
    ))
    .map_with(|(name, args, _, body), e| (AstStmt::fn_def(name, args, body), e.span()))
    .labelled("function definition")
}

pub fn module<'src, I: TokenInput<'src>>() -> impl SyntaxParser<'src, I, Spanned<AstModule>> {
    let item = statement(expression());

    item.repeated()
        .collect::<Vec<_>>()
        .map_with(|items, e| (AstModule::new(items), e.span()))
        .labelled("module")
}

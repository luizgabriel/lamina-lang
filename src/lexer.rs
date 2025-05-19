use std::fmt::Display;

use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use trait_set::trait_set;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'src> {
    Num(f64),
    Ident(&'src str),
    Operator(&'src str),
    Ctrl(char),
    Let,
    In,
    Fn,
    True,
    False,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{}", n),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Operator(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

trait_set! {
    pub trait Lexer<'src, T> = chumsky::Parser<'src, &'src str, T, extra::Err<Rich<'src, char>>>;
}

pub fn lexer<'src>() -> impl Lexer<'src, Vec<Spanned<Token<'src>>>> {
    let keyword = text::ident().map(|s| match s {
        "let" => Token::Let,
        "in" => Token::In,
        "fn" => Token::Fn,
        "true" => Token::True,
        "false" => Token::False,
        s => Token::Ident(s),
    });

    let ctrl = one_of("(){}[];,").map(Token::Ctrl);

    let op = one_of("+*-/!=<|>&^")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Operator);

    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num);

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let token = choice((num, ctrl, op, keyword));

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

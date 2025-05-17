use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use std::fmt::Display;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(f64),
    Parens(Vec<Spanned<Self>>),

    // Ops
    Eq,
    Plus,
    Asterisk,

    // Keywords
    Let,
    In,
    Fn,
    True,
    False,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{}", s),
            Token::Num(n) => write!(f, "{}", n),
            Token::Parens(t) => write!(
                f,
                "({})",
                t.iter()
                    .map(|t| t.0.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Token::Eq => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    recursive(|token| {
        choice((
            // Keywords
            text::ident().map(|s| match s {
                "let" => Token::Let,
                "in" => Token::In,
                "fn" => Token::Fn,
                "true" => Token::True,
                "false" => Token::False,
                s => Token::Ident(s),
            }),
            // Operators
            just("=").to(Token::Eq),
            just("+").to(Token::Plus),
            just("*").to(Token::Asterisk),
            // Numbers
            text::int(10)
                .then(just('.').then(text::digits(10)).or_not())
                .to_slice()
                .map(|s: &str| Token::Num(s.parse().unwrap())),
            token
                .repeated()
                .collect()
                .delimited_by(just('('), just(')'))
                .labelled("token tree")
                .as_context()
                .map(Token::Parens),
        ))
        .map_with(|t, e| (t, e.span()))
        .padded()
    })
    .repeated()
    .collect()
}

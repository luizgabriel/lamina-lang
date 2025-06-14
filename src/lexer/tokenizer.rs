use super::{semicolon::insert_virtual_semicolons, span::Spanned, Token};
use chumsky::prelude::*;
use trait_set::trait_set;

trait_set! {
    pub trait Lexer<'src, T> = chumsky::Parser<'src, &'src str, T, extra::Err<Rich<'src, char>>>;
}

pub fn lexer<'src>() -> impl Lexer<'src, Vec<Spanned<Token<'src>>>> {
    let keyword = text::ident()
        .map(|s| match s {
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            s => Token::Ident(s),
        })
        .labelled("keyword/identifier");

    let semi = just(";").map(|_| Token::Semi).labelled("semicolon");

    let comma = just(",").map(|_| Token::Comma).labelled("comma");

    let colon = just(":").map(|_| Token::Colon).labelled("colon");

    // Newlines as tokens
    let newline = just('\n')
        .repeated()
        .at_least(1)
        .map(|_| Token::Newline)
        .labelled("newline");

    let open_ctrl = one_of("({[")
        .map(Token::OpenCtrl)
        .labelled("open control character");

    let close_ctrl = one_of(")]}")
        .map(Token::CloseCtrl)
        .labelled("close control character");

    let op = one_of("+*-/!=<|>&^")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|op| match op {
            "->" => Token::RightArrow,
            "=" => Token::Equal,
            other => Token::Op(other),
        })
        .labelled("operator");

    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num)
        .labelled("number");

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .labelled("comment");

    // Horizontal whitespace only (spaces and tabs, not newlines)
    let horizontal_whitespace = one_of(" \t").repeated();

    let token = choice((
        num, semi, comma, colon, newline, open_ctrl, close_ctrl, op, keyword,
    ));

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded_by(horizontal_whitespace)
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        .map(insert_virtual_semicolons)
}

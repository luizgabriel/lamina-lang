use chumsky::Parser;
use k9::snapshot;
use lamina_lang::lexer::{Span, Token, lexer};

#[inline]
fn span(start: usize, end: usize) -> Span {
    (start..end).into()
}

#[test]
fn test_keywords() {
    let src = "let fn true false";
    let tokens = lexer().parse(src).unwrap();
    let expected = vec![
        (Token::Let, span(0, 3)),
        (Token::Fn, span(4, 6)),
        (Token::True, span(7, 11)),
        (Token::False, span(12, 17)),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_identifiers() {
    let src = "foo bar";
    let tokens = lexer().parse(src).unwrap();
    let expected = vec![
        (Token::Ident("foo"), span(0, 3)),
        (Token::Ident("bar"), span(4, 7)),
    ];
    assert_eq!(tokens, expected);
}

#[allow(clippy::approx_constant)]
#[test]
fn test_numbers() {
    let src = "42 3.14";
    let tokens = lexer().parse(src).unwrap();
    let expected = vec![
        (Token::Num(42.0), span(0, 2)),
        (Token::Num(3.14), span(3, 7)),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_operators() {
    let src = "= + *";
    let tokens = lexer().parse(src).unwrap();
    let expected = vec![
        (Token::Eq, span(0, 1)),
        (Token::Plus, span(2, 3)),
        (Token::Asterisk, span(4, 5)),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_parens() {
    let src = "(foo)";
    let tokens = lexer().parse(src).unwrap();
    let expected = vec![(
        Token::Parens(vec![(Token::Ident("foo"), span(1, 4))]),
        span(0, 5),
    )];
    assert_eq!(tokens, expected);
}

#[test]
fn test_error_output() {
    let src = "let @ foo";
    let result = lexer().parse(src).into_result();
    snapshot!(
        result.unwrap_err().first().unwrap(),
        r#"found ''@'' at 4..5 expected identifier, ''='', ''+'', ''*'', non-zero digit, ''0'', token tree, or end of input"#
    );
}

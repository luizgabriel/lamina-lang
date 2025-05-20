use chumsky::Parser;
use k9::snapshot;
use lamina_lang::{
    lexer::lexer,
    syntax::{Span, Token},
};

#[macro_export]
macro_rules! assert_lex {
    ($src:expr, [ $( $tok:expr ),* $(,)? ]) => {
        let tokens = lexer().parse($src).unwrap();
        assert_eq!(tokens, vec![$($tok),*]);
    };
}

fn span(start: usize, end: usize) -> Span {
    (start..end).into()
}

#[test]
fn test_keywords() {
    let src = "let fn true false";
    assert_lex!(
        src,
        [
            (Token::Let, span(0, 3)),
            (Token::Fn, span(4, 6)),
            (Token::True, span(7, 11)),
            (Token::False, span(12, 17)),
        ]
    );
}

#[test]
fn test_identifiers() {
    let src = "foo bar";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            (Token::Ident("bar"), span(4, 7)),
        ]
    );
}

#[allow(clippy::approx_constant)]
#[test]
fn test_numbers() {
    let src = "42 3.14";
    assert_lex!(
        src,
        [
            (Token::Num(42.0), span(0, 2)),
            (Token::Num(3.14), span(3, 7)),
        ]
    );
}

#[test]
fn test_operators() {
    let src = "= + *";
    assert_lex!(
        src,
        [
            (Token::Operator("="), span(0, 1)),
            (Token::Operator("+"), span(2, 3)),
            (Token::Operator("*"), span(4, 5)),
        ]
    );
}

#[test]
fn test_parens() {
    let src = "(foo)";
    assert_lex!(
        src,
        [
            (Token::Ctrl('('), span(0, 1)),
            (Token::Ident("foo"), span(1, 4)),
            (Token::Ctrl(')'), span(4, 5)),
        ]
    );
}

#[test]
fn test_error_output() {
    let src = "let @ foo";
    let result = lexer().parse(src).into_result();
    snapshot!(
        result.unwrap_err().first().unwrap(),
        r#"found ''@'' at 4..5 expected ''#'', comment, number, control character, operator, or keyword/identifier"#
    );
}

#[test]
fn test_custom_operators() {
    let src = ">>> && || |> <=> !=";
    assert_lex!(
        src,
        [
            (Token::Operator(">>>"), span(0, 3)),
            (Token::Operator("&&"), span(4, 6)),
            (Token::Operator("||"), span(7, 9)),
            (Token::Operator("|>"), span(10, 12)),
            (Token::Operator("<=>"), span(13, 16)),
            (Token::Operator("!="), span(17, 19)),
        ]
    );
}

#[test]
fn test_comments() {
    let src = "foo # bar\n baz";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            (Token::Ident("baz"), span(11, 14)),
        ]
    );
}

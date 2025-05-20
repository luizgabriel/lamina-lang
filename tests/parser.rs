use chumsky::input::Stream;
use chumsky::prelude::*;
use lamina_lang::{
    lexer::lexer,
    parser::parser,
    syntax::{Span, SyntaxTree},
};

#[macro_export]
macro_rules! assert_parse {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer().parse($src).unwrap();
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = parser().parse(tokens).unwrap();
        assert_eq!(ast, $expected);
    };
}

fn span(start: usize, end: usize) -> Span {
    (start..end).into()
}

#[test]
fn test_parse_var() {
    assert_parse!("foo", (SyntaxTree::var("foo"), span(0, 3)));
    assert_parse!("foo_bar", (SyntaxTree::var("foo_bar"), span(0, 7)));
}

#[test]
fn test_parse_num() {
    assert_parse!("123", (SyntaxTree::num(123.0), span(0, 3)));
    assert_parse!("123.456", (SyntaxTree::num(123.456), span(0, 7)));
}

#[test]
fn test_parse_bool() {
    assert_parse!("true", (SyntaxTree::bool(true), span(0, 4)));
    assert_parse!("false", (SyntaxTree::bool(false), span(0, 5)));
}

#[test]
fn test_op_app() {
    assert_parse!(
        "1 + 2",
        (
            SyntaxTree::op_app(
                ("+", span(2, 3)),
                (SyntaxTree::num(1.0), span(0, 1)),
                (SyntaxTree::num(2.0), span(4, 5))
            ),
            span(0, 5)
        )
    );
}

#[test]
fn test_fn_app() {
    assert_parse!(
        "f 1",
        (
            SyntaxTree::fn_app(
                (SyntaxTree::var("f"), span(0, 1)),
                (SyntaxTree::num(1.0), span(2, 3))
            ),
            span(0, 3)
        )
    );
}

#[test]
fn binding_strength() {
    assert_parse!(
        "f 1 + g 2",
        (
            SyntaxTree::op_app(
                ("+", span(4, 5)),
                (
                    SyntaxTree::fn_app(
                        (SyntaxTree::var("f"), span(0, 1)),
                        (SyntaxTree::num(1.0), span(2, 3))
                    ),
                    span(0, 3)
                ),
                (
                    SyntaxTree::fn_app(
                        (SyntaxTree::var("g"), span(6, 7)),
                        (SyntaxTree::num(2.0), span(8, 9))
                    ),
                    span(6, 9)
                )
            ),
            span(0, 9)
        )
    );
}

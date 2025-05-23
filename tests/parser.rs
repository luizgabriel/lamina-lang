use chumsky::input::Stream;
use chumsky::prelude::*;
use lamina_lang::{
    lexer::lexer,
    parser::parser,
    syntax::{Expr, Span, Statement},
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
    assert_parse!("foo", (Expr::var("foo"), span(0, 3)));
    assert_parse!("foo_bar", (Expr::var("foo_bar"), span(0, 7)));
}

#[test]
fn test_parse_num() {
    assert_parse!("123", (Expr::num(123.0), span(0, 3)));
    assert_parse!("123.456", (Expr::num(123.456), span(0, 7)));
}

#[test]
fn test_parse_bool() {
    assert_parse!("true", (Expr::bool(true), span(0, 4)));
    assert_parse!("false", (Expr::bool(false), span(0, 5)));
}

#[test]
fn test_op_app() {
    assert_parse!(
        "1 + 2",
        (
            Expr::op_app(
                ("+", span(2, 3)),
                (Expr::num(1.0), span(0, 1)),
                (Expr::num(2.0), span(4, 5))
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
            Expr::fn_app((Expr::var("f"), span(0, 1)), (Expr::num(1.0), span(2, 3))),
            span(0, 3)
        )
    );
}

#[test]
fn binding_strength() {
    assert_parse!(
        "f 1 + g 2",
        (
            Expr::op_app(
                ("+", span(4, 5)),
                (
                    Expr::fn_app((Expr::var("f"), span(0, 1)), (Expr::num(1.0), span(2, 3))),
                    span(0, 3)
                ),
                (
                    Expr::fn_app((Expr::var("g"), span(6, 7)), (Expr::num(2.0), span(8, 9))),
                    span(6, 9)
                )
            ),
            span(0, 9)
        )
    );
}

#[test]
fn test_fn_def() {
    assert_parse!(
        "fn add x y = x + y",
        (
            Expr::fn_def(
                ("add", span(3, 6)),
                vec![("x", span(7, 8)), ("y", span(9, 10))],
                (
                    Expr::op_app(
                        ("+", span(15, 16)),
                        (Expr::var("x"), span(13, 14)),
                        (Expr::var("y"), span(17, 18))
                    ),
                    span(13, 18)
                )
            ),
            span(0, 18)
        )
    );
}

#[test]
fn test_block() {
    assert_parse!("{}", (Expr::block(vec![]), span(0, 2)));

    assert_parse!(
        "{ 1; 2; 3 }",
        (
            Expr::block(vec![
                (Statement::expr((Expr::num(1.0), span(2, 3))), span(2, 3)),
                (Statement::expr((Expr::num(2.0), span(5, 6))), span(5, 6)),
                (Statement::expr((Expr::num(3.0), span(8, 9))), span(8, 9))
            ]),
            span(0, 11)
        )
    );

    assert_parse!(
        "fn add x y = { let sum = x + y; sum }",
        (
            Expr::fn_def(
                ("add", span(3, 6)),
                vec![("x", span(7, 8)), ("y", span(9, 10))],
                (
                    Expr::block(vec![
                        (
                            Statement::assignment(
                                ("sum", span(19, 22)),
                                (
                                    Expr::op_app(
                                        ("+", span(27, 28)),
                                        (Expr::var("x"), span(25, 26)),
                                        (Expr::var("y"), span(29, 30))
                                    ),
                                    span(25, 30)
                                )
                            ),
                            span(15, 30)
                        ),
                        (
                            Statement::expr((Expr::var("sum"), span(32, 35))),
                            span(32, 35)
                        )
                    ]),
                    span(13, 37)
                )
            ),
            span(0, 37)
        )
    );
}

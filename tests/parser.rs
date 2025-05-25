use chumsky::input::Stream;
use chumsky::prelude::*;
use lamina_lang::{
    lexer::{Span, lexer},
    parser::{expression, statement},
    syntax::{AstExpr, AstStmt},
};

#[macro_export]
macro_rules! assert_expr {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer().parse($src).unwrap();
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = expression().parse(tokens).unwrap();
        assert_eq!(ast, $expected);
    };
}

macro_rules! assert_stmt {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer().parse($src).unwrap();
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = statement(expression()).parse(tokens).unwrap();
        assert_eq!(ast, $expected);
    };
}

fn span(start: usize, end: usize) -> Span {
    (start..end).into()
}

#[test]
fn test_parse_var() {
    assert_expr!("foo", (AstExpr::ident("foo"), span(0, 3)));
    assert_expr!("foo_bar", (AstExpr::ident("foo_bar"), span(0, 7)));
}

#[test]
fn test_parse_num() {
    assert_expr!("123", (AstExpr::literal(123.0), span(0, 3)));
    assert_expr!("123.456", (AstExpr::literal(123.456), span(0, 7)));
}

#[test]
fn test_parse_bool() {
    assert_expr!("true", (AstExpr::literal(true), span(0, 4)));
    assert_expr!("false", (AstExpr::literal(false), span(0, 5)));
}

#[test]
fn test_parse_tuple() {
    // empty tuple
    assert_expr!("()", (AstExpr::literal(()), span(0, 2)));

    // single item tuple -> parsed as a parenthesized expression
    assert_expr!("(1)", (AstExpr::literal(1.0), span(1, 2)));

    // multiple item tuple
    assert_expr!(
        "(1, 2, 3)",
        (
            AstExpr::tuple(vec![
                (AstExpr::literal(1.0), span(1, 2)),
                (AstExpr::literal(2.0), span(4, 5)),
                (AstExpr::literal(3.0), span(7, 8))
            ]),
            span(0, 9)
        )
    );
}

#[test]
fn test_op_app() {
    assert_expr!(
        "1 + 2",
        (
            AstExpr::op_app(
                ("+", span(2, 3)),
                (AstExpr::literal(1.0), span(0, 1)),
                (AstExpr::literal(2.0), span(4, 5))
            ),
            span(0, 5)
        )
    );
}

#[test]
fn test_fn_app() {
    assert_expr!(
        "f 1",
        (
            AstExpr::fn_app(
                (AstExpr::ident("f"), span(0, 1)),
                (AstExpr::literal(1.0), span(2, 3))
            ),
            span(0, 3)
        )
    );
}

#[test]
fn binding_strength() {
    assert_expr!(
        "f 1 + g 2",
        (
            AstExpr::op_app(
                ("+", span(4, 5)),
                (
                    AstExpr::fn_app(
                        (AstExpr::ident("f"), span(0, 1)),
                        (AstExpr::literal(1.0), span(2, 3))
                    ),
                    span(0, 3)
                ),
                (
                    AstExpr::fn_app(
                        (AstExpr::ident("g"), span(6, 7)),
                        (AstExpr::literal(2.0), span(8, 9))
                    ),
                    span(6, 9)
                )
            ),
            span(0, 9)
        )
    );
}

#[test]
fn test_fn_def() {
    assert_stmt!(
        "fn add x y = x + y;",
        (
            AstStmt::fn_def(
                ("add", span(3, 6)),
                vec![("x", span(7, 8)), ("y", span(9, 10))],
                (
                    AstExpr::op_app(
                        ("+", span(15, 16)),
                        (AstExpr::ident("x"), span(13, 14)),
                        (AstExpr::ident("y"), span(17, 18))
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
    assert_expr!("{}", (AstExpr::block(vec![], None), span(1, 1)));
    assert_expr!(
        "{ 1; 2; 3 }",
        (
            AstExpr::block(
                vec![
                    (
                        AstStmt::expr((AstExpr::literal(1.0), span(2, 3))),
                        span(2, 3)
                    ),
                    (
                        AstStmt::expr((AstExpr::literal(2.0), span(5, 6))),
                        span(5, 6)
                    ),
                ],
                Some((AstExpr::literal(3.0), span(8, 9)))
            ),
            span(2, 9)
        )
    );

    assert_stmt!(
        r#"
          fn add x y = {
            let sum = x + y;
            sum
          };
        "#,
        (
            AstStmt::fn_def(
                ("add", span(14, 17)),
                vec![("x", span(18, 19)), ("y", span(20, 21))],
                (
                    AstExpr::block(
                        vec![(
                            AstStmt::let_def(
                                ("sum", span(42, 45)),
                                (
                                    AstExpr::op_app(
                                        ("+", span(50, 51)),
                                        (AstExpr::ident("x"), span(48, 49)),
                                        (AstExpr::ident("y"), span(52, 53))
                                    ),
                                    span(48, 53)
                                )
                            ),
                            span(38, 53)
                        )],
                        Some((AstExpr::ident("sum"), span(67, 70)))
                    ),
                    span(38, 70)
                )
            ),
            span(11, 82)
        )
    );
}

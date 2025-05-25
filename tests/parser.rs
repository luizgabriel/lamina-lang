use chumsky::input::Stream;
use chumsky::prelude::*;
use lamina_lang::{
    lexer::lexer,
    parser::{expression, statement},
    syntax::{Expr, Span, Stmt},
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
    assert_expr!("foo", (Expr::ident("foo"), span(0, 3)));
    assert_expr!("foo_bar", (Expr::ident("foo_bar"), span(0, 7)));
}

#[test]
fn test_parse_num() {
    assert_expr!("123", (Expr::literal(123.0), span(0, 3)));
    assert_expr!("123.456", (Expr::literal(123.456), span(0, 7)));
}

#[test]
fn test_parse_bool() {
    assert_expr!("true", (Expr::literal(true), span(0, 4)));
    assert_expr!("false", (Expr::literal(false), span(0, 5)));
}

#[test]
fn test_parse_tuple() {
    // empty tuple
    assert_expr!("()", (Expr::literal(()), span(0, 2)));

    // single item tuple -> parsed as a parenthesized expression
    assert_expr!("(1)", (Expr::literal(1.0), span(1, 2)));

    // multiple item tuple
    assert_expr!(
        "(1, 2, 3)",
        (
            Expr::tuple(vec![
                (Expr::literal(1.0), span(1, 2)),
                (Expr::literal(2.0), span(4, 5)),
                (Expr::literal(3.0), span(7, 8))
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
            Expr::op_app(
                ("+", span(2, 3)),
                (Expr::literal(1.0), span(0, 1)),
                (Expr::literal(2.0), span(4, 5))
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
            Expr::fn_app(
                (Expr::ident("f"), span(0, 1)),
                (Expr::literal(1.0), span(2, 3))
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
            Expr::op_app(
                ("+", span(4, 5)),
                (
                    Expr::fn_app(
                        (Expr::ident("f"), span(0, 1)),
                        (Expr::literal(1.0), span(2, 3))
                    ),
                    span(0, 3)
                ),
                (
                    Expr::fn_app(
                        (Expr::ident("g"), span(6, 7)),
                        (Expr::literal(2.0), span(8, 9))
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
            Stmt::fn_def(
                ("add", span(3, 6)),
                vec![("x", span(7, 8)), ("y", span(9, 10))],
                (
                    Expr::op_app(
                        ("+", span(15, 16)),
                        (Expr::ident("x"), span(13, 14)),
                        (Expr::ident("y"), span(17, 18))
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
    assert_expr!("{}", (Expr::block(vec![], None), span(1, 1)));
    assert_expr!(
        "{ 1; 2; 3 }",
        (
            Expr::block(
                vec![
                    (Stmt::expr((Expr::literal(1.0), span(2, 3))), span(2, 3)),
                    (Stmt::expr((Expr::literal(2.0), span(5, 6))), span(5, 6)),
                ],
                Some((Expr::literal(3.0), span(8, 9)))
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
            Stmt::fn_def(
                ("add", span(14, 17)),
                vec![("x", span(18, 19)), ("y", span(20, 21))],
                (
                    Expr::block(
                        vec![(
                            Stmt::let_def(
                                ("sum", span(42, 45)),
                                (
                                    Expr::op_app(
                                        ("+", span(50, 51)),
                                        (Expr::ident("x"), span(48, 49)),
                                        (Expr::ident("y"), span(52, 53))
                                    ),
                                    span(48, 53)
                                )
                            ),
                            span(38, 53)
                        )],
                        Some((Expr::ident("sum"), span(67, 70)))
                    ),
                    span(38, 70)
                )
            ),
            span(11, 82)
        )
    );
}

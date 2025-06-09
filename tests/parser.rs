use chumsky::input::Stream;
use chumsky::prelude::*;
use lamina_lang::{
    lexer::{lexer, Span},
    parser::{expression, statement, type_expr, AstExpr, AstStmt, AstType},
};

#[macro_export]
macro_rules! assert_expr {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer()
            .parse($src)
            .into_result()
            .expect("Failed to lex input");
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = expression()
            .parse(tokens)
            .into_result()
            .expect("Failed to parse expression");
        assert_eq!(ast, $expected);
    };
}

macro_rules! assert_stmt {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer()
            .parse($src)
            .into_result()
            .expect("Failed to lex input");
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = statement(expression())
            .parse(tokens)
            .into_result()
            .expect("Failed to parse statement");
        assert_eq!(ast, $expected);
    };
}

macro_rules! assert_type_expr {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer()
            .parse($src)
            .into_result()
            .expect("Failed to lex input");
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = type_expr()
            .parse(tokens)
            .into_result()
            .expect("Failed to parse type expression");
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
            AstExpr::tuple([
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
                ("+".into(), span(2, 3)),
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
                ("+".into(), span(4, 5)),
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
        "add x y = x + y;",
        (
            AstStmt::fn_def(
                ("add".into(), span(0, 3)),
                [("x".into(), span(4, 5)), ("y".into(), span(6, 7))],
                (
                    AstExpr::op_app(
                        ("+".into(), span(12, 13)),
                        (AstExpr::ident("x"), span(10, 11)),
                        (AstExpr::ident("y"), span(14, 15))
                    ),
                    span(10, 15)
                )
            ),
            span(0, 15)
        )
    );
}

#[test]
fn test_block() {
    assert_expr!("{}", (AstExpr::block([], None), span(1, 1)));
    assert_expr!(
        "{ 1; 2; 3 }",
        (
            AstExpr::block(
                [
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
          add x y = {
            sum = x + y;
            sum
          };
        "#,
        (
            AstStmt::fn_def(
                ("add".into(), span(11, 14)),
                [("x".into(), span(15, 16)), ("y".into(), span(17, 18))],
                (
                    AstExpr::block(
                        [(
                            AstStmt::assign(
                                ("sum".into(), span(35, 38)),
                                (
                                    AstExpr::op_app(
                                        ("+".into(), span(43, 44)),
                                        (AstExpr::ident("x"), span(41, 42)),
                                        (AstExpr::ident("y"), span(45, 46))
                                    ),
                                    span(41, 46)
                                )
                            ),
                            span(35, 46)
                        )],
                        Some((AstExpr::ident("sum"), span(60, 63)))
                    ),
                    span(35, 63)
                )
            ),
            span(11, 75)
        )
    );

    assert_stmt!(
        r#"id = x -> x;"#,
        (
            AstStmt::assign(
                ("id".into(), span(0, 2)),
                (
                    AstExpr::lambda(
                        ("x".into(), span(5, 6)),
                        (AstExpr::ident("x"), span(10, 11))
                    ),
                    span(5, 11)
                )
            ),
            span(0, 11)
        )
    );

    assert_expr!(
        r#"{
            id x = x
            id_lambda = x -> x
            id_lambda 42
        }"#,
        (
            AstExpr::block(
                [
                    (
                        AstStmt::fn_def(
                            ("id".into(), span(14, 16)),
                            [("x".into(), span(17, 18))],
                            (AstExpr::ident("x"), span(21, 22))
                        ),
                        span(14, 22)
                    ),
                    (
                        AstStmt::assign(
                            ("id_lambda".into(), span(35, 44)),
                            (
                                AstExpr::lambda(
                                    ("x".into(), span(47, 48)),
                                    (AstExpr::ident("x"), span(52, 53))
                                ),
                                span(47, 53)
                            )
                        ),
                        span(35, 53)
                    ),
                ],
                Some((
                    AstExpr::fn_app(
                        (AstExpr::ident("id_lambda"), span(66, 75)),
                        (AstExpr::literal(42.0), span(76, 78))
                    ),
                    span(66, 78)
                ))
            ),
            span(14, 78)
        )
    );

    assert_expr!(
        r#"{
            id x = x
            id_lambda = x -> x
            id_lambda 42;
        }"#,
        (
            AstExpr::block(
                [
                    (
                        AstStmt::fn_def(
                            ("id".into(), span(14, 16)),
                            [("x".into(), span(17, 18))],
                            (AstExpr::ident("x"), span(21, 22))
                        ),
                        span(14, 22)
                    ),
                    (
                        AstStmt::assign(
                            ("id_lambda".into(), span(35, 44)),
                            (
                                AstExpr::lambda(
                                    ("x".into(), span(47, 48)),
                                    (AstExpr::ident("x"), span(52, 53))
                                ),
                                span(47, 53)
                            )
                        ),
                        span(35, 53)
                    ),
                    (
                        AstStmt::expr((
                            AstExpr::fn_app(
                                (AstExpr::ident("id_lambda"), span(66, 75)),
                                (AstExpr::literal(42.0), span(76, 78))
                            ),
                            span(66, 78)
                        )),
                        span(66, 78)
                    )
                ],
                None
            ),
            span(14, 79)
        )
    );
}

#[test]
fn test_if_expr() {
    assert_expr!(
        "if true then 1 else 2",
        (
            AstExpr::if_expr(
                (AstExpr::literal(true), span(3, 7)),
                (AstExpr::literal(1.0), span(13, 14)),
                (AstExpr::literal(2.0), span(20, 21))
            ),
            span(0, 21)
        )
    );

    assert_expr!(
        "if x < 5 then x + 1 else x - 1",
        (
            AstExpr::if_expr(
                (
                    AstExpr::op_app(
                        ("<".into(), span(5, 6)),
                        (AstExpr::ident("x"), span(3, 4)),
                        (AstExpr::literal(5.0), span(7, 8))
                    ),
                    span(3, 8)
                ),
                (
                    AstExpr::op_app(
                        ("+".into(), span(16, 17)),
                        (AstExpr::ident("x"), span(14, 15)),
                        (AstExpr::literal(1.0), span(18, 19))
                    ),
                    span(14, 19)
                ),
                (
                    AstExpr::op_app(
                        ("-".into(), span(27, 28)),
                        (AstExpr::ident("x"), span(25, 26)),
                        (AstExpr::literal(1.0), span(29, 30))
                    ),
                    span(25, 30)
                )
            ),
            span(0, 30)
        )
    );
}

#[test]
fn test_lambda() {
    assert_expr!(
        "x -> 1",
        (
            AstExpr::lambda(
                ("x".into(), span(0, 1)),
                (AstExpr::literal(1.0), span(5, 6))
            ),
            span(0, 6)
        )
    );

    assert_stmt!(
        "add = x -> y -> x + y;",
        (
            AstStmt::assign(
                ("add".into(), span(0, 3)),
                (
                    AstExpr::lambda(
                        ("x".into(), span(6, 7)),
                        (
                            AstExpr::lambda(
                                ("y".into(), span(11, 12)),
                                (
                                    AstExpr::op_app(
                                        ("+".into(), span(18, 19)),
                                        (AstExpr::ident("x"), span(16, 17)),
                                        (AstExpr::ident("y"), span(20, 21))
                                    ),
                                    span(16, 21)
                                )
                            ),
                            span(11, 21)
                        )
                    ),
                    span(6, 21)
                )
            ),
            span(0, 21)
        )
    );
}

#[test]
fn test_type_expr() {
    assert_type_expr!("Num", (AstType::cons("Num"), span(0, 3)));
    assert_type_expr!("Bool", (AstType::cons("Bool"), span(0, 4)));
    assert_type_expr!(
        "(Num, Bool)",
        (
            AstType::tuple([
                (AstType::cons("Num"), span(1, 4)),
                (AstType::cons("Bool"), span(6, 10))
            ]),
            span(0, 11)
        )
    );
    assert_type_expr!(
        "Num -> Bool",
        (
            AstType::func(
                (AstType::cons("Num"), span(0, 3)),
                (AstType::cons("Bool"), span(7, 11))
            ),
            span(0, 11)
        )
    );
    assert_type_expr!(
        "(Num, Bool) -> Num",
        (
            AstType::func(
                (
                    AstType::tuple([
                        (AstType::cons("Num"), span(1, 4)),
                        (AstType::cons("Bool"), span(6, 10))
                    ]),
                    span(0, 11)
                ),
                (AstType::cons("Num"), span(15, 18))
            ),
            span(0, 18)
        )
    );

    assert_type_expr!(
        "Num -> Bool -> Num",
        (
            AstType::func(
                (AstType::cons("Num"), span(0, 3)),
                (
                    AstType::func(
                        (AstType::cons("Bool"), span(7, 11)),
                        (AstType::cons("Num"), span(15, 18))
                    ),
                    span(7, 18)
                )
            ),
            span(0, 18)
        )
    );
}

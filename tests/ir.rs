use chumsky::{
    Parser,
    input::{Input, Stream},
};
use lamina_lang::{
    ir::{IrExpr, lowering_expr},
    lexer::{Span, lexer},
    parser::expression,
};

macro_rules! assert_hir {
    ($src:expr, $expected:expr) => {
        let input = $src;
        let tokens = lexer().parse($src).unwrap();
        let tokens = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
        let ast = expression().parse(tokens).unwrap();
        assert_eq!(lowering_expr(ast), $expected);
    };
}

fn span(start: usize, end: usize) -> Span {
    (start..end).into()
}

#[test]
fn test_lowering_simple() {
    assert_hir!("foo", (IrExpr::ident("foo"), span(0, 3)));
    assert_hir!("1", (IrExpr::literal(1.0), span(0, 1)));
    assert_hir!("true", (IrExpr::literal(true), span(0, 4)));
    assert_hir!("false", (IrExpr::literal(false), span(0, 5)));
    assert_hir!("()", (IrExpr::literal(()), span(0, 2)));
    assert_hir!("(1)", (IrExpr::literal(1.0), span(1, 2)));
    assert_hir!(
        "(1, 2)",
        (
            IrExpr::tuple(vec![
                (IrExpr::literal(1.0), span(1, 2)),
                (IrExpr::literal(2.0), span(4, 5))
            ]),
            span(0, 6)
        )
    );
}

#[test]
fn test_fn_app() {
    assert_hir!(
        "foo 1",
        (
            IrExpr::fn_app(
                (IrExpr::ident("foo"), span(0, 3)),
                (IrExpr::literal(1.0), span(4, 5))
            ),
            span(0, 5)
        )
    );

    assert_hir!(
        "f g x",
        (
            IrExpr::fn_app(
                (
                    IrExpr::fn_app(
                        (IrExpr::ident("f"), span(0, 1)),
                        (IrExpr::ident("g"), span(2, 3))
                    ),
                    span(0, 3)
                ),
                (IrExpr::ident("x"), span(4, 5))
            ),
            span(0, 5)
        )
    );
}

#[test]
fn test_block() {
    assert_hir!("{ }", (IrExpr::literal(()), span(1, 1)));

    assert_hir!("{ 1 }", (IrExpr::literal(1.0), span(2, 3)));

    assert_hir!(
        "{ 1; 2 }",
        (
            IrExpr::let_binding(
                ("_", span(2, 3)),
                (IrExpr::literal(1.0), span(2, 3)),
                (IrExpr::literal(2.0), span(5, 6)),
            ),
            span(2, 3)
        )
    );
}

#[test]
fn test_op_app() {
    assert_hir!(
        "1 + 2",
        (
            IrExpr::fn_app(
                (
                    IrExpr::fn_app(
                        (IrExpr::ident("+"), span(2, 3)),
                        (IrExpr::literal(1.0), span(0, 1))
                    ),
                    span(0, 3)
                ),
                (IrExpr::literal(2.0), span(4, 5))
            ),
            span(0, 5)
        )
    );
}

#[test]
fn test_if_lowering() {
    assert_hir!(
        "if true then 1 else 2",
        (
            IrExpr::if_expr(
                (IrExpr::literal(true), span(3, 7)),
                (IrExpr::literal(1.0), span(13, 14)),
                (IrExpr::literal(2.0), span(20, 21))
            ),
            span(0, 21)
        )
    );

    assert_hir!(
        "if x < 5 then x else 0",
        (
            IrExpr::if_expr(
                (
                    IrExpr::fn_app(
                        (
                            IrExpr::fn_app(
                                (IrExpr::ident("<"), span(5, 6)),
                                (IrExpr::ident("x"), span(3, 4))
                            ),
                            span(3, 6)
                        ),
                        (IrExpr::literal(5.0), span(7, 8))
                    ),
                    span(3, 8)
                ),
                (IrExpr::ident("x"), span(14, 15)),
                (IrExpr::literal(0.0), span(21, 22))
            ),
            span(0, 22)
        )
    );
}

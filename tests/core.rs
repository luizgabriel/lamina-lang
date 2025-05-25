use chumsky::{
    Parser,
    input::{Input, Stream},
};
use lamina_lang::{
    core::{CoreLang, lowering_expr},
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
    assert_hir!("foo", (CoreLang::ident("foo"), span(0, 3)));
    assert_hir!("1", (CoreLang::literal(1.0), span(0, 1)));
    assert_hir!("true", (CoreLang::literal(true), span(0, 4)));
    assert_hir!("false", (CoreLang::literal(false), span(0, 5)));
    assert_hir!("()", (CoreLang::literal(()), span(0, 2)));
    assert_hir!("(1)", (CoreLang::literal(1.0), span(1, 2)));
    assert_hir!(
        "(1, 2)",
        (
            CoreLang::tuple(vec![
                (CoreLang::literal(1.0), span(1, 2)),
                (CoreLang::literal(2.0), span(4, 5))
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
            CoreLang::fn_app(
                (CoreLang::ident("foo"), span(0, 3)),
                (CoreLang::literal(1.0), span(4, 5))
            ),
            span(0, 5)
        )
    );

    assert_hir!(
        "f g x",
        (
            CoreLang::fn_app(
                (
                    CoreLang::fn_app(
                        (CoreLang::ident("f"), span(0, 1)),
                        (CoreLang::ident("g"), span(2, 3))
                    ),
                    span(0, 3)
                ),
                (CoreLang::ident("x"), span(4, 5))
            ),
            span(0, 5)
        )
    );
}

#[test]
fn test_block() {
    assert_hir!("{ }", (CoreLang::literal(()), span(1, 1)));

    assert_hir!("{ 1 }", (CoreLang::literal(1.0), span(2, 3)));

    assert_hir!(
        "{ 1; 2 }",
        (
            CoreLang::let_binding(
                ("_", span(2, 3)),
                (CoreLang::literal(1.0), span(2, 3)),
                (CoreLang::literal(2.0), span(5, 6)),
            ),
            span(2, 3)
        )
    );
}

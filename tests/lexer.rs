use chumsky::Parser;
use k9::snapshot;
use lamina_lang::lexer::{lexer, Span, Token};

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
    let src = "let if true false";
    assert_lex!(
        src,
        [
            (Token::Ident("let"), span(0, 3)),
            (Token::If, span(4, 6)),
            (Token::True, span(7, 11)),
            (Token::False, span(12, 17)),
        ]
    );
}

#[test]
fn test_if_keywords() {
    let src = "if then else";
    assert_lex!(
        src,
        [
            (Token::If, span(0, 2)),
            (Token::Then, span(3, 7)),
            (Token::Else, span(8, 12)),
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
            (Token::Equal, span(0, 1)),
            (Token::Op("+"), span(2, 3)),
            (Token::Op("*"), span(4, 5)),
        ]
    );
}

#[test]
fn test_parens() {
    let src = "(foo)";
    assert_lex!(
        src,
        [
            (Token::OpenCtrl('('), span(0, 1)),
            (Token::Ident("foo"), span(1, 4)),
            (Token::CloseCtrl(')'), span(4, 5)),
        ]
    );
}

#[test]
fn test_error_output() {
    let src = "let @ foo";
    let result = lexer().parse(src).into_result();
    snapshot!(
        result.unwrap_err().first().unwrap(),
        r#"found ''@'' at 4..5 expected '' '', ''\t'', comment, number, semicolon, comma, colon, newline, open control character, close control character, operator, or keyword/identifier"#
    );
}

#[test]
fn test_custom_operators() {
    let src = ">>> && || |> <=> !=";
    assert_lex!(
        src,
        [
            (Token::Op(">>>"), span(0, 3)),
            (Token::Op("&&"), span(4, 6)),
            (Token::Op("||"), span(7, 9)),
            (Token::Op("|>"), span(10, 12)),
            (Token::Op("<=>"), span(13, 16)),
            (Token::Op("!="), span(17, 19)),
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
            (Token::VirtualSemi, span(9, 10)), // Virtual semicolon inserted after newline
            (Token::Ident("baz"), span(11, 14)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_after_identifier() {
    let src = "foo\nbar";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            (Token::VirtualSemi, span(3, 4)), // Virtual semicolon after newline
            (Token::Ident("bar"), span(4, 7)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_after_number() {
    let src = "42\n2.5";
    assert_lex!(
        src,
        [
            (Token::Num(42.0), span(0, 2)),
            (Token::VirtualSemi, span(2, 3)), // Virtual semicolon after newline
            (Token::Num(2.5), span(3, 6)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_after_boolean() {
    let src = "true\nfalse";
    assert_lex!(
        src,
        [
            (Token::True, span(0, 4)),
            (Token::VirtualSemi, span(4, 5)), // Virtual semicolon after newline
            (Token::False, span(5, 10)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_after_closing_paren() {
    let src = "(foo)\nbar";
    assert_lex!(
        src,
        [
            (Token::OpenCtrl('('), span(0, 1)),
            (Token::Ident("foo"), span(1, 4)),
            (Token::CloseCtrl(')'), span(4, 5)),
            (Token::VirtualSemi, span(5, 6)), // Virtual semicolon after closing paren
            (Token::Ident("bar"), span(6, 9)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_before_then() {
    let src = "true\nthen";
    assert_lex!(
        src,
        [
            (Token::True, span(0, 4)),
            // No virtual semicolon because 'then' prevents insertion
            (Token::Then, span(5, 9)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_before_else() {
    let src = "42\nelse";
    assert_lex!(
        src,
        [
            (Token::Num(42.0), span(0, 2)),
            // No virtual semicolon because 'else' prevents insertion
            (Token::Else, span(3, 7)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_before_operator() {
    let src = "foo\n+ bar";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            // No virtual semicolon because '+' prevents insertion
            (Token::Op("+"), span(4, 5)),
            (Token::Ident("bar"), span(6, 9)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_before_comma() {
    let src = "foo\n, bar";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            // No virtual semicolon because ',' prevents insertion
            (Token::Comma, span(4, 5)),
            (Token::Ident("bar"), span(6, 9)),
        ]
    );
}

#[test]
fn test_mixed_explicit_and_virtual_semicolons() {
    let src = "foo;\nbar\nbaz;";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            (Token::Semi, span(3, 4)), // Explicit semicolon
            (Token::Ident("bar"), span(5, 8)),
            (Token::VirtualSemi, span(8, 9)), // Virtual semicolon after newline
            (Token::Ident("baz"), span(9, 12)),
            (Token::Semi, span(12, 13)), // Explicit semicolon
        ]
    );
}

#[test]
fn test_virtual_semicolon_multiple_newlines() {
    let src = "foo\n\n\nbar";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            (Token::VirtualSemi, span(3, 6)), // Virtual semicolon for multiple newlines
            (Token::Ident("bar"), span(6, 9)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_at_start() {
    let src = "\nfoo";
    assert_lex!(
        src,
        [
            // No virtual semicolon at start (no previous token)
            (Token::Ident("foo"), span(1, 4)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_before_closing_brace() {
    let src = "foo\n}";
    assert_lex!(
        src,
        [
            (Token::Ident("foo"), span(0, 3)),
            // No virtual semicolon because '}' prevents insertion
            (Token::CloseCtrl('}'), span(4, 5)),
        ]
    );
}

#[test]
fn test_virtual_semicolon_complex_scenario() {
    let src = "let x = 42\nif x > 0\n    then x\n    else 0\ny = x + 1";
    assert_lex!(
        src,
        [
            (Token::Ident("let"), span(0, 3)),
            (Token::Ident("x"), span(4, 5)),
            (Token::Equal, span(6, 7)),
            (Token::Num(42.0), span(8, 10)),
            (Token::VirtualSemi, span(10, 11)), // Virtual semicolon after 42
            (Token::If, span(11, 13)),
            (Token::Ident("x"), span(14, 15)),
            (Token::Op(">"), span(16, 17)),
            (Token::Num(0.0), span(18, 19)),
            // No virtual semicolon before 'then'
            (Token::Then, span(24, 28)),
            (Token::Ident("x"), span(29, 30)),
            // No virtual semicolon before 'else'
            (Token::Else, span(35, 39)),
            (Token::Num(0.0), span(40, 41)),
            (Token::VirtualSemi, span(41, 42)), // Virtual semicolon after 0
            (Token::Ident("y"), span(42, 43)),
            (Token::Equal, span(44, 45)),
            (Token::Ident("x"), span(46, 47)),
            (Token::Op("+"), span(48, 49)),
            (Token::Num(1.0), span(50, 51)),
        ]
    );
}

#[test]
fn test_no_virtual_semicolon_in_incomplete_if() {
    // Test that virtual semicolons are not inserted inside incomplete if expressions
    let src = "if x > 1 then\n42";
    assert_lex!(
        src,
        [
            (Token::If, span(0, 2)),
            (Token::Ident("x"), span(3, 4)),
            (Token::Op(">"), span(5, 6)),
            (Token::Num(1.0), span(7, 8)),
            (Token::Then, span(9, 13)),
            // No virtual semicolon after 'then' and newline, even though 42 can end statement
            (Token::Num(42.0), span(14, 16)),
        ]
    );
}

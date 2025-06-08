use lamina_lang::{
    parser::{parse_expr, parse_type, AstType},
    typecheck::{infer, TyEnvironment, TypeError},
};

fn parse_and_infer(input: &str) -> Result<AstType, TypeError> {
    let expr = parse_expr(input).expect("Failed to parse input");
    infer(&expr.0, &TyEnvironment::builtins())
}

macro_rules! assert_infer {
    ($input:expr, $expected:expr) => {
        assert_eq!(
            parse_and_infer($input).unwrap(),
            parse_type($expected).unwrap().0
        );
    };
}

#[test]
fn test_infer_literal() {
    assert_infer!("42", "num");
    assert_infer!("true", "bool");
    assert_infer!("()", "unit");
}

#[test]
fn test_infer_tuple() {
    assert_infer!("(42, true)", "(num, bool)");
    assert_infer!("(42, true, ())", "(num, bool, unit)");
}

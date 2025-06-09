use lamina_lang::{
    parser::parse_expr,
    typecheck::{infer, Type, TypeEnvironment, TypeError, TypeVarContext},
};

fn parse_and_infer(input: &str) -> Result<Type, TypeError> {
    let expr = parse_expr(input).expect("Failed to parse input");
    let mut ctx = TypeVarContext::default();
    let env = TypeEnvironment::builtins(&mut ctx);
    let (ty, _) = infer(&expr.0, &env, &mut ctx)?;

    Ok(ty)
}

macro_rules! assert_infer {
    ($input:expr, $expected:expr) => {
        assert_eq!(parse_and_infer($input).unwrap(), $expected);
    };
}

#[test]
fn test_infer_literal() {
    assert_infer!("42", Type::Num);
    assert_infer!("true", Type::Bool);
    assert_infer!("()", Type::Unit);
}

#[test]
fn test_infer_tuple() {
    assert_infer!("(42, true)", Type::tuple([Type::Num, Type::Bool]));
    assert_infer!(
        "(42, true, ())",
        Type::tuple([Type::Num, Type::Bool, Type::Unit])
    );
}

#[test]
fn test_infer_lambda() {
    assert_infer!("x -> x", Type::func(Type::var(2), Type::var(2)));
    assert_infer!("x -> if x then 1 else 2", Type::func(Type::Bool, Type::Num));
}

#[test]
fn test_infer_fn_app() {
    assert_infer!("(x -> x) 42", Type::Num);
    assert_infer!("(x -> x) true", Type::Bool);
}

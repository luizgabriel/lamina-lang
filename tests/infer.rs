use k9::assert_err;
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

#[test]
fn test_infer_if() {
    assert_infer!("if true then 1 else 2", Type::Num);
    assert_infer!("if true then true else false", Type::Bool);
}

#[test]
fn test_infer_fn_def() {
    assert_infer!(
        r#"{
            id x = x
            id_lambda = x -> x
            id_lambda
        }"#,
        Type::func(Type::var(3), Type::var(3))
    );
}

#[test]
fn test_infer_block() {
    assert_infer!(
        r#"{
            id x = x
            k = f -> if true then f 42 else f 43
            k id
        }"#,
        Type::Num
    );

    assert_infer!(
        r#"{
            id x = x
            k = f -> if true then f 42 else f 43
            k id;
        }"#,
        Type::Unit
    );
}

#[test]
fn test_constraint_propagation_in_conditional() {
    assert_infer!(
        "x -> if x == 1 then x + 1 else x",
        Type::func(Type::Num, Type::Num)
    );
}

#[test]
fn test_function_application_with_operator_constraint() {
    let result = parse_and_infer("f -> x -> f (x + 1)").unwrap();

    assert!(
        matches!(
            result,
            Type::Fn(
                ref f_type,
                ref inner_fn
            ) if matches!(
                (f_type.as_ref(), inner_fn.as_ref()),
                (
                    Type::Fn(ref f_arg, _),
                    Type::Fn(ref x_type, _)
                ) if **f_arg == Type::Num && **x_type == Type::Num
            )
        ),
        "Expected (Num -> a) -> Num -> a pattern, got: {:?}",
        result
    );
}

#[test]
fn test_type_error_with_constraint_propagation() {
    let result = parse_and_infer("(x -> x + 1) true");
    assert_err!(result, "Expected type error but got: {:?}");
}

#[test]
fn test_nested_application_constraint_flow() {
    assert_infer!("(f -> f 1) (x -> x + 1)", Type::Num);
}

#[test]
fn test_bidirectional_constraint_propagation() {
    assert_infer!(
        "x -> y -> if x == y then x + 1 else y - 1",
        Type::func(Type::Num, Type::func(Type::Num, Type::Num))
    );
}

#[test]
fn test_complex_operator_constraint_chain() {
    assert_infer!(
        "a -> b -> c -> if a == b then b + c else a - c",
        Type::func(
            Type::Num,
            Type::func(Type::Num, Type::func(Type::Num, Type::Num))
        )
    );
}

#[test]
fn test_polymorphic_function_structure() {
    // Test that polymorphic functions have the right structure using exact type checks
    assert_infer!("x -> x", Type::func(Type::var(2), Type::var(2)));
    assert_infer!(
        "f -> x -> f x",
        Type::func(
            Type::func(Type::var(3), Type::var(4)),
            Type::func(Type::var(3), Type::var(4))
        )
    );
}

#[test]
fn test_function_type_patterns() {
    // Test various function type patterns
    assert_infer!("x -> if x then 1 else 2", Type::func(Type::Bool, Type::Num));
    // Test that lambda returns a function
    assert_infer!(
        "x -> y -> x",
        Type::func(Type::var(2), Type::func(Type::var(3), Type::var(2)))
    );
}

#[test]
fn test_error_patterns() {
    let result = parse_and_infer("x + true");
    assert_err!(result, "Expected error for 'x + true'");

    let result = parse_and_infer("if 42 then 1 else 2");
    assert_err!(result, "Expected error for 'if 42 then 1 else 2'");
}

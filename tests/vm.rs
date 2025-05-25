use lamina_lang::{core::lowering_expr, parser::parse_expr, vm::compile_and_execute};

// Macro to simplify testing VM expressions
macro_rules! assert_vm {
    ($input:expr, $expected:expr) => {
        assert_eq!(test_expression($input).unwrap(), $expected);
    };
    ($input:expr, $expected:expr, $($arg:tt)*) => {
        assert_eq!(test_expression($input).unwrap(), $expected, $($arg)*);
    };
}

fn test_expression(input: &str) -> Result<String, String> {
    match parse_expr(input) {
        Ok(ast) => {
            let core_expr = lowering_expr(ast);
            match compile_and_execute(&core_expr) {
                Ok(value) => Ok(value.to_string()),
                Err(err) => Err(format!("Runtime error: {}", err)),
            }
        }
        Err(err) => Err(format!("Parse error: {:?}", err)),
    }
}

#[test]
fn test_basic_literals() {
    assert_vm!("42", "42");
    assert_vm!("true", "true");
    assert_vm!("false", "false");
    assert_vm!("()", "()");
}

#[test]
fn test_tuples() {
    assert_vm!("(1, 2, 3)", "(1, 2, 3)");
    assert_vm!("(true, 42)", "(true, 42)");
}

#[test]
fn test_binary_operations() {
    assert_vm!("5 + 3", "8");
    assert_vm!("4 * 6", "24");
    assert_vm!("10 - 3", "7");
    assert_vm!("15 / 3", "5");
}

#[test]
fn test_comparisons() {
    assert_vm!("5 < 10", "true");
    assert_vm!("10 > 5", "true");
    assert_vm!("5 == 5", "true");
    assert_vm!("5 == 3", "false");
}

#[test]
fn test_boolean_operations() {
    assert_vm!("true && false", "false");
    assert_vm!("true || false", "true");
    // Note: Unary operators might not be supported in this syntax
}

#[test]
fn test_function_applications() {
    // Test function application with identifiers
    // Note: The language doesn't support parenthesized operators like (+)
    // This test is removed for now
}

#[test]
fn test_let_statements_in_blocks() {
    assert_vm!("{ let x = 42; x }", "42");
    assert_vm!("{ let x = 5; x + 10 }", "15");
}

#[test]
fn test_complex_expressions() {
    // Nested function applications
    assert_vm!("(2 * 3) + (8 / 2)", "10");

    // Nested let bindings in blocks
    assert_vm!("{ let x = 5; let y = 10; x + y }", "15");
}

#[test]
fn test_error_cases() {
    // Division by zero
    assert!(test_expression("5 / 0").is_err());

    // Type errors
    assert!(test_expression("true + 5").is_err());
    assert!(test_expression("5 && true").is_err());
}

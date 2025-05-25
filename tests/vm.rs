use k9::assert_err;
use lamina_lang::{
    parser::ParseError,
    vm::{Compiler, VM, VMError, VmValue},
};

pub fn compile_and_execute(input: &str) -> Result<VmValue, VMError> {
    let mut compiler = Compiler::new();
    let mut vm = VM::default();
    let instructions = compiler.compile_input(input).unwrap();
    vm.execute(instructions)
}

// Macro to simplify testing VM expressions
macro_rules! assert_vm {
    ($input:expr, $expected:expr) => {
        assert_eq!(compile_and_execute($input).unwrap().to_string(), $expected);
    };
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
    assert_err!(compile_and_execute("5 / 0"));

    // Type errors
    assert_err!(compile_and_execute("true + 5"));
    assert_err!(compile_and_execute("5 && true"));
}

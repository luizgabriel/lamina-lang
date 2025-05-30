use k9::assert_err;
use lamina_lang::vm::{Compiler, VM, VMError, VmEnv, VmValue};

pub fn compile_and_execute(input: &str) -> Result<VmValue, VMError> {
    let mut compiler = Compiler::new();
    let mut vm = VM::new(32, VmEnv::builtins());
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
fn test_partial_application() {
    assert_vm!("{ let add1 = (+) 1; add1 2 }", "3");
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

#[test]
fn test_if_expressions() {
    // Simple if expressions
    assert_vm!("if true then 1 else 2", "1");
    assert_vm!("if false then 1 else 2", "2");

    // If with comparisons
    assert_vm!("if 5 > 3 then 42 else 0", "42");
    assert_vm!("if 2 > 5 then 42 else 0", "0");

    // Nested if expressions
    assert_vm!("if true then (if false then 1 else 2) else 3", "2");

    // If with variables in blocks
    assert_vm!("{ let x = 5; if x > 3 then x + 1 else x - 1 }", "6");
    assert_vm!("{ let x = 2; if x > 3 then x + 1 else x - 1 }", "1");
}

#[test]
fn test_recursive_functions() {
    // Simple recursive factorial function
    assert_vm!(
        "{ fn fact n = if n == 0 then 1 else n * (fact (n - 1)); fact 5 }",
        "120"
    );

    // Recursive fibonacci function
    assert_vm!(
        "{ 
            fn fib n = 
                if n < 2 
                    then n
                    else (fib (n - 1)) + (fib (n - 2));
            fib 6
        }",
        "8"
    );

    // Recursive countdown function
    assert_vm!(
        "{ fn countdown n = if n == 0 then 0 else countdown (n - 1); countdown 5 }",
        "0"
    );
}

#[test]
fn test_stack_overflow() {
    let mut compiler = Compiler::new();
    // Recursive function that never terminates
    let code = "{ fn loop_forever n = loop_forever n; loop_forever 0 }";
    let instructions = compiler.compile_input(code).unwrap();
    // Set a low max_call_stack_depth to trigger overflow quickly
    let mut vm = VM::new(16, VmEnv::builtins());
    let result = vm.execute(instructions);
    assert!(matches!(result, Err(VMError::StackOverflow)));
}

#[test]
fn test_function_scope_no_leak() {
    // Variable defined in function should not be accessible outside
    let result = compile_and_execute("{ fn foo n = { let y = n + 1; y }; foo(1); y }");
    assert!(
        matches!(result, Err(VMError::UnboundVariable { name: _ })),
        "Variable 'y' should not be accessible outside the function"
    );
}

#[test]
#[ignore]
fn test_block_scope_no_leak() {
    // Variable defined in block should not be accessible outside
    let result = compile_and_execute("{ { let x = 42; x }; x }");
    assert!(
        matches!(result, Err(VMError::UnboundVariable { name: _ })),
        "Variable 'x' should not be accessible outside the block"
    );
}

#[test]
#[ignore]
fn test_shadowing_no_leak() {
    // Shadowing should not leak inner value to outer scope
    let result = compile_and_execute("{ let x = 1; { let x = 2; x }; x }");
    assert_eq!(result.unwrap().to_string(), "1");
}

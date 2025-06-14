use k9::assert_err;
use lamina_lang::{
    interpreter::{eval, InterpreterError, Value, ValueEnv},
    parser::parse_expr,
};

pub fn parse_and_evaluate(input: &str) -> Result<Value, InterpreterError> {
    let expr = parse_expr(input).expect("Failed to parse input");
    eval(expr.0, &ValueEnv::builtins())
}

// Macro to simplify testing interpreter expressions
macro_rules! assert_eval {
    ($input:expr, $expected:expr) => {
        assert_eq!(parse_and_evaluate($input).unwrap().to_string(), $expected);
    };
}

#[test]
fn test_basic_literals() {
    assert_eval!("42", "42");
    assert_eval!("true", "true");
    assert_eval!("false", "false");
    assert_eval!("()", "()");
}

#[test]
fn test_tuples() {
    assert_eval!("(1, 2, 3)", "(1, 2, 3)");
    assert_eval!("(true, 42)", "(true, 42)");
}

#[test]
fn test_binary_operations() {
    assert_eval!("5 + 3", "8");
    assert_eval!("4 * 6", "24");
    assert_eval!("10 - 3", "7");
    assert_eval!("15 / 3", "5");
    // Division by zero
    assert_err!(parse_and_evaluate("5 / 0"));
}

#[test]
fn test_comparisons() {
    assert_eval!("5 < 10", "true");
    assert_eval!("10 > 5", "true");
    assert_eval!("5 == 5", "true");
    assert_eval!("5 == 3", "false");
}

#[test]
fn test_boolean_operations() {
    assert_eval!("true && false", "false");
    assert_eval!("true || false", "true");
}

#[test]
fn test_partial_application() {
    assert_eval!("{ inc = (+) 1; inc 2 }", "3");
    assert_eval!("{ dec x = (-) x 1; dec 2 }", "1");
    assert_eval!("{ add = (+); add 2 3 }", "5");
    assert_eval!("{ double = (*) 2; double 3 }", "6");
    assert_eval!("{ half x = x / 2; half 4 }", "2");
}

#[test]
fn test_let_statements_in_blocks() {
    assert_eval!("{ x = 42; x }", "42");
    assert_eval!("{ x = 5; x + 10 }", "15");
}

#[test]
fn test_complex_expressions() {
    // Nested function applications
    assert_eval!("(2 * 3) + (8 / 2)", "10");

    // Nested bindings in blocks
    assert_eval!("{ x = 5; y = 10; x + y }", "15");
}

#[test]
fn test_error_cases() {
    // Division by zero
    assert_err!(parse_and_evaluate("5 / 0"));

    // Type errors
    assert_err!(parse_and_evaluate("true + 5"));
    assert_err!(parse_and_evaluate("5 && true"));
}

#[test]
fn test_if_expressions() {
    // Simple if expressions
    assert_eval!("if true then 1 else 2", "1");
    assert_eval!("if false then 1 else 2", "2");

    // If with comparisons
    assert_eval!("if 5 > 3 then 42 else 0", "42");
    assert_eval!("if 2 > 5 then 42 else 0", "0");

    // Nested if expressions
    assert_eval!("if true then (if false then 1 else 2) else 3", "2");

    // If with variables in blocks
    assert_eval!("{ x = 5; if x > 3 then x + 1 else x - 1 }", "6");
    assert_eval!("{ x = 2; if x > 3 then x + 1 else x - 1 }", "1");
}

#[test]
fn test_recursive_functions() {
    // Simple recursive factorial function
    assert_eval!(
        "{ fact n = if n == 0 then 1 else n * (fact (n - 1)); fact 5 }",
        "120"
    );

    // Recursive fibonacci function
    assert_eval!(
        "{
            fib n =
                if n < 2
                    then n
                    else (fib (n - 1)) + (fib (n - 2));
            fib 6
        }",
        "8"
    );

    // Recursive countdown function
    assert_eval!(
        "{ countdown n = if n == 0 then 0 else countdown (n - 1); countdown 5 }",
        "0"
    );
}

#[test]
fn test_function_scope_no_leak() {
    // Variable defined in function should not be accessible outside
    let result = parse_and_evaluate("{ foo n = { y = n + 1; y }; foo(1); y }");
    assert!(
        matches!(result, Err(InterpreterError::UnboundVariable(name)) if name == "y"),
        "Variable 'y' should not be accessible outside the function"
    );
}

#[test]
fn test_block_scope_no_leak() {
    // Variable defined in block should not be accessible outside
    let result = parse_and_evaluate("{ { x = 42; x }; x }");
    assert!(
        matches!(result, Err(InterpreterError::UnboundVariable(_))),
        "Variable 'x' should not be accessible outside the block"
    );
}

#[test]
fn test_shadowing_no_leak() {
    // Shadowing should not leak inner value to outer scope
    let result = parse_and_evaluate("{ x = 1; { x = 2; x }; x }");
    assert_eq!(result.unwrap().to_string(), "1");
}

#[test]
fn test_lambda_expressions() {
    // Simple lambda application
    assert_eval!("(x -> x + 1) 5", "6");

    // Lambda with multiplication
    assert_eval!("(x -> x * 2) 10", "20");

    // Nested lambdas
    assert_eval!("(x -> (y -> x + y) 3) 5", "8");
}

#[test]
fn test_let_bindings() {
    // Simple binding
    assert_eval!("{ x = 42; x }", "42");

    // binding with computation
    assert_eval!("{ x = 5 + 3; x * 2 }", "16");

    // Nested bindings
    assert_eval!("{ x = 5; y = 10; x + y }", "15");
}

#[test]
fn test_closure_capture() {
    // Test that closures capture their environment correctly
    assert_eval!("{ x = 10; (y -> x + y) 5 }", "15");

    // Test nested closure capture
    assert_eval!("{ x = 5; f = (y -> x + y); f 3 }", "8");
}

#[test]
fn test_higher_order_functions() {
    // Function that takes a function as argument
    assert_eval!("{ apply f x = f x; apply (y -> y * 2) 5 }", "10");

    // Function that returns a function
    assert_eval!(
        "{ make_adder n = (x -> x + n); add5 = make_adder 5; add5 3 }",
        "8"
    );
}

#[test]
fn test_built_in_functions() {
    // Test all binary operators
    assert_eval!("(+) 2 3", "5");
    assert_eval!("(-) 10 4", "6");
    assert_eval!("(*) 6 7", "42");
    assert_eval!("(/) 20 4", "5");
    assert_eval!("(==) 5 5", "true");
    assert_eval!("(<) 3 7", "true");
    assert_eval!("(>) 8 2", "true");
    assert_eval!("(&&) true false", "false");
    assert_eval!("(||) true false", "true");

    // Test unary operator
    assert_eval!("(!) true", "false");
    assert_eval!("(!) false", "true");
}

#[test]
fn test_currying() {
    // Test that multi-argument functions are curried
    assert_eval!("{ add x y = x + y; add5 = add 5; add5 3 }", "8");

    // Test three-argument function currying
    assert_eval!("{ add3 x y z = x + y + z; add3 1 2 3 }", "6");
}

#[test]
fn test_error_propagation() {
    // Test that errors are properly propagated through nested expressions
    assert_err!(parse_and_evaluate("{ f = (x -> x / 0); f 5 }"));

    // Test type error propagation
    assert_err!(parse_and_evaluate("{ f = (x -> x + true); f 5 }"));
}

#[test]
fn test_tail_recursion() {
    // Test tail recursive function (should not stack overflow)
    assert_eval!(
        "{
            sum_tail n acc =
                if n == 0
                    then acc
                    else sum_tail (n - 1) (acc + n);
            sum_tail 100 0
        }",
        "5050"
    );
}

// Tests for virtual semicolons feature
#[test]
fn test_virtual_semicolons_basic() {
    // Basic virtual semicolons - no explicit semicolons needed
    assert_eval!(
        "{
            x = 42
            y = x + 1
            y * 2
        }",
        "86"
    );
}

#[test]
fn test_virtual_semicolons_mixed_with_explicit() {
    // Mix of explicit and virtual semicolons
    assert_eval!(
        "{
            x = 10;
            y = 20
            z = x + y;
            z * 2
        }",
        "60"
    );
}

#[test]
fn test_virtual_semicolons_multiline_expressions() {
    // Multiline expressions should work without virtual semicolons interfering
    assert_eval!(
        "{
            result = if true
                then 42
                else 0
            result + 8
        }",
        "50"
    );
}

#[test]
fn test_virtual_semicolons_function_definitions() {
    // Function definitions with virtual semicolons
    assert_eval!(
        "{
            add x y = x + y
            multiply x y = x * y
            a = add 5 3
            b = multiply 2 4
            a + b
        }",
        "16"
    );
}

#[test]
fn test_virtual_semicolons_nested_blocks() {
    // Nested blocks with virtual semicolons
    assert_eval!(
        "{
            outer = 10
            inner_result = {
                inner = 5
                doubled = inner * 2
                doubled + 1
            }
            outer + inner_result
        }",
        "21"
    );
}

#[test]
fn test_virtual_semicolons_complex_expressions() {
    // Complex expressions spanning multiple lines
    assert_eval!(
        "{
            x = 5
            y = if x > 3
                then x * 2
                else x
            z = (y -> y + 1) y
            z
        }",
        "11"
    );
}

#[test]
fn test_virtual_semicolons_tuple_operations() {
    // Tuples with virtual semicolons
    assert_eval!(
        "{
            coords = (3, 4)
            x = 1
            y = 2
            (x, y)
        }",
        "(1, 2)"
    );
}

#[test]
fn test_virtual_semicolons_with_operators() {
    // Test that operators properly continue expressions across lines
    assert_eval!(
        "{
            a = 5
            result = a
                + 10
                * 2
            result
        }",
        "30"
    );
}

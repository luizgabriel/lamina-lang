use crate::parser::{AstExpr, AstModule, AstStmt};
use std::cell::RefCell;
use std::rc::Rc;

use super::{Environment, InterpreterError, Value};

pub fn eval_module(module: &AstModule, env: &Environment) -> Result<Environment, InterpreterError> {
    module
        .items
        .iter()
        .try_fold(env.clone(), |env, stmt| eval_stmt(&stmt.0, &env))
}

pub fn eval_stmt(stmt: &AstStmt, env: &Environment) -> Result<Environment, InterpreterError> {
    match stmt {
        AstStmt::Expr(expr) => {
            let _ = eval(&expr.0, env)?;
            Ok(env.clone())
        }
        AstStmt::Let { name, body } => {
            let value = eval(&body.0, env)?;
            Ok(env.extend(name.0.clone(), value))
        }
        AstStmt::FnDef { name, args, body } => {
            debug_assert!(
                !args.is_empty(),
                "Function definition must have at least one argument"
            );

            let span = body.1;

            // Build nested lambdas for currying (skip first arg, it becomes the closure's parameter)
            let lambda_body = args
                .iter()
                .skip(1)
                .cloned()
                .rfold(body.clone(), |body, arg| (AstExpr::lambda(arg, body), span));

            let env = Rc::new(RefCell::new(env.clone()));
            let closure = Value::Closure {
                arg_name: args[0].clone(),
                body: lambda_body,
                env: env.clone(),
            };

            env.borrow_mut().set(name.0.clone(), closure);

            Ok(env.borrow().clone())
        }
    }
}

pub fn eval(expr: &AstExpr, env: &Environment) -> Result<Value, InterpreterError> {
    match expr {
        AstExpr::Ident(name) => env
            .get(name)
            .cloned()
            .ok_or(InterpreterError::unbound_variable(name)),

        AstExpr::Literal(literal) => Ok(literal.clone().into()),

        AstExpr::Tuple(items) => {
            let values = items
                .iter()
                .map(|item| eval(&item.0, env))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Value::tuple(values))
        }

        AstExpr::Lambda { arg, body } => Ok(Value::Closure {
            arg_name: arg.clone(),
            body: (body.0.clone(), body.1),
            env: Rc::new(RefCell::new(env.clone())),
        }),

        AstExpr::Block { statements, expr } => {
            let env = statements
                .iter()
                .try_fold(env.clone(), |env, stmt| eval_stmt(&stmt.0, &env))?;

            expr.as_ref()
                .map(|expr| eval(&expr.0, &env))
                .unwrap_or(Ok(Value::Unit))
        }

        AstExpr::FnApp { lhs, rhs } => {
            let func = eval(&lhs.0, env)?;
            let arg = eval(&rhs.0, env)?;
            apply_function(func, arg)
        }

        AstExpr::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond_value = eval(&condition.0, env)?;
            let cond: bool = cond_value.try_into()?;
            if cond {
                eval(&then_branch.0, env)
            } else {
                eval(&else_branch.0, env)
            }
        }

        AstExpr::OpApp { op, lhs, rhs } => {
            let lhs_value = eval(&lhs.0, env)?;
            let rhs_value = eval(&rhs.0, env)?;
            execute_binary_op(&op.0, lhs_value, rhs_value)
        }
    }
}

fn apply_function(func: Value, arg: Value) -> Result<Value, InterpreterError> {
    match func {
        Value::Closure {
            arg_name,
            body,
            env,
        } => {
            // Check if this is a partially applied binary operation
            if arg_name.0 == "__second" {
                if let (Some(first_arg), Some(Value::BuiltInFn(op_name))) =
                    (env.borrow().get("__first"), env.borrow().get("__op"))
                {
                    return execute_binary_op(op_name, first_arg.clone(), arg);
                }
            }

            // Apply the closure by evaluating its body with the argument bound
            // Use the closure's captured environment, not the current environment
            let new_env = env.borrow().extend(arg_name.0.clone(), arg);
            eval(&body.0, &new_env)
        }
        Value::BuiltInFn(name) => apply_builtin(&name, arg),
        _ => Err(InterpreterError::type_error("Cannot call non-function")),
    }
}

fn apply_builtin(op: &str, first_arg: Value) -> Result<Value, InterpreterError> {
    match op {
        "+" | "-" | "*" | "/" | "==" | "<" | ">" | "&&" | "||" => {
            // Create a partial application closure
            let dummy_expr = ().into();

            Ok(Value::Closure {
                arg_name: ("__second".to_string(), (0..0).into()),
                body: (dummy_expr, (0..0).into()),
                env: Rc::new(RefCell::new(
                    Environment::empty()
                        .extend("__first".to_string(), first_arg)
                        .extend("__op".to_string(), Value::BuiltInFn(op.to_string())),
                )),
            })
        }
        "!" => match first_arg {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(InterpreterError::type_error("Logical NOT requires boolean")),
        },
        _ => Err(InterpreterError::invalid_operation(
            op,
            first_arg.to_string(),
        )),
    }
}

fn execute_binary_op(op: &str, a: Value, b: Value) -> Result<Value, InterpreterError> {
    match op {
        "+" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Num(x + y))
        }
        "-" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Num(x - y))
        }
        "*" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Num(x * y))
        }
        "/" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            if y == 0.0 {
                Err(InterpreterError::DivisionByZero)
            } else {
                Ok(Value::Num(x / y))
            }
        }
        "==" => Ok(Value::Bool(a == b)),
        "<" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Bool(x < y))
        }
        ">" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Bool(x > y))
        }
        ">=" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Bool(x >= y))
        }
        "<=" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok(Value::Bool(x <= y))
        }
        "&&" => {
            let x: bool = a.try_into()?;
            let y: bool = b.try_into()?;
            Ok(Value::Bool(x && y))
        }
        "||" => {
            let x: bool = a.try_into()?;
            let y: bool = b.try_into()?;
            Ok(Value::Bool(x || y))
        }
        _ => Err(InterpreterError::invalid_operation(
            op,
            format!("{}, {}", a, b),
        )),
    }
}

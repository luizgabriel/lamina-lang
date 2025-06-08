use crate::parser::{AstExpr, AstExprNode, AstStmt, AstStmtNode, Literal};
use std::cell::RefCell;
use std::rc::Rc;

use super::{Environment, InterpreterError, Value};

pub fn eval_stmt(stmt: AstStmt, env: &Environment) -> Result<Environment, InterpreterError> {
    match stmt.0 {
        AstStmtNode::Expr(expr) => {
            let _ = eval(expr.0, env)?;
            Ok(env.clone())
        }
        AstStmtNode::Let { name, body } => {
            let value = eval(body.0, env)?;
            Ok(env.extend(&name.0, value))
        }
        AstStmtNode::FnDef { name, params, body } => {
            debug_assert!(
                !params.is_empty(),
                "Function definition must have at least one argument"
            );

            let span = body.1;

            // Build nested lambdas for currying (skip first arg, it becomes the closure's parameter)
            let lambda_body = params
                .iter()
                .skip(1)
                .cloned()
                .rfold(body, |body, arg| (AstExpr::lambda(arg, body), span));

            let env = Rc::new(RefCell::new(env.clone()));
            let closure = Value::Closure {
                param: params[0].clone(),
                body: lambda_body,
                env: env.clone(),
            };

            env.borrow_mut().set(&name.0, closure);

            Ok(env.borrow().clone())
        }
    }
}

pub fn eval(expr: AstExpr, env: &Environment) -> Result<Value, InterpreterError> {
    match expr.0 {
        AstExprNode::Ident(name) => env
            .get(&name)
            .cloned()
            .ok_or(InterpreterError::unbound_variable(name)),

        AstExprNode::Literal(literal) => Ok(literal.into()),

        AstExprNode::Tuple(items) => {
            let values: Result<Vec<_>, _> =
                items.into_iter().map(|item| eval(item.0, env)).collect();

            Ok(Value::tuple(values?))
        }

        AstExprNode::Lambda { param, body } => Ok(Value::Closure {
            param,
            body: *body,
            env: Rc::new(RefCell::new(env.clone())),
        }),

        AstExprNode::Block { statements, expr } => {
            let env = statements
                .into_iter()
                .try_fold(env.clone(), |env, stmt| eval_stmt(stmt.0, &env))?;

            expr.map(|expr| eval(expr.0, &env)).unwrap_or(Ok(().into()))
        }

        AstExprNode::FnApp { lhs, rhs } => {
            let func = eval(lhs.0, env)?;
            let arg = eval(rhs.0, env)?;
            apply_function(func, arg)
        }

        AstExprNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond_value = eval(condition.0, env)?;
            let cond: bool = cond_value.try_into()?;
            if cond {
                eval(then_branch.0, env)
            } else {
                eval(else_branch.0, env)
            }
        }

        AstExprNode::OpApp { op, lhs, rhs } => {
            let lhs_value = eval(lhs.0, env)?;
            let rhs_value = eval(rhs.0, env)?;
            execute_binary_op(&op.0, lhs_value, rhs_value)
        }
    }
}

fn apply_function(func: Value, arg: Value) -> Result<Value, InterpreterError> {
    match func {
        Value::Closure { param, body, env } => {
            // Check if this is a partially applied binary operation
            if param.0 == "__second" {
                if let (Some(first_arg), Some(Value::BuiltInFn(op_name))) =
                    (env.borrow().get("__first"), env.borrow().get("__op"))
                {
                    return execute_binary_op(op_name, first_arg.clone(), arg);
                }
            }

            if arg.is_literal() {
                // Use partial evaluation for literals
                let substituted_body = substitute_value(body.0, &param.0, &arg);
                let partially_evaluated = partial_eval(substituted_body, &env.borrow())?;
                eval(partially_evaluated, &env.borrow())
            } else {
                // Fall back to normal evaluation for complex values (closures, tuples, etc.)
                let new_env = env.borrow().extend(&param.0, arg);
                eval(body.0, &new_env)
            }
        }
        Value::BuiltInFn(name) => apply_builtin(&name, arg),
        _ => Err(InterpreterError::type_error("Cannot call non-function")),
    }
}

/// Substitute a value for all occurrences of a variable in an expression
fn substitute_value(expr: AstExpr, var_name: &str, value: &Value) -> AstExpr {
    match expr.0 {
        AstExprNode::Ident(name) if name == var_name => value_to_expr(value),
        AstExprNode::Ident(name) => AstExpr::ident(name),
        AstExprNode::Literal(lit) => AstExpr::literal(lit.clone()),
        AstExprNode::Tuple(items) => {
            let items = items
                .into_iter()
                .map(|(expr, span)| (substitute_value(expr, var_name, value), span));

            AstExpr::tuple(items)
        }
        AstExprNode::Lambda { param: arg, body } => {
            if arg.0 == var_name {
                return AstExpr::lambda(arg, *body);
            }

            let body = (substitute_value(body.0, var_name, value), body.1);
            AstExpr::lambda(arg, body)
        }
        AstExprNode::FnApp { lhs, rhs } => {
            let lhs = (substitute_value(lhs.0, var_name, value), lhs.1);
            let rhs = (substitute_value(rhs.0, var_name, value), rhs.1);

            AstExpr::fn_app(lhs, rhs)
        }
        AstExprNode::OpApp { op, lhs, rhs } => {
            let lhs = (substitute_value(lhs.0, var_name, value), lhs.1);
            let rhs = (substitute_value(rhs.0, var_name, value), rhs.1);

            AstExpr::op_app(op, lhs, rhs)
        }
        AstExprNode::Block { statements, expr } => {
            let statements = statements
                .into_iter()
                .map(|(stmt, span)| (substitute_value_in_stmt(stmt, var_name, value), span));

            let expr = expr.map(|expr| (substitute_value(expr.0, var_name, value), expr.1));

            AstExpr::block(statements, expr)
        }
        AstExprNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = (substitute_value(condition.0, var_name, value), condition.1);

            let then_branch = (
                substitute_value(then_branch.0, var_name, value),
                then_branch.1,
            );

            let else_branch = (
                substitute_value(else_branch.0, var_name, value),
                else_branch.1,
            );

            AstExpr::if_expr(condition, then_branch, else_branch)
        }
    }
}

/// Substitute a value for all occurrences of a variable in a statement
fn substitute_value_in_stmt(stmt: AstStmt, var_name: &str, value: &Value) -> AstStmt {
    match stmt.0 {
        AstStmtNode::Expr(expr) => {
            let expr = (substitute_value(expr.0, var_name, value), expr.1);
            AstStmt::expr(expr)
        }
        AstStmtNode::Let { name, body } => {
            // Don't substitute if the let binding shadows our variable
            if name.0 == var_name {
                return AstStmt::let_def(name, body);
            }

            let body = (substitute_value(body.0, var_name, value), body.1);
            AstStmt::let_def(name, body)
        }
        AstStmtNode::FnDef {
            name,
            params: args,
            body,
        } => {
            // Don't substitute if any function argument shadows our variable
            if args.iter().any(|arg| arg.0 == var_name) {
                return AstStmt::fn_def(name, args, body);
            }

            let body = (substitute_value(body.0, var_name, value), body.1);
            AstStmt::fn_def(name, args, body)
        }
    }
}

/// Convert a value back to an expression (for substitution)
fn value_to_expr(value: &Value) -> AstExpr {
    match value {
        Value::Literal(lit) => AstExpr::literal(lit.clone()),
        Value::Tuple(items) => AstExpr::tuple(
            items
                .iter()
                .map(|item| (value_to_expr(item), (0..0).into())),
        ),
        Value::Closure { .. } | Value::BuiltInFn(_) => {
            // For closures and built-in functions, we can't easily convert back to expressions
            // In this case, we'll create a dummy identifier that will fail to resolve
            // This forces evaluation through the environment
            AstExpr::ident(format!("__closure_{}", value))
        }
    }
}

/// Perform partial evaluation on a statement
fn partial_eval_stmt(stmt: AstStmt, env: &Environment) -> Result<AstStmt, InterpreterError> {
    match stmt.0 {
        AstStmtNode::Expr(expr) => {
            let eval_expr = partial_eval(expr.0, env)?;
            Ok(AstStmt::expr((eval_expr, expr.1)))
        }
        AstStmtNode::Let { name, body } => {
            let eval_body = partial_eval(body.0, env)?;
            Ok(AstStmt::let_def(name, (eval_body, body.1)))
        }
        AstStmtNode::FnDef {
            name,
            params: args,
            body,
        } => {
            let eval_body = partial_eval(body.0, env)?;
            Ok(AstStmt::fn_def(name, args, (eval_body, body.1)))
        }
    }
}

/// Perform partial evaluation on an expression, trying to compute what can be computed
fn partial_eval(expr: AstExpr, env: &Environment) -> Result<AstExpr, InterpreterError> {
    match expr.0 {
        AstExprNode::OpApp { op, lhs, rhs } => {
            // Try to evaluate operands
            let lhs_eval = partial_eval(lhs.0, env)?;
            let rhs_eval = partial_eval(rhs.0, env)?;

            // If both operands are literals, compute the result
            if let (AstExprNode::Literal(lhs_lit), AstExprNode::Literal(rhs_lit)) =
                (&lhs_eval.0, &rhs_eval.0)
            {
                let lhs_val = lhs_lit.clone().into();
                let rhs_val = rhs_lit.clone().into();

                return match execute_binary_op(&op.0, lhs_val, rhs_val) {
                    Ok(result) => Ok(value_to_expr(&result)),
                    Err(_) => Ok(AstExpr::op_app(op, (lhs_eval, lhs.1), (rhs_eval, rhs.1))),
                };
            }

            Ok(AstExpr::op_app(op, (lhs_eval, lhs.1), (rhs_eval, rhs.1)))
        }
        AstExprNode::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond_eval = partial_eval(condition.0, env)?;

            // If condition is a literal boolean, choose the appropriate branch
            if let AstExprNode::Literal(Literal::Bool(b)) = cond_eval.0 {
                return partial_eval(if b { then_branch.0 } else { else_branch.0 }, env);
            }

            let condition = (cond_eval, condition.1);
            let then_branch = (partial_eval(then_branch.0, env)?, then_branch.1);
            let else_branch = (partial_eval(else_branch.0, env)?, else_branch.1);

            Ok(AstExpr::if_expr(condition, then_branch, else_branch))
        }
        AstExprNode::Tuple(items) => {
            let eval_items: Result<Vec<_>, _> = items
                .into_iter()
                .map(|(item, span)| partial_eval(item, env).map(|e| (e, span)))
                .collect();

            Ok(AstExpr::tuple(eval_items?))
        }
        AstExprNode::FnApp { lhs, rhs } => {
            let lhs_eval = partial_eval(lhs.0, env)?;
            let rhs_eval = partial_eval(rhs.0, env)?;

            Ok(AstExpr::fn_app((lhs_eval, lhs.1), (rhs_eval, rhs.1)))
        }
        AstExprNode::Block { statements, expr } => {
            // Partially evaluate each statement and the final expression
            let eval_statements = statements
                .into_iter()
                .map(|(stmt, span)| partial_eval_stmt(stmt, env).map(|s| (s, span)))
                .collect::<Result<Vec<_>, _>>()?;

            let eval_expr = expr
                .map(|expr| partial_eval(expr.0, env).map(|e| (e, expr.1)))
                .transpose()?;

            Ok(AstExpr::block(eval_statements, eval_expr))
        }
        AstExprNode::Lambda { .. } | AstExprNode::Ident(_) | AstExprNode::Literal(_) => {
            // Lambdas, identifiers, and literals are already in their simplest form
            Ok(expr.clone())
        }
    }
}

fn apply_builtin(op: &str, first_arg: Value) -> Result<Value, InterpreterError> {
    match op {
        "+" | "-" | "*" | "/" | "==" | "<" | ">" | "&&" | "||" => {
            // Create a partial application closure
            let dummy_expr = AstExpr::literal(Literal::Unit);

            Ok(Value::Closure {
                param: ("__second".to_string(), (0..0).into()),
                body: (dummy_expr, (0..0).into()),
                env: Rc::new(RefCell::new(
                    Environment::empty()
                        .extend("__first", first_arg)
                        .extend("__op", Value::builtin(op)),
                )),
            })
        }
        "!" => match first_arg {
            Value::Literal(Literal::Bool(b)) => Ok((!b).into()),
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
            Ok((x + y).into())
        }
        "-" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x - y).into())
        }
        "*" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x * y).into())
        }
        "/" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            if y == 0.0 {
                Err(InterpreterError::DivisionByZero)
            } else {
                Ok((x / y).into())
            }
        }
        "==" => Ok((a == b).into()),
        "<" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x < y).into())
        }
        ">" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x > y).into())
        }
        ">=" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x >= y).into())
        }
        "<=" => {
            let x: f64 = a.try_into()?;
            let y: f64 = b.try_into()?;
            Ok((x <= y).into())
        }
        "&&" => {
            let x: bool = a.try_into()?;
            let y: bool = b.try_into()?;
            Ok((x && y).into())
        }
        "||" => {
            let x: bool = a.try_into()?;
            let y: bool = b.try_into()?;
            Ok((x || y).into())
        }
        _ => Err(InterpreterError::invalid_operation(
            op,
            format!("{}, {}", a, b),
        )),
    }
}

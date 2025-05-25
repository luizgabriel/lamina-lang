use std::collections::HashMap;
use std::fmt::Display;
use thiserror::Error;

use crate::{core::CoreLang, lexer::Spanned, syntax::Literal};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // Stack operations
    LoadConst(Value),
    LoadVar(String),
    StoreVar(String),

    // Function operations
    MakeClosure { arg_name: String, body_len: usize },
    Call,
    Return,

    // Tuple operations
    MakeTuple(usize), // number of elements

    // Binary operations (native implementations)
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Num(f64),
    Bool(bool),
    Tuple(Vec<Value>),
    Closure {
        arg_name: String,
        body: Vec<Instruction>,
        env: Environment,
    },
    NativeFn(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Num(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Closure { arg_name, .. } => write!(f, "<closure λ{}>", arg_name),
            Value::NativeFn(name) => write!(f, "<native {}>", name),
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Unit => Value::Unit,
            Literal::Num(n) => Value::Num(n),
            Literal::Bool(b) => Value::Bool(b),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub bindings: HashMap<String, Value>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut env = Environment {
            bindings: HashMap::new(),
        };

        // Add built-in functions as native functions
        env.bindings
            .insert("+".to_string(), Value::NativeFn("+".to_string()));
        env.bindings
            .insert("-".to_string(), Value::NativeFn("-".to_string()));
        env.bindings
            .insert("*".to_string(), Value::NativeFn("*".to_string()));
        env.bindings
            .insert("/".to_string(), Value::NativeFn("/".to_string()));
        env.bindings
            .insert("==".to_string(), Value::NativeFn("==".to_string()));
        env.bindings
            .insert("<".to_string(), Value::NativeFn("<".to_string()));
        env.bindings
            .insert(">".to_string(), Value::NativeFn(">".to_string()));
        env.bindings
            .insert("&&".to_string(), Value::NativeFn("&&".to_string()));
        env.bindings
            .insert("||".to_string(), Value::NativeFn("||".to_string()));
        env.bindings
            .insert("!".to_string(), Value::NativeFn("!".to_string()));

        env
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment::default()
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn extend(&self, name: String, value: Value) -> Self {
        let mut new_env = self.clone();
        new_env.set(name, value);
        new_env
    }
}

#[derive(Default)]
pub struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler::default()
    }

    pub fn compile(&mut self, expr: &Spanned<CoreLang>) -> Vec<Instruction> {
        self.compile_expr(&expr.0);
        std::mem::take(&mut self.instructions)
    }

    fn compile_expr(&mut self, expr: &CoreLang) {
        match expr {
            CoreLang::Ident(name) => {
                self.instructions
                    .push(Instruction::LoadVar(name.to_string()));
            }
            CoreLang::Literal(literal) => {
                self.instructions
                    .push(Instruction::LoadConst(literal.clone().into()));
            }
            CoreLang::Tuple(items) => {
                for item in items {
                    self.compile_expr(&item.0);
                }
                self.instructions.push(Instruction::MakeTuple(items.len()));
            }
            CoreLang::Lambda { arg, body } => {
                let mut body_compiler = Compiler::new();
                body_compiler.compile_expr(&body.0);
                body_compiler.instructions.push(Instruction::Return);

                self.instructions.push(Instruction::MakeClosure {
                    arg_name: arg.0.to_string(),
                    body_len: body_compiler.instructions.len(),
                });
                self.instructions.extend(body_compiler.instructions);
            }
            CoreLang::Let { name, rhs, then } => {
                self.compile_expr(&rhs.0);
                self.instructions
                    .push(Instruction::StoreVar(name.0.to_string()));
                self.compile_expr(&then.0);
            }
            CoreLang::FnApp { lhs, rhs } => {
                self.compile_expr(&lhs.0);
                self.compile_expr(&rhs.0);
                self.instructions.push(Instruction::Call);
            }
        }
    }
}

#[derive(Default)]
pub struct VM {
    pub stack: Vec<Value>,
    pub env: Environment,
    pub call_stack: Vec<CallFrame>,
}

#[derive(Clone, Debug)]
pub struct CallFrame {
    pub instructions: Vec<Instruction>,
    pub pc: usize,
    pub env: Environment,
}

#[derive(Debug, Error)]
pub enum VMError {
    #[error("Stack underflow: attempted to pop from empty stack")]
    StackUnderflow,

    #[error("Unbound variable: '{name}'")]
    UnboundVariable { name: String },

    #[error("Type error: {message}")]
    TypeError { message: String },

    #[error("Runtime error: {message}")]
    RuntimeError { message: String },

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid operation: {operation} cannot be applied to {operand_type}")]
    InvalidOperation {
        operation: String,
        operand_type: String,
    },

    #[error("Function call error: {message}")]
    CallError { message: String },
}

impl VMError {
    pub fn unbound_variable(name: impl Into<String>) -> Self {
        VMError::UnboundVariable { name: name.into() }
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        VMError::TypeError {
            message: message.into(),
        }
    }

    pub fn runtime_error(message: impl Into<String>) -> Self {
        VMError::RuntimeError {
            message: message.into(),
        }
    }

    pub fn call_error(message: impl Into<String>) -> Self {
        VMError::CallError {
            message: message.into(),
        }
    }

    pub fn invalid_operation(
        operation: impl Into<String>,
        operand_type: impl Into<String>,
    ) -> Self {
        VMError::InvalidOperation {
            operation: operation.into(),
            operand_type: operand_type.into(),
        }
    }
}

impl VM {
    pub fn execute(&mut self, instructions: Vec<Instruction>) -> Result<Value, VMError> {
        self.call_stack.push(CallFrame {
            instructions,
            pc: 0,
            env: self.env.clone(),
        });

        while let Some(mut frame) = self.call_stack.pop() {
            let is_top_level = self.call_stack.is_empty();

            while frame.pc < frame.instructions.len() {
                let instruction = &frame.instructions[frame.pc].clone();
                frame.pc += 1;

                match instruction {
                    Instruction::LoadConst(value) => {
                        self.stack.push(value.clone());
                    }
                    Instruction::LoadVar(name) => {
                        if let Some(value) = frame.env.get(name) {
                            self.stack.push(value.clone());
                        } else {
                            return Err(VMError::unbound_variable(name));
                        }
                    }
                    Instruction::StoreVar(name) => {
                        let value = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        frame.env.set(name.clone(), value.clone());
                        // If this is the top-level frame, also update the VM's environment
                        if is_top_level {
                            self.env.set(name.clone(), value);
                        }
                    }
                    Instruction::MakeClosure { arg_name, body_len } => {
                        let body_start = frame.pc;
                        let body_end = frame.pc + body_len;
                        let body = frame.instructions[body_start..body_end].to_vec();
                        frame.pc = body_end;

                        let closure = Value::Closure {
                            arg_name: arg_name.clone(),
                            body,
                            env: frame.env.clone(),
                        };
                        self.stack.push(closure);
                    }
                    Instruction::Call => {
                        let arg = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let func = self.stack.pop().ok_or(VMError::StackUnderflow)?;

                        match func {
                            Value::Closure {
                                arg_name,
                                body,
                                env,
                            } => {
                                let new_env = env.extend(arg_name, arg);
                                self.call_stack.push(frame);
                                self.call_stack.push(CallFrame {
                                    instructions: body,
                                    pc: 0,
                                    env: new_env,
                                });
                                break;
                            }
                            Value::NativeFn(name) => {
                                // For binary operations, we need to create a partial application
                                let result = self.create_partial_application(&name, arg)?;
                                self.stack.push(result);
                            }
                            _ => {
                                return Err(VMError::type_error("Cannot call non-function"));
                            }
                        }
                    }
                    Instruction::Return => {
                        break;
                    }
                    Instruction::MakeTuple(size) => {
                        let mut items = Vec::new();
                        for _ in 0..*size {
                            items.push(self.stack.pop().ok_or(VMError::StackUnderflow)?);
                        }
                        items.reverse();
                        self.stack.push(Value::Tuple(items));
                    }
                    Instruction::Add => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => self.stack.push(Value::Num(x + y)),
                            _ => {
                                return Err(VMError::type_error("Addition requires numbers"));
                            }
                        }
                    }
                    Instruction::Sub => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => self.stack.push(Value::Num(x - y)),
                            _ => {
                                return Err(VMError::type_error("Subtraction requires numbers"));
                            }
                        }
                    }
                    Instruction::Mul => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => self.stack.push(Value::Num(x * y)),
                            _ => {
                                return Err(VMError::type_error("Multiplication requires numbers"));
                            }
                        }
                    }
                    Instruction::Div => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => {
                                if y == 0.0 {
                                    return Err(VMError::DivisionByZero);
                                }
                                self.stack.push(Value::Num(x / y));
                            }
                            _ => {
                                return Err(VMError::type_error("Division requires numbers"));
                            }
                        }
                    }
                    Instruction::Eq => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        self.stack.push(Value::Bool(a == b));
                    }
                    Instruction::Lt => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => self.stack.push(Value::Bool(x < y)),
                            _ => {
                                return Err(VMError::type_error("Comparison requires numbers"));
                            }
                        }
                    }
                    Instruction::Gt => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Num(x), Value::Num(y)) => self.stack.push(Value::Bool(x > y)),
                            _ => {
                                return Err(VMError::type_error("Comparison requires numbers"));
                            }
                        }
                    }
                    Instruction::And => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Bool(x), Value::Bool(y)) => {
                                self.stack.push(Value::Bool(x && y))
                            }
                            _ => {
                                return Err(VMError::type_error("Logical AND requires booleans"));
                            }
                        }
                    }
                    Instruction::Or => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (Value::Bool(x), Value::Bool(y)) => {
                                self.stack.push(Value::Bool(x || y))
                            }
                            _ => {
                                return Err(VMError::type_error("Logical OR requires booleans"));
                            }
                        }
                    }
                    Instruction::Not => {
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match a {
                            Value::Bool(x) => self.stack.push(Value::Bool(!x)),
                            _ => {
                                return Err(VMError::type_error("Logical NOT requires boolean"));
                            }
                        }
                    }
                }
            }
        }

        Ok(self.stack.pop().unwrap_or(Value::Unit))
    }

    fn create_partial_application(&self, op: &str, first_arg: Value) -> Result<Value, VMError> {
        let instruction = match op {
            "+" => Instruction::Add,
            "-" => Instruction::Sub,
            "*" => Instruction::Mul,
            "/" => Instruction::Div,
            "==" => Instruction::Eq,
            "<" => Instruction::Lt,
            ">" => Instruction::Gt,
            "&&" => Instruction::And,
            "||" => Instruction::Or,
            "!" => {
                // Unary operation - apply immediately
                match first_arg {
                    Value::Bool(b) => return Ok(Value::Bool(!b)),
                    _ => {
                        return Err(VMError::type_error("Logical NOT requires boolean"));
                    }
                }
            }
            _ => {
                return Err(VMError::invalid_operation(op, first_arg.to_string()));
            }
        };

        // Create a closure that captures the first argument and applies the operation
        let body = vec![
            Instruction::LoadVar("__first".to_string()),
            Instruction::LoadVar("__second".to_string()),
            instruction,
            Instruction::Return,
        ];

        let env = Environment::default().extend("__first".to_string(), first_arg);

        Ok(Value::Closure {
            arg_name: "__second".to_string(),
            body,
            env,
        })
    }

    pub fn get_env(&self) -> &Environment {
        &self.env
    }

    pub fn set_var(&mut self, name: String, value: Value) {
        self.env.set(name, value);
    }

    pub fn get_stack(&self) -> &Vec<Value> {
        &self.stack
    }
}

pub fn compile_and_execute(expr: &Spanned<CoreLang>) -> Result<Value, VMError> {
    let mut compiler = Compiler::new();
    let instructions = compiler.compile(expr);
    let mut vm = VM::default();
    vm.execute(instructions)
}

use super::{Instruction, VMError, VmEnv, VmValue};

#[derive(Default)]
pub struct VM {
    pub stack: Vec<VmValue>,
    pub env: VmEnv,
    pub call_stack: Vec<CallFrame>,
}

#[derive(Clone, Debug)]
pub struct CallFrame {
    pub instructions: Vec<Instruction>,
    pub pc: usize,
    pub env: VmEnv,
}

impl VM {
    pub fn execute(&mut self, instructions: Vec<Instruction>) -> Result<VmValue, VMError> {
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

                        let closure = VmValue::Closure {
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
                            VmValue::Closure {
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
                            VmValue::NativeFn(name) => {
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
                        self.stack.push(VmValue::Tuple(items));
                    }
                    Instruction::Add => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                self.stack.push(VmValue::Num(x + y))
                            }
                            _ => {
                                return Err(VMError::type_error("Addition requires numbers"));
                            }
                        }
                    }
                    Instruction::Sub => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                self.stack.push(VmValue::Num(x - y))
                            }
                            _ => {
                                return Err(VMError::type_error("Subtraction requires numbers"));
                            }
                        }
                    }
                    Instruction::Mul => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                self.stack.push(VmValue::Num(x * y))
                            }
                            _ => {
                                return Err(VMError::type_error("Multiplication requires numbers"));
                            }
                        }
                    }
                    Instruction::Div => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                if y == 0.0 {
                                    return Err(VMError::DivisionByZero);
                                }
                                self.stack.push(VmValue::Num(x / y));
                            }
                            _ => {
                                return Err(VMError::type_error("Division requires numbers"));
                            }
                        }
                    }
                    Instruction::Eq => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        self.stack.push(VmValue::Bool(a == b));
                    }
                    Instruction::Lt => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                self.stack.push(VmValue::Bool(x < y))
                            }
                            _ => {
                                return Err(VMError::type_error("Comparison requires numbers"));
                            }
                        }
                    }
                    Instruction::Gt => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Num(x), VmValue::Num(y)) => {
                                self.stack.push(VmValue::Bool(x > y))
                            }
                            _ => {
                                return Err(VMError::type_error("Comparison requires numbers"));
                            }
                        }
                    }
                    Instruction::And => {
                        let b = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match (a, b) {
                            (VmValue::Bool(x), VmValue::Bool(y)) => {
                                self.stack.push(VmValue::Bool(x && y))
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
                            (VmValue::Bool(x), VmValue::Bool(y)) => {
                                self.stack.push(VmValue::Bool(x || y))
                            }
                            _ => {
                                return Err(VMError::type_error("Logical OR requires booleans"));
                            }
                        }
                    }
                    Instruction::Not => {
                        let a = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match a {
                            VmValue::Bool(x) => self.stack.push(VmValue::Bool(!x)),
                            _ => {
                                return Err(VMError::type_error("Logical NOT requires boolean"));
                            }
                        }
                    }
                    Instruction::Jump(offset) => {
                        frame.pc += offset;
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = self.stack.pop().ok_or(VMError::StackUnderflow)?;
                        match condition {
                            VmValue::Bool(false) => {
                                frame.pc += offset;
                            }
                            VmValue::Bool(true) => {
                                // Continue to next instruction
                            }
                            _ => {
                                return Err(VMError::type_error("Jump condition must be boolean"));
                            }
                        }
                    }
                }
            }
        }

        Ok(self.stack.pop().unwrap_or(VmValue::Unit))
    }

    fn create_partial_application(&self, op: &str, first_arg: VmValue) -> Result<VmValue, VMError> {
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
                    VmValue::Bool(b) => return Ok(VmValue::Bool(!b)),
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

        let env = VmEnv::default().extend("__first".to_string(), first_arg);

        Ok(VmValue::Closure {
            arg_name: "__second".to_string(),
            body,
            env,
        })
    }

    pub fn get_env(&self) -> &VmEnv {
        &self.env
    }

    pub fn set_var(&mut self, name: String, value: VmValue) {
        self.env.set(name, value);
    }

    pub fn get_stack(&self) -> &Vec<VmValue> {
        &self.stack
    }
}

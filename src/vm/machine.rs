use super::{Instruction, VMError, VmEnv, VmValue};
use std::cell::RefCell;
use std::rc::Rc;

pub struct VM {
    pub env: Rc<RefCell<VmEnv>>,
    pub call_stack: Vec<CallFrame>,
    max_call_stack_depth: usize,
}

#[derive(Clone, Debug)]
pub struct CallFrame {
    pub instructions: Vec<Instruction>,
    pub pc: usize,
    pub env: Rc<RefCell<VmEnv>>,
    pub stack: Vec<VmValue>,
}

impl CallFrame {
    pub fn new(instructions: Vec<Instruction>, env: Rc<RefCell<VmEnv>>) -> Self {
        Self {
            instructions,
            pc: 0,
            env,
            stack: Vec::new(),
        }
    }

    pub fn pop_stack(&mut self) -> Result<VmValue, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
}

impl VM {
    pub fn new(max_call_stack_depth: usize, env: VmEnv) -> Self {
        Self {
            env: Rc::new(RefCell::new(env)),
            max_call_stack_depth,
            call_stack: Vec::new(),
        }
    }

    pub fn execute(&mut self, instructions: Vec<Instruction>) -> Result<VmValue, VMError> {
        self.call_stack
            .push(CallFrame::new(instructions, self.env.clone()));

        let mut return_value = VmValue::Unit;

        while let Some(mut frame) = self.call_stack.pop() {
            while frame.pc < frame.instructions.len() {
                let instruction = &frame.instructions[frame.pc].clone();
                frame.pc += 1;

                match instruction {
                    Instruction::LoadConst(value) => {
                        frame.stack.push(value.clone());
                    }
                    Instruction::LoadVar(name) => {
                        if let Some(value) = frame.env.borrow().get(name) {
                            frame.stack.push(value.clone());
                        } else {
                            return Err(VMError::unbound_variable(name));
                        }
                    }
                    Instruction::StoreVar(name) => {
                        let value = frame.pop_stack()?;
                        frame.env.borrow_mut().set(name.clone(), value);
                    }
                    Instruction::MakeRecursiveClosure {
                        fn_name,
                        arg_name,
                        body_len,
                    } => {
                        let body = self.extract_closure_body(&mut frame, *body_len);
                        let env = frame.env.clone();
                        let closure = VmValue::Closure {
                            arg_name: arg_name.clone(),
                            body,
                            env: env.clone(),
                        };
                        env.borrow_mut().set(fn_name.clone(), closure.clone());
                        frame.stack.push(closure);
                    }
                    Instruction::MakeClosure { arg_name, body_len } => {
                        let body = self.extract_closure_body(&mut frame, *body_len);
                        let closure = VmValue::Closure {
                            arg_name: arg_name.clone(),
                            body,
                            env: frame.env.clone(),
                        };
                        frame.stack.push(closure);
                    }
                    Instruction::Call => {
                        let arg = frame.pop_stack()?;
                        let func = frame.pop_stack()?;
                        if self.call_stack.len() + 2 > self.max_call_stack_depth {
                            return Err(VMError::stack_overflow());
                        }

                        match func {
                            VmValue::Closure {
                                arg_name,
                                body,
                                env,
                            } => {
                                self.call_stack.push(frame);
                                let new_env = env.borrow().extend(arg_name.clone(), arg.clone());
                                let new_frame =
                                    CallFrame::new(body, Rc::new(RefCell::new(new_env)));
                                self.call_stack.push(new_frame);
                                break;
                            }
                            VmValue::BuiltInFn(name) => {
                                let result = self.create_partial_application(&name, arg)?;
                                frame.stack.push(result);
                            }
                            _ => {
                                return Err(VMError::type_error("Cannot call non-function"));
                            }
                        }
                    }
                    Instruction::Return => {
                        return_value = frame.pop_stack()?;
                        break;
                    }
                    Instruction::MakeTuple(size) => {
                        let mut items = Vec::new();
                        for _ in 0..*size {
                            items.push(frame.pop_stack()?);
                        }
                        items.reverse();
                        frame.stack.push(VmValue::Tuple(items));
                    }
                    Instruction::Add => {
                        self.execute_binary_operation::<f64, f64, _>(&mut frame, |x, y| x + y)?;
                    }
                    Instruction::Sub => {
                        self.execute_binary_operation::<f64, f64, _>(&mut frame, |x, y| x - y)?;
                    }
                    Instruction::Mul => {
                        self.execute_binary_operation::<f64, f64, _>(&mut frame, |x, y| x * y)?;
                    }
                    Instruction::Div => {
                        self.execute_division(&mut frame)?;
                    }
                    Instruction::Eq => {
                        let b = frame.pop_stack()?;
                        let a = frame.pop_stack()?;
                        frame.stack.push(VmValue::Bool(a == b));
                    }
                    Instruction::Lt => {
                        self.execute_binary_operation::<f64, bool, _>(&mut frame, |x, y| x < y)?;
                    }
                    Instruction::Gt => {
                        self.execute_binary_operation::<f64, bool, _>(&mut frame, |x, y| x > y)?;
                    }
                    Instruction::And => {
                        self.execute_binary_operation::<bool, bool, _>(&mut frame, |x, y| x && y)?;
                    }
                    Instruction::Or => {
                        self.execute_binary_operation::<bool, bool, _>(&mut frame, |x, y| x || y)?;
                    }
                    Instruction::Not => {
                        let a = frame.pop_stack()?;
                        let x: bool = a.try_into()?;
                        frame.stack.push(VmValue::Bool(!x));
                    }
                    Instruction::Jump(offset) => {
                        frame.pc += offset;
                    }
                    Instruction::JumpIfFalse(offset) => {
                        let condition = frame.pop_stack()?;
                        let cond: bool = condition.try_into()?;
                        if !cond {
                            frame.pc += offset;
                        }
                    }
                }
            }
            if let Some(prev_frame) = self.call_stack.last_mut() {
                prev_frame.stack.push(return_value.clone());
            }
        }

        Ok(return_value)
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
            "!" => match first_arg {
                VmValue::Bool(b) => return Ok(VmValue::Bool(!b)),
                _ => {
                    return Err(VMError::type_error("Logical NOT requires boolean"));
                }
            },
            _ => {
                return Err(VMError::invalid_operation(op, first_arg.to_string()));
            }
        };

        let body = vec![
            Instruction::LoadVar("__first".to_string()),
            Instruction::LoadVar("__second".to_string()),
            instruction,
            Instruction::Return,
        ];

        let env = VmEnv::empty().extend("__first".to_string(), first_arg);

        Ok(VmValue::Closure {
            arg_name: "__second".to_string(),
            body,
            env: Rc::new(RefCell::new(env)),
        })
    }

    fn execute_division(&mut self, frame: &mut CallFrame) -> Result<(), VMError> {
        let b = frame.pop_stack()?;
        let a = frame.pop_stack()?;
        let x: f64 = a.try_into()?;
        let y: f64 = b.try_into()?;
        if y == 0.0 {
            return Err(VMError::DivisionByZero);
        }
        frame.stack.push(VmValue::Num(x / y));
        Ok(())
    }

    fn execute_binary_operation<T, U, F>(
        &mut self,
        frame: &mut CallFrame,
        op: F,
    ) -> Result<(), VMError>
    where
        T: TryFrom<VmValue, Error = VMError>,
        U: Into<VmValue>,
        F: FnOnce(T, T) -> U,
    {
        let b = frame.pop_stack()?;
        let a = frame.pop_stack()?;
        let x: T = a.try_into()?;
        let y: T = b.try_into()?;
        frame.stack.push(op(x, y).into());
        Ok(())
    }

    fn extract_closure_body(&mut self, frame: &mut CallFrame, body_len: usize) -> Vec<Instruction> {
        let body_start = frame.pc;
        let body_end = frame.pc + body_len;
        let body = frame.instructions[body_start..body_end].to_vec();
        frame.pc = body_end;
        body
    }
}

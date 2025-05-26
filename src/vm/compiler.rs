use crate::{
    ir::{IrExpr, lowering_expr, lowering_stmt},
    lexer::Spanned,
    parser::{ParseError, parse_expr, parse_stmt},
    syntax::{AstExpr, AstStmt},
};

use super::Instruction;

pub struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn compile_input<'src>(
        &mut self,
        input: &'src str,
    ) -> Result<Vec<Instruction>, ParseError<'src>> {
        match parse_expr(input) {
            Ok(ast) => Ok(self.compile_ast(ast)),
            Err(_) => match parse_stmt(input) {
                Ok(ast) => Ok(self.compile_stmt(ast)),
                Err(err) => Err(err),
            },
        }
    }

    pub fn compile_stmt(&mut self, stmt: Spanned<AstStmt>) -> Vec<Instruction> {
        let ir = lowering_stmt(stmt.0, (().into(), stmt.1));
        self.compile_ir(ir);
        std::mem::take(&mut self.instructions)
    }

    pub fn compile_ast(&mut self, expr: Spanned<AstExpr>) -> Vec<Instruction> {
        let ir = lowering_expr(expr);
        self.compile_ir(ir.0);
        self.instructions.push(Instruction::Return);
        std::mem::take(&mut self.instructions)
    }

    /// Helper method to compile a function body and return the instructions with a Return at the end
    fn compile_function_body(&mut self, body: IrExpr) -> Vec<Instruction> {
        let mut body_compiler = Compiler::new();
        body_compiler.compile_ir(body);
        body_compiler.instructions.push(Instruction::Return);
        body_compiler.instructions
    }

    /// Helper method to compile an expression and return the instructions without a Return
    fn compile_expression(&mut self, expr: IrExpr) -> Vec<Instruction> {
        let mut expr_compiler = Compiler::new();
        expr_compiler.compile_ir(expr);
        expr_compiler.instructions
    }

    fn compile_ir(&mut self, expr: IrExpr) {
        match expr {
            IrExpr::Ident(name) => {
                self.instructions
                    .push(Instruction::LoadVar(name.to_string()));
            }
            IrExpr::Literal(literal) => {
                self.instructions
                    .push(Instruction::LoadConst(literal.into()));
            }
            IrExpr::Tuple(items) => {
                let size = items.len();
                for item in items {
                    self.compile_ir(item.0);
                }
                self.instructions.push(Instruction::MakeTuple(size));
            }
            IrExpr::Lambda { arg, body } => {
                let body_instructions = self.compile_function_body(body.0.clone());

                self.instructions.push(Instruction::MakeClosure {
                    arg_name: arg.0.to_string(),
                    body_len: body_instructions.len(),
                });
                self.instructions.extend(body_instructions);
            }
            IrExpr::Let { name, rhs, then } => {
                match rhs.0 {
                    IrExpr::Lambda { arg, body } => {
                        let body_instructions = self.compile_function_body(body.0.clone());

                        self.instructions.push(Instruction::MakeRecursiveClosure {
                            fn_name: name.0.to_string(),
                            arg_name: arg.0.to_string(),
                            body_len: body_instructions.len(),
                        });

                        self.instructions.extend(body_instructions);
                    }
                    _ => {
                        self.compile_ir(rhs.0.clone());
                    }
                }

                self.instructions
                    .push(Instruction::StoreVar(name.0.to_string()));
                self.compile_ir((*then).0);
            }
            IrExpr::FnApp { lhs, rhs } => {
                self.compile_ir((*lhs).0);
                self.compile_ir((*rhs).0);
                self.instructions.push(Instruction::Call);
            }
            IrExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Compile condition
                self.compile_ir((*condition).0);

                // Compile then and else branches separately to get their lengths
                let then_instructions = self.compile_expression((*then_branch).0);
                let else_instructions = self.compile_expression((*else_branch).0);

                // JumpIfFalse to else branch (skip then branch + jump instruction)
                self.instructions
                    .push(Instruction::JumpIfFalse(then_instructions.len() + 1));

                // Add then branch instructions
                self.instructions.extend(then_instructions);

                // Jump over else branch
                self.instructions
                    .push(Instruction::Jump(else_instructions.len()));

                // Add else branch instructions
                self.instructions.extend(else_instructions);
            }
        }
    }
}

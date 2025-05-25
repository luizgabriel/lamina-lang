use crate::{
    ir::{IrExpr, lowering_expr, lowering_stmt},
    lexer::Spanned,
    parser::{ParseError, parse_expr, parse_stmt},
    syntax::{AstExpr, AstStmt},
};

use super::Instruction;

#[derive(Default)]
pub struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler::default()
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
        std::mem::take(&mut self.instructions)
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
                let mut body_compiler = Compiler::new();
                body_compiler.compile_ir((*body).0);
                body_compiler.instructions.push(Instruction::Return);

                self.instructions.push(Instruction::MakeClosure {
                    arg_name: arg.0.to_string(),
                    body_len: body_compiler.instructions.len(),
                });
                self.instructions.extend(body_compiler.instructions);
            }
            IrExpr::Let { name, rhs, then } => {
                self.compile_ir((*rhs).0);
                self.instructions
                    .push(Instruction::StoreVar(name.0.to_string()));
                self.compile_ir((*then).0);
            }
            IrExpr::FnApp { lhs, rhs } => {
                self.compile_ir((*lhs).0);
                self.compile_ir((*rhs).0);
                self.instructions.push(Instruction::Call);
            }
        }
    }
}

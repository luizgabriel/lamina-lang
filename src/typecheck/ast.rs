use crate::lexer::Spanned;
use crate::parser::{AstExprNode, AstStmtNode, AstType, Literal};

/// Typed AST statement that contains both type information and the statement structure
#[derive(Clone, Debug, PartialEq)]
pub struct TypedStmt(AstStmtNode<TypedAstExpr>);

impl TypedStmt {
    pub fn fn_def(
        name: Spanned<String>,
        args: Vec<Spanned<String>>,
        body: Spanned<TypedAstExpr>,
    ) -> Self {
        Self(AstStmtNode::FnDef {
            name,
            params: args,
            body,
        })
    }

    pub fn let_def(name: Spanned<String>, body: Spanned<TypedAstExpr>) -> Self {
        Self(AstStmtNode::Let { name, body })
    }

    pub fn expr(expr: Spanned<TypedAstExpr>) -> Self {
        Self(AstStmtNode::Expr(expr))
    }
}

/// Typed AST expression that contains both type information and the expression structure
#[derive(Clone, Debug, PartialEq)]
pub struct TypedAstExpr {
    pub ty: AstType,
    pub expr: AstExprNode<TypedAstExpr, TypedStmt>,
}

impl TypedAstExpr {
    pub fn literal(literal: Literal) -> Self {
        Self {
            ty: AstType::from(literal),
            expr: AstExprNode::Literal(literal),
        }
    }

    pub fn ident(name: impl Into<String>, ty: AstType) -> Self {
        Self {
            ty,
            expr: AstExprNode::Ident(name.into()),
        }
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<Self>>, ty: AstType) -> Self {
        Self {
            ty,
            expr: AstExprNode::Tuple(items.into_iter().collect()),
        }
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>, ty: AstType) -> Self {
        Self {
            ty,
            expr: AstExprNode::FnApp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        }
    }

    pub fn op_app(
        op: Spanned<String>,
        lhs: Spanned<Self>,
        rhs: Spanned<Self>,
        ty: AstType,
    ) -> Self {
        Self {
            ty,
            expr: AstExprNode::OpApp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        }
    }

    pub fn block(
        statements: impl IntoIterator<Item = Spanned<TypedStmt>>,
        expr: Option<Spanned<Self>>,
        ty: AstType,
    ) -> Self {
        Self {
            ty,
            expr: AstExprNode::Block {
                statements: statements.into_iter().collect(),
                expr: expr.map(Box::new),
            },
        }
    }

    pub fn if_expr(
        condition: Spanned<Self>,
        then_branch: Spanned<Self>,
        else_branch: Spanned<Self>,
        ty: AstType,
    ) -> Self {
        Self {
            ty,
            expr: AstExprNode::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
        }
    }

    pub fn lambda(arg: Spanned<String>, body: Spanned<Self>, ty: AstType) -> Self {
        Self {
            ty,
            expr: AstExprNode::Lambda {
                param: arg,
                body: Box::new(body),
            },
        }
    }
}

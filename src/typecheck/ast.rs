use crate::lexer::Spanned;
use crate::parser::{AstExprNode, AstStmtNode, Literal};
use crate::typecheck::Type;

/// Typed AST statement that contains both type information and the statement structure
#[derive(Clone, Debug, PartialEq)]
pub struct TypedStmt(AstStmtNode<TypedAstExpr>);

impl TypedStmt {
    pub fn fn_def(
        name: Spanned<String>,
        params: impl IntoIterator<Item = Spanned<String>>,
        body: Spanned<TypedAstExpr>,
    ) -> Self {
        Self(AstStmtNode::FnDef {
            name,
            params: params.into_iter().collect(),
            body,
        })
    }

    pub fn assign(name: Spanned<String>, body: Spanned<TypedAstExpr>) -> Self {
        Self(AstStmtNode::Assign { name, body })
    }

    pub fn expr(expr: Spanned<TypedAstExpr>) -> Self {
        Self(AstStmtNode::Expr(expr))
    }
}

/// Typed AST expression that contains both type information and the expression structure
#[derive(Clone, Debug, PartialEq)]
pub struct TypedAstExpr {
    pub ty: Type,
    pub node: AstExprNode<TypedAstExpr, TypedStmt>,
}

impl TypedAstExpr {
    pub fn literal(literal: Literal) -> Self {
        Self {
            ty: (&literal).into(),
            node: AstExprNode::Literal(literal),
        }
    }

    pub fn ident(name: impl Into<String>, ty: Type) -> Self {
        Self {
            ty,
            node: AstExprNode::Ident(name.into()),
        }
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<Self>>, ty: Type) -> Self {
        Self {
            ty,
            node: AstExprNode::Tuple(items.into_iter().collect()),
        }
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>, ty: Type) -> Self {
        Self {
            ty,
            node: AstExprNode::FnApp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        }
    }

    pub fn op_app(op: Spanned<String>, lhs: Spanned<Self>, rhs: Spanned<Self>, ty: Type) -> Self {
        Self {
            ty,
            node: AstExprNode::OpApp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        }
    }

    pub fn block(
        statements: impl IntoIterator<Item = Spanned<TypedStmt>>,
        expr: Option<Spanned<Self>>,
        ty: Type,
    ) -> Self {
        Self {
            ty,
            node: AstExprNode::Block {
                statements: statements.into_iter().collect(),
                expr: expr.map(Box::new),
            },
        }
    }

    pub fn if_expr(
        condition: Spanned<Self>,
        then_branch: Spanned<Self>,
        else_branch: Spanned<Self>,
        ty: Type,
    ) -> Self {
        Self {
            ty,
            node: AstExprNode::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
        }
    }

    pub fn lambda(arg: Spanned<String>, body: Spanned<Self>, ty: Type) -> Self {
        Self {
            ty,
            node: AstExprNode::Lambda {
                param: arg,
                body: Box::new(body),
            },
        }
    }
}

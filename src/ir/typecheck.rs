use crate::ir::IrExpr;
use crate::lexer::Spanned;
use crate::syntax::{AstLiteral, AstType};

use super::IrExprNode;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnificationFail,
    UnboundVariable(String),
    Mismatch(AstType, AstType),
    // Add more as needed
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedIrExpr<'src> {
    ty: AstType,
    expr: IrExprNode<'src, TypedIrExpr<'src>>,
}

impl<'src> AsRef<IrExprNode<'src, TypedIrExpr<'src>>> for TypedIrExpr<'src> {
    fn as_ref(&self) -> &IrExprNode<'src, TypedIrExpr<'src>> {
        &self.expr
    }
}

impl<'src> AsRef<AstType> for TypedIrExpr<'src> {
    fn as_ref(&self) -> &AstType {
        &self.ty
    }
}

impl<'src> TypedIrExpr<'src> {
    pub fn literal(literal: impl Into<AstLiteral>) -> Self {
        let lit = literal.into();
        Self {
            ty: AstType::from(&lit),
            expr: IrExprNode::Literal(lit),
        }
    }

    pub fn ident(name: &'src str, ty: AstType) -> Self {
        Self {
            ty,
            expr: IrExprNode::Ident(name),
        }
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<TypedIrExpr<'src>>>) -> Self {
        let (items, types) = items
            .into_iter()
            .map(|item| {
                let (typed_expr, span) = item;
                ((typed_expr.clone(), span), typed_expr.ty)
            })
            .unzip();

        Self {
            ty: AstType::Tuple(types),
            expr: IrExprNode::Tuple(items),
        }
    }

    pub fn lambda(
        arg: Spanned<&'src str>,
        arg_ty: AstType,
        body: Spanned<TypedIrExpr<'src>>,
    ) -> Self {
        Self {
            ty: AstType::func(arg_ty, body.0.ty.clone()),
            expr: IrExprNode::Lambda {
                arg,
                body: Box::new(body),
            },
        }
    }

    pub fn let_binding(
        name: Spanned<&'src str>,
        ty: AstType,
        rhs: Spanned<TypedIrExpr<'src>>,
        then: Spanned<TypedIrExpr<'src>>,
    ) -> Self {
        Self {
            ty,
            expr: IrExprNode::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            },
        }
    }
}

pub fn typecheck_ir<'src>(_: &IrExpr<'src>) -> Result<TypedIrExpr<'src>, TypeError> {
    // TODO: Implement Hindley-Milner type inference here
    unimplemented!("Hindley-Milner type inference not yet implemented")
}

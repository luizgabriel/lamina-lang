use crate::{
    parser::{AstExpr, AstExprNode, AstType},
    typecheck::{TyEnvironment, TypeError},
};

pub fn infer(expr: &AstExpr, env: &TyEnvironment) -> Result<AstType, TypeError> {
    match &expr.0 {
        AstExprNode::Literal(literal) => Ok(AstType::from(*literal)),
        AstExprNode::Ident(name) => env
            .get(name)
            .map(|t| t.0.clone())
            .ok_or(TypeError::unbound_variable(name)),
        AstExprNode::Tuple(items) => {
            let types: Result<Vec<_>, _> = items
                .iter()
                .map(|(item, span)| infer(item, env).map(|ty| (ty, *span)))
                .collect();

            Ok(AstType::Tuple(types?))
        }
        _ => todo!(),
    }
}

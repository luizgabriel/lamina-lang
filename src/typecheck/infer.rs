use crate::{
    lexer::Spanned,
    parser::{AstExpr, AstExprNode},
    typecheck::{unify, Subst, Type, TypeEnvironment, TypeError, TypeVarContext},
};

fn infer_tuple(
    items: &im::Vector<Spanned<AstExpr>>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let mut types = im::Vector::new();
    let mut subst = Subst::empty();

    for item in items {
        let (ty, s) = infer(&item.0, &env.clone().apply(&subst), ctx)?;
        types.push_back(ty);
        subst = subst.compose(s);
    }

    Ok((Type::Tuple(types), subst))
}

fn infer_ident(name: &str, env: &TypeEnvironment) -> Result<(Type, Subst), TypeError> {
    env.get(name)
        .cloned()
        .ok_or(TypeError::unbound_variable(name))
        .map(|ty| (ty, Subst::empty()))
}

fn infer_lambda(
    param: &Spanned<String>,
    body: &Spanned<AstExpr>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let param_ty = Type::Var(ctx.fresh());
    let new_env = env.extend(param.0.clone(), param_ty.clone());
    let (body_type, subst) = infer(&body.0, &new_env, ctx)?;

    Ok((Type::func(subst.apply(param_ty), body_type), subst))
}

fn infer_fn_app(
    lhs: &Spanned<AstExpr>,
    rhs: &Spanned<AstExpr>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let (lhs_ty, s1) = infer(&lhs.0, env, ctx)?;
    let (rhs_ty, s2) = infer(&rhs.0, &env.clone().apply(&s1), ctx)?;

    let ret_ty = Type::Var(ctx.fresh());
    let s3 = unify(s2.apply(lhs_ty), Type::func(rhs_ty, ret_ty.clone()))?;

    Ok((s3.apply(ret_ty), s1.compose(s2).compose(s3)))
}

fn infer_if(
    condition: &Spanned<AstExpr>,
    then_branch: &Spanned<AstExpr>,
    else_branch: &Spanned<AstExpr>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let (cond_ty, s1) = infer(&condition.0, env, ctx)?;
    let (then_ty, s2) = infer(&then_branch.0, &env.clone().apply(&s1), ctx)?;
    let (else_ty, s3) = infer(&else_branch.0, &env.clone().apply(&s2), ctx)?;

    let s4 = unify(cond_ty, Type::Bool)?;
    let s5 = unify(s4.apply(then_ty.clone()), s4.apply(else_ty))?;

    Ok((
        s5.apply(then_ty),
        s1.compose(s2).compose(s3).compose(s4).compose(s5),
    ))
}
pub fn infer(
    expr: &AstExpr,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    match &expr.0 {
        AstExprNode::Literal(literal) => Ok((literal.into(), Subst::empty())),

        AstExprNode::Ident(name) => infer_ident(name, env),

        AstExprNode::Tuple(items) => infer_tuple(items, env, ctx),

        AstExprNode::Lambda { param, body } => infer_lambda(param, body, env, ctx),

        AstExprNode::FnApp { lhs, rhs } => infer_fn_app(lhs, rhs, env, ctx),

        AstExprNode::If {
            condition,
            then_branch,
            else_branch,
        } => infer_if(condition, then_branch, else_branch, env, ctx),

        _ => todo!(),
    }
}

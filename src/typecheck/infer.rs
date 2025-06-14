use crate::{
    lexer::Spanned,
    parser::{AstExpr, AstExprNode, AstStmt, AstStmtNode},
    typecheck::{unify, Subst, Type, TypeEnvironment, TypeError, TypeVarContext},
};

fn infer_tuple(
    items: &[Spanned<AstExpr>],
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let mut types = Vec::with_capacity(items.len());
    let mut subst = Subst::empty();

    for item in items {
        let (ty, s) = infer(&item.0, &env.apply(&subst), ctx)?;
        types.push(ty);
        subst = subst.compose(&s);
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
    let (rhs_ty, s2) = infer(&rhs.0, &env.apply(&s1), ctx)?;

    let ret_ty = Type::Var(ctx.fresh());
    let s3 = unify(
        s1.compose(&s2).apply(lhs_ty),
        Type::func(s2.apply(rhs_ty), ret_ty.clone()),
    )?;

    Ok((s3.apply(ret_ty), s1.compose(&s2).compose(&s3)))
}

fn infer_op_app(
    op: &Spanned<String>,
    lhs: &Spanned<AstExpr>,
    rhs: &Spanned<AstExpr>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let op_expr = AstExpr::ident(&op.0);
    let op_spanned = (op_expr, op.1);

    let partial_app = (AstExpr::fn_app(op_spanned, lhs.clone()), op.1);
    infer_fn_app(&partial_app, rhs, env, ctx)
}

fn infer_if(
    condition: &Spanned<AstExpr>,
    then_branch: &Spanned<AstExpr>,
    else_branch: &Spanned<AstExpr>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let (cond_ty, s1) = infer(&condition.0, env, ctx)?;
    let (then_ty, s2) = infer(&then_branch.0, &env.apply(&s1), ctx)?;
    let (else_ty, s3) = infer(&else_branch.0, &env.apply(&s1.compose(&s2)), ctx)?;

    let s4 = unify(s1.compose(&s2).compose(&s3).apply(cond_ty), Type::Bool)?;

    let s5 = unify(
        s4.apply(s2.compose(&s3).apply(then_ty.clone())),
        s4.apply(s3.apply(else_ty)),
    )?;

    Ok((
        s5.apply(s4.apply(then_ty)),
        s1.compose(&s2).compose(&s3).compose(&s4).compose(&s5),
    ))
}

fn infer_stmt(
    stmt: &AstStmt,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(TypeEnvironment, Subst), TypeError> {
    match &stmt.0 {
        AstStmtNode::Assign { name, body } => {
            let (ty, s) = infer(&body.0, env, ctx)?;
            let new_env = env.extend(name.0.clone(), ty);
            Ok((new_env, s))
        }
        AstStmtNode::FnDef { name, params, body } => {
            let lambda = params.iter().rev().fold(body.clone(), |acc, param| {
                let span = param.1.start..acc.1.end;
                (AstExpr::lambda(param.clone(), acc), span.into())
            });

            let (ty, s) = infer(&lambda.0, env, ctx)?;
            let new_env = env.extend(name.0.clone(), ty);
            Ok((new_env, s))
        }
        AstStmtNode::Expr(expr) => {
            let (_, s) = infer(&expr.0, env, ctx)?;
            Ok((env.clone(), s))
        }
    }
}

fn infer_block(
    statements: &[Spanned<AstStmt>],
    expr: Option<&Spanned<AstExpr>>,
    env: &TypeEnvironment,
    ctx: &mut TypeVarContext,
) -> Result<(Type, Subst), TypeError> {
    let mut env = env.clone();
    let mut subst = Subst::empty();
    for stmt in statements {
        let (new_env, s) = infer_stmt(&stmt.0, &env.apply(&subst), ctx)?;
        env = new_env;
        subst = subst.compose(&s);
    }

    match expr {
        Some(expr) => {
            let (ty, s) = infer(&expr.0, &env.apply(&subst), ctx)?;
            Ok((ty, subst.compose(&s)))
        }
        None => Ok((Type::Unit, subst)),
    }
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

        AstExprNode::OpApp { op, lhs, rhs } => infer_op_app(op, lhs, rhs, env, ctx),

        AstExprNode::If {
            condition,
            then_branch,
            else_branch,
        } => infer_if(condition, then_branch, else_branch, env, ctx),

        AstExprNode::Block { statements, expr } => {
            infer_block(statements, expr.as_deref(), env, ctx)
        }
    }
}

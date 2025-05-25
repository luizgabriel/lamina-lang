use chumsky::span::SimpleSpan;

use crate::{
    lexer::Spanned,
    syntax::{AstExpr, AstStmt},
};

use super::expr::IrExpr;

pub fn lowering_stmt<'src>(stmt: AstStmt<'src>, then: Spanned<IrExpr<'src>>) -> IrExpr<'src> {
    match stmt {
        AstStmt::Expr(expr) => IrExpr::let_binding(("_", expr.1), lowering_expr(expr), then),

        AstStmt::Let { name, body } => IrExpr::let_binding(name, lowering_expr(body), then),

        AstStmt::FnDef { name, args, body } => {
            let curried: Spanned<IrExpr<'_>> = args.into_iter().rfold(
                lowering_expr(body),
                |(body, body_span), (arg, arg_span)| {
                    let final_span = SimpleSpan::from(arg_span.start..body_span.end);
                    (
                        IrExpr::lambda((arg, arg_span), (body, body_span)),
                        final_span,
                    )
                },
            );

            IrExpr::let_binding(name, curried, then)
        }
    }
}

pub fn lowering_expr((expr, span): Spanned<AstExpr<'_>>) -> Spanned<IrExpr<'_>> {
    match expr {
        AstExpr::Ident(name) => (IrExpr::ident(name), span),
        AstExpr::Literal(literal) => (IrExpr::literal(literal), span),
        AstExpr::Tuple(items) => (
            IrExpr::tuple(
                items
                    .into_iter()
                    .map(|item| lowering_expr(item))
                    .collect::<Vec<_>>(),
            ),
            span,
        ),
        AstExpr::FnApp { lhs, rhs } => (
            IrExpr::fn_app(lowering_expr(*lhs), lowering_expr(*rhs)),
            span,
        ),
        AstExpr::Block { statements, expr } => {
            let expr: Spanned<IrExpr<'_>> = expr.map_or(
                (IrExpr::literal(()), SimpleSpan::from(span.end..span.end)),
                |b| lowering_expr(*b),
            );

            statements
                .into_iter()
                .rfold(expr, |acc, (stmt, s)| (lowering_stmt(stmt, acc), s))
        }
        AstExpr::OpApp { op, lhs, rhs } => {
            let op_ident: Spanned<IrExpr<'_>> = (IrExpr::ident(op.0), op.1);
            let op_app_span = SimpleSpan::from(lhs.1.start..op.1.end);

            let lhs = lowering_expr(*lhs);
            let rhs = lowering_expr(*rhs);

            let op_app = (IrExpr::fn_app(op_ident, lhs), op_app_span);
            (IrExpr::fn_app(op_app, rhs), span)
        }
    }
}

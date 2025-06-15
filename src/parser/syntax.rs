use crate::lexer::Spanned;
use smallvec::SmallVec;
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct AstModule {
    pub items: Vec<Spanned<AstStmt>>,
}

impl AstModule {
    pub fn new(items: Vec<Spanned<AstStmt>>) -> Self {
        AstModule { items }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Unit,
    Num(f64),
    Bool(bool),
}

impl From<f64> for Literal {
    fn from(n: f64) -> Self {
        Literal::Num(n)
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Bool(b)
    }
}

impl From<()> for Literal {
    fn from(_: ()) -> Self {
        Literal::Unit
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => write!(f, "()"),
            Literal::Num(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

/// Generic AST statement node that can be parameterized over different expression types
#[derive(Clone, Debug, PartialEq)]
pub enum AstStmt {
    FnDef {
        name: Spanned<String>,
        params: SmallVec<[Spanned<String>; 4]>,
        body: Spanned<AstExpr>,
    },
    Assign {
        name: Spanned<String>,
        body: Spanned<AstExpr>,
    },
    Expr(Spanned<AstExpr>),
}

impl AstStmt {
    pub fn fn_def(
        name: Spanned<String>,
        params: impl IntoIterator<Item = Spanned<String>>,
        body: Spanned<AstExpr>,
    ) -> Self {
        AstStmt::FnDef {
            name,
            params: params.into_iter().collect(),
            body,
        }
    }

    pub fn assign(name: Spanned<String>, body: Spanned<AstExpr>) -> Self {
        AstStmt::Assign { name, body }
    }

    pub fn expr(expr: Spanned<AstExpr>) -> Self {
        AstStmt::Expr(expr)
    }
}

/// Generic AST expression node that can be parameterized over different recursion types
#[derive(Clone, Debug, PartialEq)]
pub enum AstExpr {
    Ident(String),
    Literal(Literal),
    Tuple(Vec<Spanned<AstExpr>>),
    Lambda {
        param: Spanned<String>,
        body: Box<Spanned<AstExpr>>,
    },
    FnApp {
        lhs: Box<Spanned<AstExpr>>,
        rhs: Box<Spanned<AstExpr>>,
    },
    OpApp {
        op: Spanned<String>,
        lhs: Box<Spanned<AstExpr>>,
        rhs: Box<Spanned<AstExpr>>,
    },
    Block {
        statements: Vec<Spanned<AstStmt>>,
        expr: Option<Box<Spanned<AstExpr>>>,
    },
    If {
        condition: Box<Spanned<AstExpr>>,
        then_branch: Box<Spanned<AstExpr>>,
        else_branch: Box<Spanned<AstExpr>>,
    },
}

impl From<f64> for AstExpr {
    fn from(n: f64) -> Self {
        AstExpr::Literal(n.into())
    }
}

impl From<bool> for AstExpr {
    fn from(b: bool) -> Self {
        AstExpr::Literal(b.into())
    }
}

impl From<()> for AstExpr {
    fn from(_: ()) -> Self {
        AstExpr::Literal(().into())
    }
}

impl AstExpr {
    pub fn literal(literal: impl Into<Literal>) -> Self {
        AstExpr::Literal(literal.into())
    }

    pub fn ident(name: impl Into<String>) -> Self {
        AstExpr::Ident(name.into())
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<Self>>) -> Self {
        AstExpr::Tuple(items.into_iter().collect())
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        AstExpr::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn op_app(op: Spanned<String>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        AstExpr::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn block(
        statements: impl IntoIterator<Item = Spanned<AstStmt>>,
        expr: Option<Spanned<Self>>,
    ) -> Self {
        AstExpr::Block {
            statements: statements.into_iter().collect(),
            expr: expr.map(Box::new),
        }
    }

    pub fn if_expr(
        condition: Spanned<Self>,
        then_branch: Spanned<Self>,
        else_branch: Spanned<Self>,
    ) -> Self {
        AstExpr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    pub fn lambda(arg: Spanned<String>, body: Spanned<AstExpr>) -> Self {
        AstExpr::Lambda {
            param: arg,
            body: Box::new(body),
        }
    }
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstExpr::Ident(name) => write!(f, "{}", name),
            AstExpr::Literal(literal) => write!(f, "{}", literal),
            AstExpr::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|a| a.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AstExpr::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            AstExpr::OpApp { op, lhs, rhs } => write!(f, "({} {} {})", lhs.0, op.0, rhs.0),
            AstExpr::Block { statements, expr } => write!(
                f,
                "{{ {}{} }}",
                statements
                    .iter()
                    .map(|s| s.0.to_string())
                    .collect::<Vec<_>>()
                    .join("; "),
                expr.as_ref()
                    .map(|e| format!("; {}", e.0))
                    .unwrap_or_default(),
            ),
            AstExpr::If {
                condition,
                then_branch,
                else_branch,
            } => write!(
                f,
                "if ({}) then ({}) else ({})",
                condition.0, then_branch.0, else_branch.0
            ),
            AstExpr::Lambda { param: arg, body } => write!(f, "{} -> ({})", arg.0, body.0),
        }
    }
}

impl Display for AstStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStmt::FnDef {
                name,
                params: args,
                body,
            } => write!(
                f,
                "fn {} ({}) = {}",
                name.0,
                args.iter()
                    .map(|a| a.0.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                body.0
            ),
            AstStmt::Assign { name, body } => write!(f, "{} = {}", name.0, body.0),
            AstStmt::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

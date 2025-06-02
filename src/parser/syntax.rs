use crate::lexer::Spanned;
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

#[derive(Clone, Debug, PartialEq)]
pub enum AstLiteral {
    Unit,
    Num(f64),
    Bool(bool),
}

impl From<f64> for AstLiteral {
    fn from(n: f64) -> Self {
        AstLiteral::Num(n)
    }
}

impl From<bool> for AstLiteral {
    fn from(b: bool) -> Self {
        AstLiteral::Bool(b)
    }
}

impl From<()> for AstLiteral {
    fn from(_: ()) -> Self {
        AstLiteral::Unit
    }
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

impl Display for AstLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstLiteral::Unit => write!(f, "()"),
            AstLiteral::Num(n) => write!(f, "{}", n),
            AstLiteral::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstStmt {
    FnDef {
        name: Spanned<String>,
        args: Vec<Spanned<String>>,
        body: Spanned<AstExpr>,
    },
    Let {
        name: Spanned<String>,
        body: Spanned<AstExpr>,
    },
    Expr(Spanned<AstExpr>),
}

impl AstStmt {
    pub fn fn_def(
        name: Spanned<String>,
        args: Vec<Spanned<String>>,
        body: Spanned<AstExpr>,
    ) -> Self {
        AstStmt::FnDef { name, args, body }
    }

    pub fn let_def(name: Spanned<String>, body: Spanned<AstExpr>) -> Self {
        AstStmt::Let { name, body }
    }

    pub fn expr(expr: Spanned<AstExpr>) -> Self {
        AstStmt::Expr(expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstExpr {
    Ident(String),
    Literal(AstLiteral),
    Tuple(Vec<Spanned<AstExpr>>),
    Lambda {
        arg: Spanned<String>,
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
        expr: Option<Box<Spanned<Self>>>,
    },
    If {
        condition: Box<Spanned<AstExpr>>,
        then_branch: Box<Spanned<AstExpr>>,
        else_branch: Box<Spanned<AstExpr>>,
    },
}

impl AstExpr {
    pub fn literal(literal: impl Into<AstLiteral>) -> Self {
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
            arg,
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
            AstExpr::Lambda { arg, body } => write!(f, "{} -> ({})", arg.0, body.0),
        }
    }
}

impl Display for AstStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStmt::FnDef { name, args, body } => write!(
                f,
                "fn {} ({}) = {}",
                name.0,
                args.iter()
                    .map(|a| a.0.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                body.0
            ),
            AstStmt::Let { name, body } => write!(f, "let {} = {}", name.0, body.0),
            AstStmt::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstType {
    Unit,
    Var(usize),
    Num,
    Bool,
    Tuple(Vec<AstType>),
    Fn(Box<AstType>, Box<AstType>),
}

impl AstType {
    pub fn var(n: usize) -> Self {
        AstType::Var(n)
    }

    pub fn num() -> Self {
        AstType::Num
    }

    pub fn bool() -> Self {
        AstType::Bool
    }

    pub fn func(arg: AstType, ret: AstType) -> Self {
        AstType::Fn(Box::new(arg), Box::new(ret))
    }
}

impl Display for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::Unit => write!(f, "unit"),
            AstType::Var(n) => write!(f, "{}", n),
            AstType::Num => write!(f, "num"),
            AstType::Bool => write!(f, "bool"),
            AstType::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AstType::Fn(arg, ret) => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl From<&AstLiteral> for AstType {
    fn from(literal: &AstLiteral) -> Self {
        match literal {
            AstLiteral::Unit => AstType::Unit,
            AstLiteral::Num(_) => AstType::Num,
            AstLiteral::Bool(_) => AstType::Bool,
        }
    }
}

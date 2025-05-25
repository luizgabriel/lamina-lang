use crate::lexer::Spanned;
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct AstModule<'src> {
    pub items: Vec<Spanned<AstStmt<'src>>>,
}

impl<'src> AstModule<'src> {
    pub fn new(items: Vec<Spanned<AstStmt<'src>>>) -> Self {
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

impl From<f64> for AstExpr<'_> {
    fn from(n: f64) -> Self {
        AstExpr::Literal(n.into())
    }
}

impl From<bool> for AstExpr<'_> {
    fn from(b: bool) -> Self {
        AstExpr::Literal(b.into())
    }
}

impl From<()> for AstExpr<'_> {
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
pub enum AstStmt<'src> {
    FnDef {
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<AstExpr<'src>>,
    },
    Let {
        name: Spanned<&'src str>,
        body: Spanned<AstExpr<'src>>,
    },
    Expr(Spanned<AstExpr<'src>>),
}

impl<'src> AstStmt<'src> {
    pub fn fn_def(
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<AstExpr<'src>>,
    ) -> Self {
        AstStmt::FnDef { name, args, body }
    }

    pub fn let_def(name: Spanned<&'src str>, body: Spanned<AstExpr<'src>>) -> Self {
        AstStmt::Let { name, body }
    }

    pub fn expr(expr: Spanned<AstExpr<'src>>) -> Self {
        AstStmt::Expr(expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstExpr<'src> {
    Ident(&'src str),
    Literal(AstLiteral),
    Tuple(Vec<Spanned<AstExpr<'src>>>),
    FnApp {
        lhs: Box<Spanned<AstExpr<'src>>>,
        rhs: Box<Spanned<AstExpr<'src>>>,
    },
    OpApp {
        op: Spanned<&'src str>,
        lhs: Box<Spanned<AstExpr<'src>>>,
        rhs: Box<Spanned<AstExpr<'src>>>,
    },
    Block {
        statements: Vec<Spanned<AstStmt<'src>>>,
        expr: Option<Box<Spanned<Self>>>,
    },
}

impl<'src> AstExpr<'src> {
    pub fn literal(literal: impl Into<AstLiteral>) -> Self {
        AstExpr::Literal(literal.into())
    }

    pub fn ident(name: &'src str) -> Self {
        AstExpr::Ident(name)
    }

    pub fn tuple(items: Vec<Spanned<Self>>) -> Self {
        AstExpr::Tuple(items)
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        AstExpr::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn op_app(op: Spanned<&'src str>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        AstExpr::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn block(statements: Vec<Spanned<AstStmt<'src>>>, expr: Option<Spanned<Self>>) -> Self {
        AstExpr::Block {
            statements,
            expr: expr.map(Box::new),
        }
    }
}

impl Display for AstExpr<'_> {
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
        }
    }
}

impl Display for AstStmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStmt::FnDef { name, args, body } => write!(
                f,
                "fn {} ({}) = {}",
                name.0,
                args.iter().map(|a| a.0).collect::<Vec<_>>().join(", "),
                body.0
            ),
            AstStmt::Let { name, body } => write!(f, "let {} = {}", name.0, body.0),
            AstStmt::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstType {
    Var(usize),
    Num,
    Bool,
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
            AstType::Var(n) => write!(f, "{}", n),
            AstType::Num => write!(f, "num"),
            AstType::Bool => write!(f, "bool"),
            AstType::Fn(arg, ret) => write!(f, "({} -> {})", arg, ret),
        }
    }
}

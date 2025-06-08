use smallvec::SmallVec;

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
pub enum AstStmtNode<T> {
    FnDef {
        name: Spanned<String>,
        params: SmallVec<[Spanned<String>; 4]>,
        body: Spanned<T>,
    },
    Let {
        name: Spanned<String>,
        body: Spanned<T>,
    },
    Expr(Spanned<T>),
}

/// Untyped AST statement
#[derive(Clone, Debug, PartialEq)]
pub struct AstStmt(pub AstStmtNode<AstExpr>);

impl AstStmt {
    pub fn fn_def(
        name: Spanned<String>,
        params: impl IntoIterator<Item = Spanned<String>>,
        body: Spanned<AstExpr>,
    ) -> Self {
        AstStmt(AstStmtNode::FnDef {
            name,
            params: params.into_iter().collect(),
            body,
        })
    }

    pub fn let_def(name: Spanned<String>, body: Spanned<AstExpr>) -> Self {
        AstStmt(AstStmtNode::Let { name, body })
    }

    pub fn expr(expr: Spanned<AstExpr>) -> Self {
        AstStmt(AstStmtNode::Expr(expr))
    }
}

/// Generic AST expression node that can be parameterized over different recursion types
#[derive(Clone, Debug, PartialEq)]
pub enum AstExprNode<T: Clone, S: Clone> {
    Ident(String),
    Literal(Literal),
    Tuple(im::Vector<Spanned<T>>),
    Lambda {
        param: Spanned<String>,
        body: Box<Spanned<T>>,
    },
    FnApp {
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    OpApp {
        op: Spanned<String>,
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    Block {
        statements: im::Vector<Spanned<S>>,
        expr: Option<Box<Spanned<T>>>,
    },
    If {
        condition: Box<Spanned<T>>,
        then_branch: Box<Spanned<T>>,
        else_branch: Box<Spanned<T>>,
    },
}

/// Untyped AST expression
#[derive(Clone, Debug, PartialEq)]
pub struct AstExpr(pub AstExprNode<AstExpr, AstStmt>);

impl From<f64> for AstExpr {
    fn from(n: f64) -> Self {
        Self(AstExprNode::Literal(n.into()))
    }
}

impl From<bool> for AstExpr {
    fn from(b: bool) -> Self {
        Self(AstExprNode::Literal(b.into()))
    }
}

impl From<()> for AstExpr {
    fn from(_: ()) -> Self {
        Self(AstExprNode::Literal(().into()))
    }
}

impl AstExpr {
    pub fn literal(literal: impl Into<Literal>) -> Self {
        Self(AstExprNode::Literal(literal.into()))
    }

    pub fn ident(name: impl Into<String>) -> Self {
        Self(AstExprNode::Ident(name.into()))
    }

    pub fn tuple(items: impl IntoIterator<Item = Spanned<Self>>) -> Self {
        Self(AstExprNode::Tuple(items.into_iter().collect()))
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(AstExprNode::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn op_app(op: Spanned<String>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(AstExprNode::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn block(
        statements: impl IntoIterator<Item = Spanned<AstStmt>>,
        expr: Option<Spanned<Self>>,
    ) -> Self {
        Self(AstExprNode::Block {
            statements: statements.into_iter().collect(),
            expr: expr.map(Box::new),
        })
    }

    pub fn if_expr(
        condition: Spanned<Self>,
        then_branch: Spanned<Self>,
        else_branch: Spanned<Self>,
    ) -> Self {
        Self(AstExprNode::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    pub fn lambda(arg: Spanned<String>, body: Spanned<AstExpr>) -> Self {
        Self(AstExprNode::Lambda {
            param: arg,
            body: Box::new(body),
        })
    }
}

impl<T: Display + Clone, S: Display + Clone> Display for AstExprNode<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstExprNode::Ident(name) => write!(f, "{}", name),
            AstExprNode::Literal(literal) => write!(f, "{}", literal),
            AstExprNode::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|a| a.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            AstExprNode::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            AstExprNode::OpApp { op, lhs, rhs } => write!(f, "({} {} {})", lhs.0, op.0, rhs.0),
            AstExprNode::Block { statements, expr } => write!(
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
            AstExprNode::If {
                condition,
                then_branch,
                else_branch,
            } => write!(
                f,
                "if ({}) then ({}) else ({})",
                condition.0, then_branch.0, else_branch.0
            ),
            AstExprNode::Lambda { param: arg, body } => write!(f, "{} -> ({})", arg.0, body.0),
        }
    }
}

impl<T: Display> Display for AstStmtNode<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstStmtNode::FnDef {
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
            AstStmtNode::Let { name, body } => write!(f, "{} = {}", name.0, body.0),
            AstStmtNode::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

impl Display for AstStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for AstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

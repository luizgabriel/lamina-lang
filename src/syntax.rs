use chumsky::span::SimpleSpan;
use std::fmt::Display;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Num(f64),
    Ident(&'src str),
    Op(&'src str),
    Ctrl(char),
    Semi,
    Comma,
    Let,
    In,
    Fn,
    True,
    False,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{}", n),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module<'src> {
    pub items: Vec<Spanned<Stmt<'src>>>,
}

impl<'src> Module<'src> {
    pub fn new(items: Vec<Spanned<Stmt<'src>>>) -> Self {
        Module { items }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl From<f64> for Expr<'_> {
    fn from(n: f64) -> Self {
        Expr::Literal(n.into())
    }
}

impl From<bool> for Expr<'_> {
    fn from(b: bool) -> Self {
        Expr::Literal(b.into())
    }
}

impl From<()> for Expr<'_> {
    fn from(_: ()) -> Self {
        Expr::Literal(().into())
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

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    FnDef {
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<Expr<'src>>,
    },
    Let {
        name: Spanned<&'src str>,
        body: Spanned<Expr<'src>>,
    },
    Expr(Spanned<Expr<'src>>),
}

impl<'src> Stmt<'src> {
    pub fn fn_def(
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<Expr<'src>>,
    ) -> Self {
        Stmt::FnDef { name, args, body }
    }

    pub fn let_def(name: Spanned<&'src str>, body: Spanned<Expr<'src>>) -> Self {
        Stmt::Let { name, body }
    }

    pub fn expr(expr: Spanned<Expr<'src>>) -> Self {
        Stmt::Expr(expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Ident(&'src str),
    Literal(Literal),
    Tuple(Vec<Spanned<Expr<'src>>>),
    FnApp {
        lhs: Box<Spanned<Expr<'src>>>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    OpApp {
        op: Spanned<&'src str>,
        lhs: Box<Spanned<Expr<'src>>>,
        rhs: Box<Spanned<Expr<'src>>>,
    },
    Block {
        statements: Vec<Spanned<Stmt<'src>>>,
        expr: Option<Box<Spanned<Self>>>,
    },
}

impl<'src> Expr<'src> {
    pub fn literal(literal: impl Into<Literal>) -> Self {
        Expr::Literal(literal.into())
    }

    pub fn ident(name: &'src str) -> Self {
        Expr::Ident(name)
    }

    pub fn tuple(items: Vec<Spanned<Self>>) -> Self {
        Expr::Tuple(items)
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Expr::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn op_app(op: Spanned<&'src str>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Expr::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn block(statements: Vec<Spanned<Stmt<'src>>>, expr: Option<Spanned<Self>>) -> Self {
        Expr::Block {
            statements,
            expr: expr.map(Box::new),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::Literal(literal) => write!(f, "{}", literal),
            Expr::Tuple(items) => write!(
                f,
                "({})",
                items
                    .iter()
                    .map(|a| a.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            Expr::OpApp { op, lhs, rhs } => write!(f, "({} {} {})", lhs.0, op.0, rhs.0),
            Expr::Block { statements, expr } => write!(
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

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::FnDef { name, args, body } => write!(
                f,
                "fn {} ({}) = {}",
                name.0,
                args.iter().map(|a| a.0).collect::<Vec<_>>().join(", "),
                body.0
            ),
            Stmt::Let { name, body } => write!(f, "let {} = {}", name.0, body.0),
            Stmt::Expr(expr) => write!(f, "{}", expr.0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Var(usize),
    Num,
    Bool,
    Fn(Box<Ty>, Box<Ty>),
}

impl Ty {
    pub fn var(n: usize) -> Self {
        Ty::Var(n)
    }

    pub fn num() -> Self {
        Ty::Num
    }

    pub fn bool() -> Self {
        Ty::Bool
    }

    pub fn func(arg: Ty, ret: Ty) -> Self {
        Ty::Fn(Box::new(arg), Box::new(ret))
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Var(n) => write!(f, "{}", n),
            Ty::Num => write!(f, "num"),
            Ty::Bool => write!(f, "bool"),
            Ty::Fn(arg, ret) => write!(f, "({} -> {})", arg, ret),
        }
    }
}

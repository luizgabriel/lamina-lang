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
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Fn => write!(f, "fn"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Expr(Spanned<Expr<'src>>),
    Assignment(Spanned<&'src str>, Spanned<Expr<'src>>),
}

impl<'src> Statement<'src> {
    pub fn expr(expr: Spanned<Expr<'src>>) -> Self {
        Statement::Expr(expr)
    }

    pub fn assignment(name: Spanned<&'src str>, rhs: Spanned<Expr<'src>>) -> Self {
        Statement::Assignment(name, rhs)
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{}", expr.0),
            Statement::Assignment(name, rhs) => write!(f, "{} = {}", name.0, rhs.0),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    FnDef {
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Box<Spanned<Expr<'src>>>,
    },
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
        body: Vec<Spanned<Statement<'src>>>,
    },
}

impl<'src> Expr<'src> {
    pub fn var(name: &'src str) -> Self {
        Expr::Var(name)
    }

    pub fn num(n: f64) -> Self {
        Expr::Num(n)
    }

    pub fn bool(b: bool) -> Self {
        Expr::Bool(b)
    }

    pub fn fn_def(
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<Self>,
    ) -> Self {
        Expr::FnDef {
            name,
            args,
            body: Box::new(body),
        }
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

    pub fn block(body: Vec<Spanned<Statement<'src>>>) -> Self {
        Expr::Block { body }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::FnDef { name, args, body } => write!(
                f,
                "fn {} ({}) = {}",
                name.0,
                args.iter().map(|a| a.0).collect::<Vec<_>>().join(", "),
                body.0
            ),
            Expr::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            Expr::OpApp { op, lhs, rhs } => write!(f, "({} {} {})", lhs.0, op.0, rhs.0),
            Expr::Block { body } => write!(
                f,
                "{{ {} }}",
                body.iter()
                    .map(|s| s.0.to_string())
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
        }
    }
}

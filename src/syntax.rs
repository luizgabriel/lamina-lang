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
pub enum SyntaxNode<'src, T> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    Let {
        lhs: Spanned<&'src str>,
        rhs: Box<Spanned<T>>,
        then: Box<Spanned<T>>,
    },
    FnDef {
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Box<Spanned<T>>,
    },
    FnApp {
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    OpApp {
        op: Spanned<&'src str>,
        lhs: Box<Spanned<T>>,
        rhs: Box<Spanned<T>>,
    },
    Block {
        body: Vec<Spanned<T>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxTree<'src>(SyntaxNode<'src, SyntaxTree<'src>>);

impl<'src> SyntaxTree<'src> {
    pub fn var(name: &'src str) -> Self {
        Self(SyntaxNode::Var(name))
    }

    pub fn num(n: f64) -> Self {
        Self(SyntaxNode::Num(n))
    }

    pub fn bool(b: bool) -> Self {
        Self(SyntaxNode::Bool(b))
    }

    pub fn let_expr(lhs: Spanned<&'src str>, rhs: Spanned<Self>, then: Spanned<Self>) -> Self {
        Self(SyntaxNode::Let {
            lhs,
            rhs: Box::new(rhs),
            then: Box::new(then),
        })
    }

    pub fn fn_def(
        name: Spanned<&'src str>,
        args: Vec<Spanned<&'src str>>,
        body: Spanned<Self>,
    ) -> Self {
        Self(SyntaxNode::FnDef {
            name,
            args,
            body: Box::new(body),
        })
    }

    pub fn fn_app(lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(SyntaxNode::FnApp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn op_app(op: Spanned<&'src str>, lhs: Spanned<Self>, rhs: Spanned<Self>) -> Self {
        Self(SyntaxNode::OpApp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn block(body: Vec<Spanned<Self>>) -> Self {
        Self(SyntaxNode::Block { body })
    }
}

impl Display for SyntaxTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            SyntaxNode::Var(name) => write!(f, "{}", name),
            SyntaxNode::Num(n) => write!(f, "{}", n),
            SyntaxNode::Bool(b) => write!(f, "{}", b),
            SyntaxNode::FnApp { lhs, rhs } => write!(f, "({} {})", lhs.0, rhs.0),
            SyntaxNode::OpApp { op, lhs, rhs } => write!(f, "(({}) {} {})", op.0, lhs.0, rhs.0),
            SyntaxNode::FnDef { name, args, body } => write!(
                f,
                "fn {} {} = {}",
                name.0,
                args.iter().map(|a| a.0).collect::<Vec<_>>().join(" "),
                body.0
            ),
            SyntaxNode::Block { body } => write!(
                f,
                "{{ {} }}",
                body.iter()
                    .map(|b| b.0.to_string())
                    .collect::<Vec<_>>()
                    .join("; ")
            ),
            _ => todo!(),
        }
    }
}

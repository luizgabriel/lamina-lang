use std::fmt::Display;

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

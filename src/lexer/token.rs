use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Num(f64),
    Ident(&'src str),
    Op(&'src str),
    OpenCtrl(char),
    CloseCtrl(char),
    Semi,
    VirtualSemi,
    Comma,
    Newline,
    In,
    True,
    False,
    If,
    Then,
    Else,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{}", n),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::OpenCtrl(c) => write!(f, "{}", c),
            Token::CloseCtrl(c) => write!(f, "{}", c),
            Token::Semi => write!(f, ";"),
            Token::VirtualSemi => write!(f, ";<virtual>"),
            Token::Comma => write!(f, ","),
            Token::Newline => write!(f, "\\n"),
            Token::In => write!(f, "in"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
        }
    }
}

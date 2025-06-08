mod error;
mod nodes;
mod syntax;
mod r#type;

pub use error::*;
pub use nodes::*;
pub use syntax::*;
pub use r#type::*;

use chumsky::input::Stream;
use chumsky::prelude::*;

use crate::lexer::{Spanned, lexer};

pub fn parse_stmt(input: &str) -> Result<Spanned<AstStmt>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = statement(expression()).parse(stream).into_result()?;

    Ok(ast)
}

pub fn parse_expr(input: &str) -> Result<Spanned<AstExpr>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = expression().parse(stream).into_result()?;

    Ok(ast)
}

pub fn parse_type(input: &str) -> Result<Spanned<AstType>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = type_expr().parse(stream).into_result()?;

    Ok(ast)
}

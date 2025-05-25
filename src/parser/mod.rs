mod error;
mod nodes;

pub use error::*;
pub use nodes::*;

use chumsky::input::Stream;
use chumsky::prelude::*;

use crate::{
    lexer::{Spanned, lexer},
    syntax::{AstExpr, AstModule, AstStmt},
};

pub fn parse_stmt(input: &str) -> Result<Spanned<AstStmt<'_>>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = statement(expression()).parse(stream).into_result()?;

    Ok(ast)
}

pub fn parse_expr(input: &str) -> Result<Spanned<AstExpr<'_>>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = expression().parse(stream).into_result()?;

    Ok(ast)
}

pub fn parse_module(input: &str) -> Result<Spanned<AstModule<'_>>, ParseError<'_>> {
    let tokens = lexer().parse(input).into_result()?;
    let stream = Stream::from_iter(tokens).map((0..input.len()).into(), |(t, s)| (t, s));
    let ast = module().parse(stream).into_result()?;

    Ok(ast)
}

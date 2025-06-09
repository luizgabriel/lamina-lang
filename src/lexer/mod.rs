mod semicolon;
mod span;
mod token;
mod tokenizer;

use chumsky::prelude::*;
pub use span::*;
pub use token::*;
pub use tokenizer::*;

pub fn lex_input<'src>(
    input: &'src str,
) -> Result<Vec<Spanned<Token<'src>>>, Vec<Rich<'src, char>>> {
    lexer().parse(input).into_result()
}

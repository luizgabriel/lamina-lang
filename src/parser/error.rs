use ariadne::{Color, Label, Report, ReportKind};
use chumsky::error::Rich;
use std::fmt::Display;
use thiserror::Error;

use crate::lexer::Token;

#[derive(Debug, Error)]
pub enum ParseError<'src> {
    #[error("Lexical error: {0:?}")]
    LexError(Vec<Rich<'src, char>>),

    #[error("Parse error: {0:?}")]
    ParseError(Vec<Rich<'src, Token<'src>>>),
}

impl<'src> From<Vec<Rich<'src, char>>> for ParseError<'src> {
    fn from(errors: Vec<Rich<'src, char>>) -> Self {
        ParseError::LexError(errors)
    }
}

impl<'src> From<Vec<Rich<'src, Token<'src>>>> for ParseError<'src> {
    fn from(errors: Vec<Rich<'src, Token<'src>>>) -> Self {
        ParseError::ParseError(errors)
    }
}

impl ParseError<'_> {
    pub fn is_incomplete_input(&self) -> bool {
        match self {
            ParseError::LexError(_) => false,
            ParseError::ParseError(errors) => errors.iter().any(|error| error.found().is_none()),
        }
    }

    pub fn report(&self) -> ErrorReport<'_> {
        match self {
            ParseError::LexError(errors) => errors_to_report(errors),
            ParseError::ParseError(errors) => errors_to_report(errors),
        }
    }
}

pub type ErrorReport<'a> = Report<'a, ((), std::ops::Range<usize>)>;

fn errors_to_report<'a>(errors: &[Rich<'a, impl Display>]) -> ErrorReport<'a> {
    let error = errors.first().expect("Expected at least one error");

    let main_label = Label::new(((), error.span().into_range()))
        .with_message(
            error
                .found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
        )
        .with_color(Color::Red);

    let other_labels = error
        .contexts()
        .map(|(l, s)| (format!("while parsing this {l}"), *s))
        .map(|(message, span)| {
            Label::new(((), span.into_range()))
                .with_message(message)
                .with_color(Color::Red)
        })
        .collect::<Vec<_>>();

    Report::build(ReportKind::Error, ((), error.span().into_range()))
        .with_message(error.to_string())
        .with_label(main_label)
        .with_labels(other_labels)
        .finish()
}

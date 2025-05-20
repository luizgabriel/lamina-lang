use ariadne::{Color, Label, Report, ReportKind};
use chumsky::error::Rich;
use std::fmt::Display;

pub type ErrorReport<'a> = Report<'a, ((), std::ops::Range<usize>)>;

pub fn errors_to_report<'a>(errors: &[Rich<'a, impl Display>]) -> ErrorReport<'a> {
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

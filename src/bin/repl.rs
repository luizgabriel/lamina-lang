use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{Parser, error::Rich};
use lamina_lang::lexer::lexer;
use std::{
    fmt::Display,
    io::{self, Write},
};

fn err_to_report<'a>(e: &Rich<'a, impl Display>) -> Report<'a, ((), std::ops::Range<usize>)> {
    Report::build(ReportKind::Error, ((), e.span().into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(e.to_string())
        .with_label(
            Label::new(((), e.span().into_range()))
                .with_message(e.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
}

fn main() -> anyhow::Result<()> {
    println!("Lamina REPL - Type your code below. Type 'exit' to quit.");
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut input = String::new();
        stdin.read_line(&mut input)?;

        if input.trim() == "exit" {
            return Ok(());
        }

        let (result, errors) = lexer().parse(&input).into_output_errors();

        if let Some(tokens) = result {
            println!("{:#?}", tokens);
        }

        errors
            .iter()
            .try_for_each(|err| err_to_report(err).print(Source::from(&input)))?;
    }
}

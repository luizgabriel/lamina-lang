use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{Parser, error::Rich};
use lamina_lang::lexer::lexer;
use rustyline::DefaultEditor;
use std::fmt::Display;

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

const HISTORY_PATH: &str = ".lamina_history";

fn main() -> anyhow::Result<()> {
    println!("Lamina Lang - REPL");
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(HISTORY_PATH);

    loop {
        let Ok(line) = rl.readline("> ") else {
            break;
        };

        let line = line.trim();
        rl.add_history_entry(line)?;

        let (result, errors) = lexer().parse(line).into_output_errors();
        if let Some(tokens) = result {
            println!("{:#?}", tokens);
        }
        errors
            .iter()
            .try_for_each(|err| err_to_report(err).print(Source::from(line)))?;
    }
    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

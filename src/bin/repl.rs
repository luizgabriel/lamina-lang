use std::fmt::Display;

use ariadne::Source;
use chumsky::error::Rich;
use lamina_lang::{
    core::lowering_stmt,
    error::errors_to_report,
    parser::{ParseError, parse_stmt},
};
use rustyline::DefaultEditor;

const HISTORY_PATH: &str = ".lamina_history";

fn print_errors(errors: &[Rich<'_, impl Display>], input: &str) -> anyhow::Result<()> {
    let source = Source::from(input);
    errors_to_report(errors).print(&source)?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    println!("Lamina Lang - REPL");
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(HISTORY_PATH);

    'outer: loop {
        let mut line = String::new();

        loop {
            let Ok(input) = rl.readline(if line.is_empty() { "> " } else { "... " }) else {
                break 'outer;
            };

            line.push_str(&input);

            match parse_stmt(&line) {
                Ok(ast) => {
                    rl.add_history_entry(line.trim())?;
                    println!("{}", lowering_stmt(ast.0, (().into(), ast.1)));
                    break;
                }
                Err(ParseError::LexError(errors)) => print_errors(&errors, &line)?,
                Err(ParseError::ParseError(errors)) => {
                    let is_incomplete = errors.iter().any(|error| error.found().is_none());
                    if is_incomplete {
                        line.push_str("\n\t");
                        continue;
                    } else {
                        print_errors(&errors, &line)?;
                        line.clear();
                        break;
                    }
                }
            }
        }
    }

    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

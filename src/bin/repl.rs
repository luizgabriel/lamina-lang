use ariadne::Source;
use chumsky::{
    Parser,
    input::{Input, Stream},
};
use lamina_lang::{
    lexer::lexer,
    parser::{SyntaxTree, errors_to_report, parser},
};
use rustyline::DefaultEditor;

const HISTORY_PATH: &str = ".lamina_history";

fn lex_and_parse(line: &str) -> Result<SyntaxTree<'_>, ()> {
    let source = Source::from(line);

    let tokens = lexer()
        .parse(line)
        .into_result()
        .map_err(|errors| errors_to_report(&errors).print(&source).unwrap())?;

    let stream = Stream::from_iter(tokens).map((0..line.len()).into(), |(t, s)| (t, s));

    let ast = parser()
        .parse(stream)
        .into_result()
        .map_err(|errors| errors_to_report(&errors).print(&source).unwrap())?;

    Ok(ast.0)
}

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

        let ast = lex_and_parse(line);
        println!("{:#?}", ast);
    }
    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

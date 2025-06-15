use ariadne::Source;
use lamina_lang::{
    interpreter::{eval, eval_stmt, ValueEnv},
    lexer::lex_input,
    parser::{parse_stmt, AstStmt, ParseError},
    repl::{parse_command, print_env, print_help, print_tokens, ReplCommand, ReplState},
    typecheck::infer as infer_type,
};

const HISTORY_PATH: &str = ".lamina_history";

fn print_errors<'src>(error: ParseError<'src>, input: &'src str) -> anyhow::Result<()> {
    let source = Source::from(input);
    error.report().print(&source)?;
    Ok(())
}

/// For any input without explicit semicolons, add a newline to trigger virtual semicolon insertion
fn prepare_input(input: &str) -> String {
    let input = input.trim();
    if !input.ends_with(';') && !input.starts_with(':') {
        format!("{}\n", input)
    } else {
        input.to_string()
    }
}

fn main() -> anyhow::Result<()> {
    println!("Lamina Lang - REPL (Interpreter Mode)");
    println!("Type :help for available commands.");
    let mut state = ReplState::new()?;
    let _ = state.rl.load_history(HISTORY_PATH);

    'outer: loop {
        let mut line = String::new();

        loop {
            let Ok(input) = state
                .rl
                .readline(if line.is_empty() { "> " } else { "... " })
            else {
                break 'outer;
            };

            line.push_str(&input);
            state.rl.add_history_entry(line.trim())?;

            match parse_command(&input) {
                Ok(ReplCommand::Eval(expr)) => match parse_stmt(&prepare_input(expr)) {
                    Ok((AstStmt::Expr(expr), _)) => match eval(expr.0, &state.env) {
                        Ok(value) => println!("{}", value),
                        Err(err) => println!("Evaluation error: {err}"),
                    },
                    Ok((stmt, _)) => {
                        let new_env = eval_stmt(stmt, &state.env)?;
                        state.env = new_env;
                    }
                    Err(err) => {
                        if err.is_incomplete_input() {
                            line.push_str("\n\t");
                            continue;
                        }

                        print_errors(err, &line)?;
                    }
                },
                Ok(ReplCommand::PrintType(expr)) => match parse_stmt(&prepare_input(expr)) {
                    Ok((AstStmt::Expr(expr), _)) => {
                        let (inferred_type, _) =
                            infer_type(&expr.0, &state.type_env, &mut state.type_ctx)?;
                        println!("{}", inferred_type);
                    }
                    Ok(_) => {
                        eprintln!("Cannot infer type of statements");
                    }
                    Err(err) => {
                        if err.is_incomplete_input() {
                            line.push_str("\n\t");
                            continue;
                        }

                        print_errors(err, &line)?;
                    }
                },
                Ok(ReplCommand::PrintParse(expr)) => match parse_stmt(&prepare_input(expr)) {
                    Ok((stmt, _)) => {
                        println!("{stmt:#?}");
                    }
                    Err(err) => {
                        if err.is_incomplete_input() {
                            line.push_str("\n\t");
                            continue;
                        }

                        print_errors(err, &line)?;
                    }
                },
                Ok(ReplCommand::Tokenize(expr)) => {
                    let input = prepare_input(expr);
                    match lex_input(&input) {
                        Ok(tokens) => print_tokens(&tokens),
                        Err(err) => print_errors(err.into(), &input)?,
                    }
                }
                Ok(ReplCommand::Help) => {
                    print_help();
                }
                Ok(ReplCommand::PrintEnv) => {
                    print_env(&state.env);
                }
                Ok(ReplCommand::ClearEnv) => {
                    state.env = ValueEnv::builtins();
                }
                Ok(ReplCommand::Quit) => {
                    break 'outer;
                }
                Err(err) => {
                    eprintln!("{err}");
                }
            }

            line.clear();
            break;
        }
    }

    state.rl.save_history(HISTORY_PATH)?;
    Ok(())
}

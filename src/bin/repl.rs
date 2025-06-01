use std::fmt::Display;

use ariadne::Source;
use lamina_lang::{
    interpreter::{Environment, InterpreterError, Value, eval, eval_stmt},
    parser::{ParseError, parse_expr, parse_stmt},
};
use rustyline::DefaultEditor;

const HISTORY_PATH: &str = ".lamina_history";

fn print_errors<'src>(error: ParseError<'src>, input: &'src str) -> anyhow::Result<()> {
    let source = Source::from(input);
    error.report().print(&source)?;
    Ok(())
}

fn print_help() {
    println!("Lamina Lang REPL Commands:");
    println!("  :help     - Show this help message");
    println!("  :env      - Show the current environment bindings");
    println!("  :clear    - Clear the interpreter state (environment)");
    println!("  :quit     - Exit the REPL");
    println!();
}

#[derive(Debug)]
enum REPLError<'src> {
    ParseError(ParseError<'src>),
    EvalError(InterpreterError),
}

impl<'src> Display for REPLError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            REPLError::ParseError(err) => write!(f, "Parse error: {err}"),
            REPLError::EvalError(err) => write!(f, "Evaluation error: {err}"),
        }
    }
}

impl<'src> From<ParseError<'src>> for REPLError<'src> {
    fn from(err: ParseError<'src>) -> Self {
        REPLError::ParseError(err)
    }
}

impl<'src> From<InterpreterError> for REPLError<'src> {
    fn from(err: InterpreterError) -> Self {
        REPLError::EvalError(err)
    }
}

enum ValOrEnv {
    Val(Value),
    Env(Environment),
}

fn eval_input<'src>(input: &'src str, env: &'_ Environment) -> Result<ValOrEnv, REPLError<'src>> {
    match parse_expr(input) {
        Ok(stmt) => {
            let value = eval(&stmt.0, env)?;
            Ok(ValOrEnv::Val(value))
        }
        Err(_) => match parse_stmt(input) {
            Ok(stmt) => {
                let env = eval_stmt(&stmt.0, env)?;
                Ok(ValOrEnv::Env(env))
            }
            Err(err) => Err(err.into()),
        },
    }
}

fn handle_repl_command(command: &str, env: &mut Environment) -> bool {
    match command.trim() {
        ":help" | ":h" => {
            print_help();
            true
        }
        ":env" | ":e" => {
            println!("Environment bindings:");
            for (name, value) in env.into_iter().filter(|(_, value)| !value.is_builtin()) {
                println!("  {name} = {value}");
            }
            true
        }
        ":clear" | ":c" => {
            *env = Environment::builtins();
            true
        }
        ":quit" | ":q" => false,
        _ => {
            println!("Unknown command: {command}. Type :help for available commands.");
            true
        }
    }
}

fn main() -> anyhow::Result<()> {
    println!("Lamina Lang - REPL (Interpreter Mode)");
    println!("Type :help for available commands.");
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(HISTORY_PATH);
    let mut env = Environment::builtins();

    'outer: loop {
        let mut line = String::new();

        loop {
            let Ok(input) = rl.readline(if line.is_empty() { "> " } else { "... " }) else {
                break 'outer;
            };

            line.push_str(&input);

            // Handle REPL commands
            if line.trim().starts_with(':') {
                if !handle_repl_command(&line, &mut env) {
                    break 'outer;
                }
                break;
            }

            match eval_input(&line, &env) {
                Ok(ValOrEnv::Val(value)) => {
                    rl.add_history_entry(line.trim())?;
                    println!("{}", value);
                    line.clear();
                    break;
                }
                Ok(ValOrEnv::Env(new_env)) => {
                    rl.add_history_entry(line.trim())?;
                    env = new_env;
                    break;
                }
                Err(err) => match err {
                    REPLError::ParseError(err) => {
                        if err.is_incomplete_input() {
                            line.push_str("\n\t");
                            continue;
                        }

                        print_errors(err, &line)?;
                        line.clear();
                        break;
                    }
                    REPLError::EvalError(err) => {
                        eprintln!("{err}");
                        line.clear();
                        break;
                    }
                },
            }
        }
    }

    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

use ariadne::Source;
use lamina_lang::{
    interpreter::{eval, eval_stmt, Environment},
    parser::{parse_stmt, AstStmt, AstStmtNode, ParseError},
    typecheck::{infer as infer_type, TypeEnvironment, TypeVarContext},
};
use rustyline::DefaultEditor;
use thiserror::Error;

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
    println!("  :ty <expr> - Print the type of the expression");
    println!("  :quit     - Exit the REPL");
    println!();
}

#[derive(Debug, Error)]
enum ParseCommandError {
    #[error("Usage: :ty <expression>")]
    InvalidTypeCommand,

    #[error("Unknown command: {0}. Type :help for available commands.")]
    UnknownCommand(String),
}

enum ReplCommand<'src> {
    Eval(&'src str),
    PrintType(&'src str),
    Help,
    PrintEnv,
    ClearEnv,
    Quit,
}

fn parse_repl_input<'src>(command: &'src str) -> Result<ReplCommand<'src>, ParseCommandError> {
    let trimmed = command.trim();

    if let Some(expr) = trimmed.strip_prefix(":ty") {
        let expr = expr.trim();
        if expr.is_empty() {
            return Err(ParseCommandError::InvalidTypeCommand);
        }

        return Ok(ReplCommand::PrintType(expr));
    }

    if trimmed.starts_with(':') {
        return match trimmed {
            ":help" | ":h" => Ok(ReplCommand::Help),
            ":env" | ":e" => Ok(ReplCommand::PrintEnv),
            ":clear" | ":c" => Ok(ReplCommand::ClearEnv),
            ":quit" | ":q" => Ok(ReplCommand::Quit),
            _ => Err(ParseCommandError::UnknownCommand(command.to_string())),
        };
    }

    Ok(ReplCommand::Eval(trimmed))
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

fn print_env(env: &Environment) {
    println!("Environment bindings:");
    for (name, value) in env.iter().filter(|(_, value)| !value.is_builtin()) {
        println!("  {name} = {value}");
    }
}

struct ReplState {
    rl: DefaultEditor,
    env: Environment,
    type_ctx: TypeVarContext,
    type_env: TypeEnvironment,
}

impl ReplState {
    fn new() -> anyhow::Result<Self> {
        let mut type_ctx = TypeVarContext::default();
        let type_env = TypeEnvironment::builtins(&mut type_ctx);
        Ok(ReplState {
            rl: DefaultEditor::new()?,
            env: Environment::builtins(),
            type_ctx,
            type_env,
        })
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

            match parse_repl_input(&input) {
                Ok(ReplCommand::Eval(expr)) => match parse_stmt(&prepare_input(expr)) {
                    Ok((AstStmt(AstStmtNode::Expr(expr)), _)) => match eval(expr.0, &state.env) {
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
                    Ok((AstStmt(AstStmtNode::Expr(expr)), _)) => {
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
                Ok(ReplCommand::Help) => {
                    print_help();
                }
                Ok(ReplCommand::PrintEnv) => {
                    print_env(&state.env);
                }
                Ok(ReplCommand::ClearEnv) => {
                    state.env = Environment::builtins();
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

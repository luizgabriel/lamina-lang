use std::fmt::Display;

use ariadne::Source;
use chumsky::error::Rich;
use lamina_lang::{
    core::{lowering_expr, lowering_stmt},
    error::errors_to_report,
    parser::{ParseError, parse_expr, parse_stmt},
    vm::{Compiler, VM},
};
use rustyline::DefaultEditor;

const HISTORY_PATH: &str = ".lamina_history";

fn print_errors(errors: &[Rich<'_, impl Display>], input: &str) -> anyhow::Result<()> {
    let source = Source::from(input);
    errors_to_report(errors).print(&source)?;
    Ok(())
}

fn print_help() {
    println!("Lamina Lang REPL Commands:");
    println!("  :help     - Show this help message");
    println!("  :stack    - Show the current VM stack");
    println!("  :env      - Show the current environment bindings");
    println!("  :clear    - Clear the VM state (stack and environment)");
    println!("  :quit     - Exit the REPL");
    println!();
}

fn handle_repl_command(command: &str, vm: &mut VM) -> bool {
    match command.trim() {
        ":help" | ":h" => {
            print_help();
            true
        }
        ":stack" | ":s" => {
            println!("VM Stack (top to bottom):");
            if vm.stack.is_empty() {
                println!("  (empty)");
            } else {
                for (i, value) in vm.stack.iter().rev().enumerate() {
                    println!("  {}: {}", i, value);
                }
            }
            true
        }
        ":env" | ":e" => {
            println!("Environment bindings:");
            for (name, value) in &vm.env.bindings {
                println!("  {} = {}", name, value);
            }
            true
        }
        ":clear" | ":c" => {
            *vm = VM::default();
            println!("VM state cleared.");
            true
        }
        ":quit" | ":q" => false,
        _ => {
            println!(
                "Unknown command: {}. Type :help for available commands.",
                command
            );
            true
        }
    }
}

fn main() -> anyhow::Result<()> {
    println!("Lamina Lang - REPL");
    println!("Type :help for available commands.");
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history(HISTORY_PATH);
    let mut vm = VM::default();

    'outer: loop {
        let mut line = String::new();

        loop {
            let Ok(input) = rl.readline(if line.is_empty() { "> " } else { "... " }) else {
                break 'outer;
            };

            line.push_str(&input);

            // Handle REPL commands
            if line.trim().starts_with(':') {
                rl.add_history_entry(line.trim())?;
                if !handle_repl_command(&line, &mut vm) {
                    break 'outer;
                }
                break;
            }

            // Try to parse as an expression first
            match parse_expr(&line) {
                Ok(ast) => {
                    rl.add_history_entry(line.trim())?;
                    let core_expr = lowering_expr(ast);

                    let mut compiler = Compiler::new();
                    let instructions = compiler.compile(&core_expr);

                    match vm.execute(instructions) {
                        Ok(value) => println!("{}", value),
                        Err(err) => println!("Runtime error: {}", err),
                    }
                    break;
                }
                Err(_) => {
                    // If expression parsing fails, try statement parsing
                    match parse_stmt(&line) {
                        Ok(ast) => {
                            rl.add_history_entry(line.trim())?;
                            let core_stmt = lowering_stmt(ast.0, (().into(), ast.1));

                            let mut compiler = Compiler::new();
                            let instructions = compiler.compile(&(core_stmt, ast.1));

                            if let Err(err) = vm.execute(instructions) {
                                println!("Runtime error: {}", err);
                            }
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
        }
    }

    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

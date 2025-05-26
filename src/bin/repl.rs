use ariadne::Source;
use lamina_lang::{
    parser::ParseError,
    vm::{Compiler, Instruction, VM, VmEnv},
};
use rustyline::DefaultEditor;

const HISTORY_PATH: &str = ".lamina_history";
const MAX_CALL_STACK_DEPTH: usize = 1024;

fn print_errors<'src>(error: ParseError<'src>, input: &'src str) -> anyhow::Result<()> {
    let source = Source::from(input);
    error.report().print(&source)?;
    Ok(())
}

fn print_help() {
    println!("Lamina Lang REPL Commands:");
    println!("  :help     - Show this help message");
    println!("  :stack    - Show the current VM stack");
    println!("  :env      - Show the current environment bindings");
    println!("  :instructions - Show the last compiled instructions");
    println!("  :clear    - Clear the VM state (stack and environment)");
    println!("  :quit     - Exit the REPL");
    println!();
}

fn handle_repl_command(
    command: &str,
    vm: &mut VM,
    last_instructions: &Option<Vec<lamina_lang::vm::Instruction>>,
) -> bool {
    match command.trim() {
        ":help" | ":h" => {
            print_help();
            true
        }
        ":env" | ":e" => {
            println!("Environment bindings:");
            for (name, value) in vm
                .env
                .borrow()
                .into_iter()
                .filter(|(_, value)| !value.is_builtin())
            {
                println!("  {} = {}", name, value);
            }
            true
        }
        ":instructions" | ":i" => {
            println!("Last compiled instructions:");
            if let Some(instrs) = last_instructions {
                for (i, instr) in instrs.iter().enumerate() {
                    println!("  {}: {:?}", i, instr);
                }
            } else {
                println!("  (none)");
            }
            true
        }
        ":clear" | ":c" => {
            *vm = VM::new(MAX_CALL_STACK_DEPTH, VmEnv::builtins());
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
    let mut vm = VM::new(MAX_CALL_STACK_DEPTH, VmEnv::builtins());
    let mut last_instructions: Option<Vec<Instruction>> = None;

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
                if !handle_repl_command(&line, &mut vm, &last_instructions) {
                    break 'outer;
                }
                break;
            }

            let mut compiler = Compiler::new();
            match compiler.compile_input(&line) {
                Ok(instructions) => {
                    rl.add_history_entry(line.trim())?;
                    last_instructions = Some(instructions.clone());
                    match vm.execute(instructions) {
                        Ok(value) if !value.is_unit() => println!("{}", value),
                        Ok(_) => (),
                        Err(err) => println!("{}", err),
                    }
                    break;
                }
                Err(err) => {
                    if err.is_incomplete_input() {
                        line.push_str("\n\t");
                        continue;
                    }

                    print_errors(err, &line)?;
                    line.clear();
                    break;
                }
            }
        }
    }

    rl.save_history(HISTORY_PATH)?;
    Ok(())
}

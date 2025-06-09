use crate::{
    interpreter::Environment,
    lexer::{Spanned, Token},
};

pub fn print_env(env: &Environment) {
    println!("Environment bindings:");
    for (name, value) in env.iter().filter(|(_, value)| !value.is_builtin()) {
        println!("  {name} = {value}");
    }
}

pub fn print_tokens(tokens: &[Spanned<Token>]) {
    println!(
        "{}",
        tokens
            .iter()
            .map(|t| format!("{:?}", t.0))
            .collect::<Vec<_>>()
            .join(" ")
    );
}

pub fn print_help() {
    println!("Lamina Lang REPL Commands:");
    println!("  :help     - Show this help message");
    println!("  :env      - Show the current environment bindings");
    println!("  :clear    - Clear the interpreter state (environment)");
    println!("  :type <expr> - Print the type of the expression");
    println!("  :parse <expr> - Print the type of the expression");
    println!("  :quit     - Exit the REPL");
    println!();
}

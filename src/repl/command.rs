use crate::repl::ParseCommandError;

pub enum ReplCommand<'src> {
    Eval(&'src str),
    PrintType(&'src str),
    PrintParse(&'src str),
    Tokenize(&'src str),
    Help,
    PrintEnv,
    ClearEnv,
    Quit,
}

pub fn parse_command<'src>(input: &'src str) -> Result<ReplCommand<'src>, ParseCommandError> {
    if let Some(expr) = input
        .strip_prefix(":tokenize")
        .or_else(|| input.strip_prefix(":tk"))
    {
        let expr = expr.trim();
        if expr.is_empty() {
            return Err(ParseCommandError::InvalidExpressionCommand(":tokenize"));
        }

        return Ok(ReplCommand::Tokenize(expr));
    }

    if let Some(expr) = input
        .strip_prefix(":parse")
        .or_else(|| input.strip_prefix(":p"))
    {
        let expr = expr.trim();
        if expr.is_empty() {
            return Err(ParseCommandError::InvalidExpressionCommand(":parse"));
        }

        return Ok(ReplCommand::PrintParse(expr));
    }

    if let Some(expr) = input
        .strip_prefix(":type")
        .or_else(|| input.strip_prefix(":ty"))
    {
        let expr = expr.trim();
        if expr.is_empty() {
            return Err(ParseCommandError::InvalidExpressionCommand(":type"));
        }

        return Ok(ReplCommand::PrintType(expr));
    }

    if input.starts_with(':') {
        return match input {
            ":help" | ":h" => Ok(ReplCommand::Help),
            ":env" | ":e" => Ok(ReplCommand::PrintEnv),
            ":clear" | ":c" => Ok(ReplCommand::ClearEnv),
            ":quit" | ":q" => Ok(ReplCommand::Quit),
            _ => Err(ParseCommandError::UnknownCommand(input.to_string())),
        };
    }

    Ok(ReplCommand::Eval(input))
}

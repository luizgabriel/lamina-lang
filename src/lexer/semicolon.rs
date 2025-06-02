use super::{Spanned, Token};

/// Check if a token can end a statement (and thus should have a virtual semicolon after a newline)
fn can_end_statement(token: &Token) -> bool {
    matches!(
        token,
        Token::Ident(_) |     // identifiers
        Token::Num(_) |       // numbers  
        Token::True |         // boolean literals
        Token::False |
        Token::Ctrl(')') |    // closing parentheses
        Token::Ctrl('}') |    // closing braces
        Token::Ctrl(']') // closing brackets
    )
}

/// Check if a token prevents automatic semicolon insertion when it follows a newline
fn prevents_semicolon_insertion(token: &Token) -> bool {
    matches!(
        token,
        Token::Then |         // then keyword in if expressions
        Token::Else |         // else keyword in if expressions
        Token::Op(_) |        // operators (continuation of expression)
        Token::Ctrl(')') |    // closing parentheses
        Token::Ctrl('}') |    // closing braces
        Token::Ctrl(']') |    // closing brackets
        Token::Comma // commas (continuation of list/tuple)
    )
}

/// Update if-expression depth tracking based on the current token
fn update_if_depth(token: &Token, if_depth: &mut usize) {
    match token {
        Token::If => *if_depth += 1,
        Token::Else => *if_depth = if_depth.saturating_sub(1),
        _ => {}
    }
}

/// Check if a virtual semicolon should be inserted at this position
fn should_insert_semicolon(
    prev_token: &Token,
    next_token: Option<&Token>,
    if_depth: usize,
) -> bool {
    // Must have a statement-ending token before the newline
    if !can_end_statement(prev_token) {
        return false;
    }

    // Check if next token prevents insertion
    if let Some(next) = next_token {
        if prevents_semicolon_insertion(next) {
            return false;
        }
    }

    // Don't insert if we're inside an incomplete if expression
    if_depth == 0
}

/// Insert virtual semicolons based on newline positions and context
pub fn insert_virtual_semicolons(tokens: Vec<Spanned<Token>>) -> Vec<Spanned<Token>> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut if_depth = 0;

    for (i, (token, span)) in tokens.iter().enumerate() {
        // Update if-expression depth tracking
        update_if_depth(token, &mut if_depth);

        match token {
            Token::Newline => {
                // Check if we should insert a virtual semicolon
                let prev_token = result.last().map(|(token, _)| token);
                let next_token = tokens.get(i + 1).map(|(token, _)| token);

                if let Some(prev) = prev_token {
                    if should_insert_semicolon(prev, next_token, if_depth) {
                        result.push((Token::VirtualSemi, *span));
                    }
                }
                // Skip the newline token (don't add it to result)
            }
            _ => {
                result.push((token.clone(), *span));
            }
        }
    }

    result
}

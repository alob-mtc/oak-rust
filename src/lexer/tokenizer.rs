use super::token::{lookup_ident, Pos, TokKind, Token};
use crate::error::error::{Error, Result};
use crate::utils::utils::log_debug;
use std::io::{BufRead, BufReader};

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref IDENTIFIER_REGEX: Regex =
        Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").expect("regex Identifiers pattern is valid");
    static ref NUMBER_REGEX: Regex =
        Regex::new(r"^[+-]?\d+(_\d+)*(\.\d+)?$").expect("regex number pattern is valid");
}

pub struct Tokenizer {
    source: Vec<char>,
    index: usize,

    line: usize,
    col: usize,
}

fn tokenize(unbuffered: &mut BufReader<&[u8]>, debug: bool) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();

    // read a complete line
    let mut line = 1;
    'out: for _line in unbuffered.lines() {
        let buf = _line.map_err(|e| Error::System(e.to_string()))?;
        // shebang-style ignored line, keep taking until EOL
        if buf.starts_with("#!") || buf.is_empty() {
            line += 1;
            continue;
        }

        let mut buf_iter = buf.chars().into_iter().enumerate().peekable();
        while let Some((col, ch)) = buf_iter.next() {
            let commit_token = |tok: TokKind, tokens| {
                commit(
                    Token {
                        kind: tok,
                        pos: Pos(line, col + 1),
                    },
                    tokens,
                    debug,
                )
            };

            match ch {
                '\t' | '\r' | ' ' => {}
                ',' => commit_token(TokKind::Comma, &mut tokens),
                '.' => {
                    if let Some((_, '.')) = buf_iter.peek() {
                        buf_iter.next();
                        if let Some((_, '.')) = buf_iter.peek() {
                            buf_iter.next();
                            commit_token(TokKind::Elispe, &mut tokens);
                        }
                    } else {
                        commit_token(TokKind::Dot, &mut tokens);
                    }
                }
                '_' => commit_token(TokKind::Underscore, &mut tokens),
                '(' => commit_token(TokKind::LeftParan, &mut tokens),
                ')' => commit_token(TokKind::RightParan, &mut tokens),
                '[' => commit_token(TokKind::LeftBracket, &mut tokens),
                ']' => commit_token(TokKind::RightBracket, &mut tokens),
                '{' => commit_token(TokKind::LeftBrace, &mut tokens),
                '}' => commit_token(TokKind::RightBrace, &mut tokens),
                ':' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Assign, &mut tokens);
                    } else {
                        commit_token(TokKind::Colon, &mut tokens);
                    }
                }
                '<' => {
                    if let Some((_, '<')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::PushArrow, &mut tokens);
                    } else if let Some((_, '-')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::NonlocalAssign, &mut tokens);
                    } else if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Leq, &mut tokens);
                    } else {
                        commit_token(TokKind::Less, &mut tokens);
                    }
                }
                '?' => commit_token(TokKind::Qmark, &mut tokens),
                '!' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Neq, &mut tokens);
                    } else {
                        commit_token(TokKind::Exclam, &mut tokens);
                    }
                }
                '+' => commit_token(TokKind::Plus, &mut tokens),
                '-' => {
                    if let Some((_, '>')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::BranchArrow, &mut tokens);
                    } else {
                        commit_token(TokKind::Minus, &mut tokens);
                    }
                }
                '/' => {
                    if let Some((_, '/')) = buf_iter.peek() {
                        buf_iter.next();
                        let comment_string = buf_iter
                            .map(|(_, ch)| ch)
                            .collect::<String>()
                            .trim()
                            .to_string();
                        commit_token(TokKind::Comment(comment_string), &mut tokens);
                        continue 'out;
                    } else {
                        commit_token(TokKind::Divide, &mut tokens);
                    }
                }
                '%' => commit_token(TokKind::Modulus, &mut tokens),
                '^' => commit_token(TokKind::Xor, &mut tokens),
                '&' => commit_token(TokKind::And, &mut tokens),
                '|' => {
                    if let Some((_, '>')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::PipeArrow, &mut tokens);
                    } else {
                        commit_token(TokKind::Or, &mut tokens);
                    }
                }
                '>' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Geq, &mut tokens);
                    } else {
                        commit_token(TokKind::Greater, &mut tokens);
                    }
                }
                '=' => commit_token(TokKind::Eq, &mut tokens),
                '\'' => {
                    let mut accumulator = String::new();
                    while let Some((_, ch)) = buf_iter.next() {
                        if ch == '\'' {
                            break;
                        }
                        accumulator.push(ch);
                    }
                    commit_token(TokKind::StringLiteral(accumulator), &mut tokens);
                }
                _ => {
                    let mut entry = String::from(ch);
                    while let Some(&(_, c)) = buf_iter.peek() {
                        let mut tmp_entry = String::from(entry.clone());
                        // support http.Server()
                        if IDENTIFIER_REGEX.is_match(&tmp_entry) && !c.is_ascii_alphabetic() {
                            break;
                        }

                        tmp_entry.push(c.clone());
                        // Break the loop if the next character does not continue a valid number or Identifiers
                        // and is neither a '.' nor a '_'
                        if !NUMBER_REGEX.is_match(&tmp_entry)
                            && !IDENTIFIER_REGEX.is_match(&tmp_entry)
                            && (c != '.' && c != '_')
                        {
                            break;
                        }

                        // Advance the iterator and update 'entry'
                        buf_iter.next();
                        entry = tmp_entry;
                    }

                    // Determine the token type based on the final 'entry' string
                    if NUMBER_REGEX.is_match(&entry) {
                        // Handle numeric token
                        commit(
                            Token {
                                kind: TokKind::NumberLiteral(entry.replace("_", "")),
                                pos: Pos(line, col),
                            },
                            &mut tokens,
                            debug,
                        );
                    } else if IDENTIFIER_REGEX.is_match(&entry) {
                        // Handle Identifiers token
                        commit(
                            Token {
                                kind: lookup_ident(&entry),
                                pos: Pos(line, col),
                            },
                            &mut tokens,
                            debug,
                        )
                    }
                }
            }
        }
        line += 1;
    }
    Ok(tokens)
}

fn commit(tok: Token, tokens: &mut Vec<Token>, debug: bool) {
    if debug {
        log_debug(&format!("-> {} {}\n", tok, tok.pos));
    }
    tokens.push(tok);
}

// tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let source_string: &[u8] = b"
        std := import('std')
        fmt := import('fmt')
        http := import('http')
        
        server := http.Server()
        with server.route('/hello/:name') fn(params) {
            fn(req, end) if req.method {
                'GET' -> end({
                    status: 200
                    body: fmt.format('Hello, {{ 0 }}!'
                        std.default(params.name, 'World'))
                })
                _ -> end(http.MethodNotAllowed)
            }
        }
        
        PORT := 9999
        std.println('server listing on port:', PORT)
        server.start(PORT)
";

        let mut reader = std::io::BufReader::new(source_string);
        let output = tokenize(&mut reader, true)
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokKind>>();

        let expected_output = vec![
            TokKind::Identifiers("std".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("import".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("std".to_string()),
            TokKind::RightParan,
            TokKind::Identifiers("fmt".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("import".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("fmt".to_string()),
            TokKind::RightParan,
            TokKind::Identifiers("http".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("import".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("http".to_string()),
            TokKind::RightParan,
            TokKind::Identifiers("server".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("http".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("Server".to_string()),
            TokKind::LeftParan,
            TokKind::RightParan,
            TokKind::WithKeyword,
            TokKind::Identifiers("server".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("route".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("/hello/:name".to_string()),
            TokKind::RightParan,
            TokKind::FnKeyword,
            TokKind::LeftParan,
            TokKind::Identifiers("params".to_string()),
            TokKind::RightParan,
            TokKind::LeftBrace,
            TokKind::FnKeyword,
            TokKind::LeftParan,
            TokKind::Identifiers("req".to_string()),
            TokKind::Comma,
            TokKind::Identifiers("end".to_string()),
            TokKind::RightParan,
            TokKind::IfKeyword,
            TokKind::Identifiers("req".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("method".to_string()),
            TokKind::LeftBrace,
            TokKind::StringLiteral("GET".to_string()),
            TokKind::BranchArrow,
            TokKind::Identifiers("end".to_string()),
            TokKind::LeftParan,
            TokKind::LeftBrace,
            TokKind::Identifiers("status".to_string()),
            TokKind::Colon,
            TokKind::NumberLiteral("200".to_string()),
            TokKind::Identifiers("body".to_string()),
            TokKind::Colon,
            TokKind::Identifiers("fmt".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("format".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("Hello, {{ 0 }}!".to_string()),
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("default".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("params".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("name".to_string()),
            TokKind::Comma,
            TokKind::StringLiteral("World".to_string()),
            TokKind::RightParan,
            TokKind::RightParan,
            TokKind::RightBrace,
            TokKind::RightParan,
            TokKind::Underscore,
            TokKind::BranchArrow,
            TokKind::Identifiers("end".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("http".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("MethodNotAllowed".to_string()),
            TokKind::RightParan,
            TokKind::RightBrace,
            TokKind::RightBrace,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::Assign,
            TokKind::NumberLiteral("9999".to_string()),
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("println".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("server listing on port:".to_string()),
            TokKind::Comma,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::RightParan,
            TokKind::Identifiers("server".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("start".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::RightParan,
        ];

        assert_eq!(output, expected_output);
    }
}

use super::token::{lookup_ident, Pos, TokKind, Token};
use crate::error::error::{Error, Result};
use crate::utils::utils::log_debug;
use std::io::{BufRead, BufReader};
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref IDENTIFIER_REGEX: Regex =
        Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*\??$").expect("regex Identifiers pattern is valid");
    static ref NUMBER_REGEX: Regex =
        Regex::new(r"^[+-]?\d+(_\d+)*(\.\d+)?$").expect("regex number pattern is valid");
}

pub fn tokenize(unbuffered: &mut BufReader<&[u8]>, debug: bool) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();

    // read a complete line
    let mut line = 1;
    'out: for _line in unbuffered.lines() {
        let buf = _line
            .map_err(|e| Error::System(e.to_string()))?
            .trim()
            .to_string();
        // shebang-style ignored line, keep taking until EOL
        if buf.starts_with("#!") || buf.is_empty() {
            line += 1;
            continue;
        }

        let mut buf_iter = buf.chars().into_iter().enumerate().peekable();
        while let Some((col, ch)) = buf_iter.next() {
            let mut commit_token =
                |tok: TokKind, buf_iter: &mut Peekable<Enumerate<Chars>>, pos: bool| {
                    let temp_tok = tok.clone();
                    commit(
                        Token {
                            kind: tok,
                            pos: if pos {
                                Pos(line, col)
                            } else {
                                Pos(line, col + 1)
                            },
                        },
                        &mut tokens,
                        debug,
                    );

                    // reach end of line
                    if buf_iter.peek().is_none() {
                        // check if ch allows padding
                        if TokKind::allow_padding(&temp_tok) {
                            commit(
                                Token {
                                    kind: TokKind::Padding,
                                    pos: Pos(line, col + 1),
                                },
                                &mut tokens,
                                debug,
                            );
                        }
                    }
                };

            let mut tokenize_num_ident = |ch: char, buf_iter: &mut Peekable<Enumerate<Chars>>| {
                let mut entry = String::from(ch);
                while let Some(&(_, c)) = buf_iter.peek() {
                    let mut tmp_entry = String::from(entry.clone());
                    // support http.Server()
                    if IDENTIFIER_REGEX.is_match(&tmp_entry)
                        && (!c.is_ascii_alphabetic() && c != '?')
                    {
                        break;
                    }

                    tmp_entry.push(c.clone());
                    // Break the loop if the next character does not continue a valid number or Identifiers
                    // and is neither a '.' nor a '_'
                    if !NUMBER_REGEX.is_match(&tmp_entry)
                        && !IDENTIFIER_REGEX.is_match(&tmp_entry)
                        && (c != '.' && c != '_' && c != '?')
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
                    commit_token(
                        TokKind::NumberLiteral(entry.replace("_", "")),
                        buf_iter,
                        true,
                    );
                } else if IDENTIFIER_REGEX.is_match(&entry) {
                    // Handle Identifiers token
                    commit_token(lookup_ident(&entry), buf_iter, false);
                }
            };

            match ch {
                '\t' | '\r' | ' ' => {}
                ',' => commit_token(TokKind::Comma, &mut buf_iter, false),
                '.' => {
                    if let Some((_, '.')) = buf_iter.peek() {
                        buf_iter.next();
                        if let Some((_, '.')) = buf_iter.peek() {
                            buf_iter.next();
                            commit_token(TokKind::Ellipsis, &mut buf_iter, false);
                        }
                    } else {
                        commit_token(TokKind::Dot, &mut buf_iter, false);
                    }
                }
                '_' => {
                    if buf_iter.peek().unwrap().1.is_ascii_alphabetic() {
                        tokenize_num_ident(ch, &mut buf_iter)
                    } else {
                        commit_token(TokKind::Underscore, &mut buf_iter, false);
                    }
                }
                '(' => commit_token(TokKind::LeftParan, &mut buf_iter, false),
                ')' => commit_token(TokKind::RightParan, &mut buf_iter, false),
                '[' => commit_token(TokKind::LeftBracket, &mut buf_iter, false),
                ']' => commit_token(TokKind::RightBracket, &mut buf_iter, false),
                '{' => commit_token(TokKind::LeftBrace, &mut buf_iter, false),
                '}' => commit_token(TokKind::RightBrace, &mut buf_iter, false),
                ':' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Assign, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Colon, &mut buf_iter, false);
                    }
                }
                '<' => {
                    if let Some((_, '<')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::PushArrow, &mut buf_iter, false);
                    } else if let Some((_, '-')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::NonlocalAssign, &mut buf_iter, false);
                    } else if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Leq, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Less, &mut buf_iter, false);
                    }
                }
                '?' => commit_token(TokKind::Qmark, &mut buf_iter, false),
                '!' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Neq, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Exclam, &mut buf_iter, false);
                    }
                }
                '+' => commit_token(TokKind::Plus, &mut buf_iter, false),
                '-' => {
                    if let Some((_, '>')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::BranchArrow, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Minus, &mut buf_iter, false);
                    }
                }
                '*' => commit_token(TokKind::Times, &mut buf_iter, false),
                '/' => {
                    if let Some((_, '/')) = buf_iter.peek() {
                        buf_iter.next();
                        let comment_string = buf_iter
                            .clone()
                            .map(|(_, ch)| {
                                buf_iter.next();
                                ch
                            })
                            .collect::<String>()
                            .trim()
                            .to_string();
                        commit_token(TokKind::Comment(comment_string), &mut buf_iter, false);
                        continue 'out;
                    } else {
                        commit_token(TokKind::Divide, &mut buf_iter, false);
                    }
                }
                '%' => commit_token(TokKind::Modulus, &mut buf_iter, false),
                '^' => commit_token(TokKind::Xor, &mut buf_iter, false),
                '&' => commit_token(TokKind::And, &mut buf_iter, false),
                '|' => {
                    if let Some((_, '>')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::PipeArrow, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Or, &mut buf_iter, false);
                    }
                }
                '>' => {
                    if let Some((_, '=')) = buf_iter.peek() {
                        buf_iter.next();
                        commit_token(TokKind::Geq, &mut buf_iter, false);
                    } else {
                        commit_token(TokKind::Greater, &mut buf_iter, false);
                    }
                }
                '=' => commit_token(TokKind::Eq, &mut buf_iter, false),
                '\'' => {
                    let mut accumulator = String::new();
                    while let Some((_, ch)) = buf_iter.next() {
                        if ch == '\'' {
                            break;
                        }
                        accumulator.push(ch);
                    }
                    commit_token(TokKind::StringLiteral(accumulator), &mut buf_iter, false);
                }
                _ => tokenize_num_ident(ch, &mut buf_iter),
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
    fn test_operator_tokens() {
        let source_string: &[u8] = b"
        + - * / % ^ & | < > = ! : , . ... ( ) [ ] { } ? _ -> <- |> <= >=
        ";
        let mut reader = std::io::BufReader::new(source_string);
        let output = tokenize(&mut reader, true)
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokKind>>();

        let expected_output = vec![
            TokKind::Plus,
            TokKind::Minus,
            TokKind::Times,
            TokKind::Divide,
            TokKind::Modulus,
            TokKind::Xor,
            TokKind::And,
            TokKind::Or,
            TokKind::Less,
            TokKind::Greater,
            TokKind::Eq,
            TokKind::Exclam,
            TokKind::Colon,
            TokKind::Comma,
            TokKind::Dot,
            TokKind::Ellipsis,
            TokKind::LeftParan,
            TokKind::RightParan,
            TokKind::LeftBracket,
            TokKind::RightBracket,
            TokKind::LeftBrace,
            TokKind::RightBrace,
            TokKind::Qmark,
            TokKind::Underscore,
            TokKind::BranchArrow,
            TokKind::NonlocalAssign,
            TokKind::PipeArrow,
            TokKind::Leq,
            TokKind::Geq,
        ];
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_tokenize2() {
        let souce_string: &[u8] = b"
        std := import('std')

        fn fizzbuzz(n) if [n % 3, n % 5] {
            [0, 0] -> 'FizzBuzz'
            [0, _] -> 'Fizz'
            [_, 0] -> 'Buzz'
            _ -> string(n)
        }
        
        std.range(1, 101) |> std.each(fn(n) {
            std.println(fizzbuzz(n))
        })
        ";
        let mut reader = std::io::BufReader::new(souce_string);
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
            TokKind::Padding,
            TokKind::FnKeyword,
            TokKind::Identifiers("fizzbuzz".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("n".to_string()),
            TokKind::RightParan,
            TokKind::IfKeyword,
            TokKind::LeftBracket,
            TokKind::Identifiers("n".to_string()),
            TokKind::Modulus,
            TokKind::NumberLiteral("3".to_string()),
            TokKind::Comma,
            TokKind::Identifiers("n".to_string()),
            TokKind::Modulus,
            TokKind::NumberLiteral("5".to_string()),
            TokKind::RightBracket,
            TokKind::LeftBrace,
            TokKind::LeftBracket,
            TokKind::NumberLiteral("0".to_string()),
            TokKind::Comma,
            TokKind::NumberLiteral("0".to_string()),
            TokKind::RightBracket,
            TokKind::BranchArrow,
            TokKind::StringLiteral("FizzBuzz".to_string()),
            TokKind::Padding,
            TokKind::LeftBracket,
            TokKind::NumberLiteral("0".to_string()),
            TokKind::Comma,
            TokKind::Underscore,
            TokKind::RightBracket,
            TokKind::BranchArrow,
            TokKind::StringLiteral("Fizz".to_string()),
            TokKind::Padding,
            TokKind::LeftBracket,
            TokKind::Underscore,
            TokKind::Comma,
            TokKind::NumberLiteral("0".to_string()),
            TokKind::RightBracket,
            TokKind::BranchArrow,
            TokKind::StringLiteral("Buzz".to_string()),
            TokKind::Padding,
            TokKind::Underscore,
            TokKind::BranchArrow,
            TokKind::Identifiers("string".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("n".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::RightBrace,
            TokKind::Padding,
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("range".to_string()),
            TokKind::LeftParan,
            TokKind::NumberLiteral("1".to_string()),
            TokKind::Comma,
            TokKind::NumberLiteral("101".to_string()),
            TokKind::RightParan,
            TokKind::PipeArrow,
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("each".to_string()),
            TokKind::LeftParan,
            TokKind::FnKeyword,
            TokKind::LeftParan,
            TokKind::Identifiers("n".to_string()),
            TokKind::RightParan,
            TokKind::LeftBrace,
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("println".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("fizzbuzz".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("n".to_string()),
            TokKind::RightParan,
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::RightBrace,
            TokKind::RightParan,
            TokKind::Padding,
        ];
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_tokenize3() {
        let souce_string: &[u8] = b"        
        leapYear? := leap?(year)
        _maybeOpt := _call?(leapYear)
        ";
        let mut reader = std::io::BufReader::new(souce_string);
        let output = tokenize(&mut reader, true)
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokKind>>();
        let expected_output = vec![
            TokKind::Identifiers("leapYear?".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("leap?".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("year".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::Identifiers("_maybeOpt".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("_call?".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("leapYear".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
        ];
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_tokenize() {
        let source_string: &[u8] = b"
        #! Shebang line to be ignored

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
            TokKind::Padding,
            TokKind::Identifiers("fmt".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("import".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("fmt".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::Identifiers("http".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("import".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("http".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::Identifiers("server".to_string()),
            TokKind::Assign,
            TokKind::Identifiers("http".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("Server".to_string()),
            TokKind::LeftParan,
            TokKind::RightParan,
            TokKind::Padding,
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
            TokKind::Padding,
            TokKind::Identifiers("body".to_string()),
            TokKind::Colon,
            TokKind::Identifiers("fmt".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("format".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("Hello, {{ 0 }}!".to_string()),
            TokKind::Padding,
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
            TokKind::Padding,
            TokKind::RightBrace,
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::Underscore,
            TokKind::BranchArrow,
            TokKind::Identifiers("end".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("http".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("MethodNotAllowed".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::RightBrace,
            TokKind::Padding,
            TokKind::RightBrace,
            TokKind::Padding,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::Assign,
            TokKind::NumberLiteral("9999".to_string()),
            TokKind::Padding,
            TokKind::Identifiers("std".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("println".to_string()),
            TokKind::LeftParan,
            TokKind::StringLiteral("server listing on port:".to_string()),
            TokKind::Comma,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
            TokKind::Identifiers("server".to_string()),
            TokKind::Dot,
            TokKind::Identifiers("start".to_string()),
            TokKind::LeftParan,
            TokKind::Identifiers("PORT".to_string()),
            TokKind::RightParan,
            TokKind::Padding,
        ];
        assert_eq!(output, expected_output);
    }
}

use super::token::{Pos, TokKind, Token};

pub struct Tokenizer {
    source: Vec<char>,
    index: usize,

    line: i32,
    col: i32,
}

impl Tokenizer {
    pub fn new(source_string: String) -> Self {
        Self {
            source: source_string.chars().collect(),
            index: 0,
            line: 1,
            col: 0,
        }
    }
}

// utils
impl Tokenizer {
    fn current_pos(&self) -> Pos {
        Pos {
            line: self.line,
            col: self.col,
        }
    }

    fn is_eof(&self) -> bool {
        self.index == self.source.len()
    }

    fn peek(&self) -> char {
        self.source[self.index]
    }

    fn peek_ahead(&self, n: usize) -> char {
        if self.index + n > self.source.len() {
            // In oak, whitespace is insignificant, so we return it as the nothing
            return ' ';
        }
        return self.source[self.index + n];
    }

    fn next(&mut self) -> char {
        let ch = self.source[self.index];

        if self.index < self.source.len() {
            self.index += 1;
        }

        if ch == '\n' {
            self.line += 1;
            self.col = 0;
        }

        ch
    }

    fn back(&mut self) {
        if self.index > 0 {
            self.index -= 1;
        }
        if self.source[self.index] == '\n' {
            self.line -= 1;
            // TODO: reset col correctly -> not obvious way to do this
        } else {
            self.col -= 0;
        }
    }

    fn read_until_char(&mut self, c: char) -> String {
        let mut accumulator = String::new();
        while !self.is_eof() && self.peek() != c {
            accumulator.push(self.next());
        }

        accumulator
    }

    fn read_valid_indentifier(&mut self) -> String {
        let mut accumulator = String::new();
        loop {
            if self.is_eof() {
                break;
            }

            let ch = self.next();
            if ch.is_ascii_lowercase()
                || ch.is_ascii_uppercase()
                || ch == '_'
                || ch == '?'
                || ch == '!'
            {
                accumulator.push(ch);
            } else {
                self.back();
                break;
            }
        }

        accumulator
    }

    fn read_valid_numeral(&mut self) -> String {
        let mut saw_dot = false;
        let mut accumulator = String::new();
        loop {
            if self.is_eof() {
                break;
            }

            let ch = self.next();
            if ch.is_ascii_digit() {
                accumulator.push(ch);
            } else if ch == '.' && !saw_dot {
                saw_dot = true;
                accumulator.push(ch)
            } else {
                self.back();
                break;
            }
        }

        accumulator
    }
}

impl Tokenizer {
    fn next_token(&mut self) -> Token {
        let ch = self.next();

        match ch {
            ',' => Token {
                kind: TokKind::Comma,
                pos: self.current_pos(),
            },
            '.' => {
                if !self.is_eof() && self.peek() == '.' && self.peek_ahead(1) == '.' {
                    let pos = self.current_pos();
                    self.next();
                    self.next();
                    return Token {
                        kind: TokKind::Elispe,
                        pos,
                    };
                }
                Token {
                    kind: TokKind::Dot,
                    pos: self.current_pos(),
                }
            }
            '(' => Token {
                kind: TokKind::LeftParan,
                pos: self.current_pos(),
            },
            ')' => Token {
                kind: TokKind::RightParan,
                pos: self.current_pos(),
            },
            '[' => Token {
                kind: TokKind::LeftBracket,
                pos: self.current_pos(),
            },
            ']' => Token {
                kind: TokKind::RightBracket,
                pos: self.current_pos(),
            },
            '{' => Token {
                kind: TokKind::LeftBrace,
                pos: self.current_pos(),
            },
            '}' => Token {
                kind: TokKind::RightBrace,
                pos: self.current_pos(),
            },
            ':' => {
                if !self.is_eof() && self.peek() == '=' {
                    let pos = self.current_pos();
                    self.next();
                    return Token {
                        kind: TokKind::Assign,
                        pos,
                    };
                }
                Token {
                    kind: TokKind::Colon,
                    pos: self.current_pos(),
                }
            }
            '<' => {
                if !self.is_eof() {
                    let ch = self.peek();
                    if ch == '<' {
                        self.next();
                        return Token {
                            kind: TokKind::PushArrow,
                            pos: self.current_pos(),
                        };
                    } else if ch == '-' {
                        self.next();
                        return Token {
                            kind: TokKind::NonlocalAssign,
                            pos: self.current_pos(),
                        };
                    } else if ch == '=' {
                        self.next();
                        return Token {
                            kind: TokKind::Leq,
                            pos: self.current_pos(),
                        };
                    }
                }
                Token {
                    kind: TokKind::Less,
                    pos: self.current_pos(),
                }
            }
            '?' => Token {
                kind: TokKind::Qmark,
                pos: self.current_pos(),
            },
            '!' => {
                if !self.is_eof() && self.peek() == '=' {
                    self.next();
                    return Token {
                        kind: TokKind::Neq,
                        pos: self.current_pos(),
                    };
                }
                Token {
                    kind: TokKind::Exclam,
                    pos: self.current_pos(),
                }
            }
            '+' => Token {
                kind: TokKind::Plus,
                pos: self.current_pos(),
            },
            '-' => {
                if !self.is_eof() && self.peek() == '>' {
                    self.next();
                    return Token {
                        kind: TokKind::BranchArrow,
                        pos: self.current_pos(),
                    };
                }
                Token {
                    kind: TokKind::Minus,
                    pos: self.current_pos(),
                }
            }
            '*' => Token {
                kind: TokKind::Times,
                pos: self.current_pos(),
            },
            '/' => {
                if !self.is_eof() && self.peek() == '/' {
                    let pos = self.current_pos();
                    self.next();
                    let comment_string = self.read_until_char('\n');
                    return Token {
                        kind: TokKind::Comment(comment_string),
                        pos,
                    };
                }
                Token {
                    kind: TokKind::Divide,
                    pos: self.current_pos(),
                }
            }
            '%' => Token {
                kind: TokKind::Modulus,
                pos: self.current_pos(),
            },
            '^' => Token {
                kind: TokKind::Xor,
                pos: self.current_pos(),
            },
            '&' => Token {
                kind: TokKind::And,
                pos: self.current_pos(),
            },
            '|' => {
                if !self.is_eof() && self.peek() == '>' {
                    self.next();
                    return Token {
                        kind: TokKind::PipeArrow,
                        pos: self.current_pos(),
                    };
                }
                Token {
                    kind: TokKind::Or,
                    pos: self.current_pos(),
                }
            }
            '>' => {
                if !self.is_eof() && self.peek() == '=' {
                    self.next();
                    return Token {
                        kind: TokKind::Geq,
                        pos: self.current_pos(),
                    };
                }
                Token {
                    kind: TokKind::Greater,
                    pos: self.current_pos(),
                }
            }
            '=' => Token {
                kind: TokKind::Eq,
                pos: self.current_pos(),
            },
            '\'' => {
                let pos = self.current_pos();
                let mut accumulator = String::new();
                while !self.is_eof() && self.peek() != '\'' {
                    let mut ch = self.next();
                    if ch == '\\' {
                        accumulator.push(ch);
                        if self.is_eof() {
                            break;
                        } else {
                            ch = self.next();
                        }
                    }
                    accumulator.push(ch);
                }
                if self.is_eof() {
                    return Token {
                        kind: TokKind::StringLiteral(accumulator),
                        pos,
                    };
                }

                self.next(); // read ending quote
                return Token {
                    kind: TokKind::StringLiteral(accumulator),
                    pos,
                };
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let pos = self.current_pos();
                let mut payload = ch.to_string();
                payload.push_str(&self.read_valid_numeral());
                Token {
                    kind: TokKind::NumberLiteral(payload),
                    pos,
                }
            }
            _ => {
                let pos = self.current_pos();
                let mut payload = ch.to_string();
                payload.push_str(&self.read_valid_indentifier());
                match &payload[..] {
                    "_" => Token {
                        kind: TokKind::Underscore,
                        pos,
                    },
                    "if" => Token {
                        kind: TokKind::IfKeyword,
                        pos,
                    },
                    "fn" => Token {
                        kind: TokKind::FnKeyword,
                        pos,
                    },
                    "with" => Token {
                        kind: TokKind::WithKeyword,
                        pos,
                    },
                    "true" => Token {
                        kind: TokKind::TrueLiteral,
                        pos,
                    },
                    "false" => Token {
                        kind: TokKind::FalseLiteral,
                        pos,
                    },
                    _ => Token {
                        kind: TokKind::Identifiers(payload),
                        pos,
                    },
                }
            }
        }
    }
    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        if !self.is_eof() && self.peek() == '#' && self.peek_ahead(1) == '!' {
            // shebang-stile ignored line, keep takeing untill EOL
            self.read_until_char('\n');
            if !self.is_eof() {
                self.next();
            }
        }

        // snip whitespace before
        while !self.is_eof() && self.peek().is_ascii_whitespace() {
            self.next();
        }

        let mut last = Token {
            kind: TokKind::Comma,
            pos: self.current_pos(),
        };
        while !self.is_eof() {
            let mut next = self.next_token();

            if (last.kind != TokKind::LeftParan
                && last.kind != TokKind::LeftBracket
                && last.kind != TokKind::LeftBrace
                && last.kind != TokKind::Comma)
                && (next.kind == TokKind::RightParan
                    || next.kind == TokKind::RightBracket
                    || next.kind == TokKind::RightBrace)
            {
                tokens.push(Token {
                    kind: TokKind::Comma,
                    pos: self.current_pos(),
                })
            }

            match &next.kind {
                TokKind::Comment(_) => {
                    next.kind = last.kind.clone();
                    next.pos = last.pos.clone()
                }
                _ => tokens.push(next.clone()),
            }

            // snip whitespace after
            while !self.is_eof() && self.peek().is_ascii_whitespace() {
                if self.peek() == '\n' {
                    match &next.kind {
                        TokKind::Comment(_)
                        | TokKind::Comma
                        | TokKind::Dot
                        | TokKind::LeftParan
                        | TokKind::RightParan
                        | TokKind::LeftBracket
                        | TokKind::RightBracket
                        | TokKind::LeftBrace
                        | TokKind::RightBrace
                        | TokKind::Assign
                        | TokKind::NonlocalAssign
                        | TokKind::PipeArrow
                        | TokKind::BranchArrow
                        | TokKind::PushArrow
                        | TokKind::Colon
                        | TokKind::Elispe
                        | TokKind::Qmark
                        | TokKind::Exclam
                        | TokKind::Plus
                        | TokKind::Minus
                        | TokKind::Times
                        | TokKind::Divide
                        | TokKind::Modulus
                        | TokKind::Xor
                        | TokKind::And
                        | TokKind::Or
                        | TokKind::Greater
                        | TokKind::Less
                        | TokKind::Eq
                        | TokKind::Geq
                        | TokKind::Leq
                        | TokKind::Neq
                        | TokKind::IfKeyword
                        | TokKind::FnKeyword
                        | TokKind::WithKeyword
                        | TokKind::Underscore
                        | TokKind::Identifiers(_)
                        | TokKind::TrueLiteral
                        | TokKind::FalseLiteral
                        | TokKind::StringLiteral(_)
                        | TokKind::NumberLiteral(_) => {}
                        _ => {
                            next = Token {
                                kind: TokKind::Comma,
                                pos: self.current_pos(),
                            }
                        }
                    }
                }
                self.next();
            }

            match &next.kind {
                TokKind::Comment(_) => {}
                _ => last = next,
            }
        }

        match &last.kind {
            TokKind::Comment(_) => {}
            _ => tokens.push(Token {
                kind: TokKind::Comma,
                pos: self.current_pos(),
            }),
        }

        tokens
    }
}

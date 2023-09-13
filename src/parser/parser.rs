use std::process::id;

use crate::{
    error::{Err as AppErr, ErrorReason, Result},
    lexer::token::{Pos, TokKind, Token},
    parser::ast::{AssignmentNode, FloatNode, IntNode, NullNode, ObjectEntry, ObjectNode},
};

use super::ast::{
    AstAny, AstNode, AtomNode, BlockNode, BoolNode, EmptyNode, FnNode, IdentifierNode, ListNode,
    StringNode, UnaryNode,
};

struct Parser {
    tokens: Vec<Token>,
    index: i32,
    min_binary_prec: Vec<i32>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            min_binary_prec: Vec::new(),
        }
    }
}

// utils
impl Parser {
    fn last_min_prec(&self) -> i32 {
        self.min_binary_prec[self.min_binary_prec.len() - 1]
    }

    fn push_min_prec(&mut self, prec: i32) {
        self.min_binary_prec.push(prec)
    }

    fn pop_min_prec(&mut self) {
        self.min_binary_prec.pop().unwrap();
    }

    fn is_eof(&self) -> bool {
        self.index == self.tokens.len() as i32
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.index as usize).unwrap()
    }

    fn peek_ahead(&self, n: i32) -> Option<&Token> {
        let index = (self.index + n) as usize;
        if index > self.tokens.len() {
            // use comma as "nothing is here" value
            return None;
        };

        self.tokens.get(index)
    }

    fn next(&mut self) -> &Token {
        let index = self.index as usize;
        if index < self.tokens.len() {
            self.index += 1;
        }

        self.tokens.get(index).unwrap()
    }

    fn back(&mut self) {
        if self.index > 0 {
            self.index -= 1
        }
    }

    fn expect(&mut self, kind: TokKind) -> Result<&Token> {
        if self.is_eof() {
            return Err(AppErr {
                reason: ErrorReason::Syntax,
                message: format!("unexpted end of input, expected {kind}"),
                pos: Pos::default(),
            });
        }

        let next = self.next();

        let valid = match &kind {
            TokKind::Comment(_) => matches!(&next.kind, TokKind::Comment(_)),
            TokKind::Identifiers(_) => matches!(&next.kind, TokKind::Identifiers(_)),
            TokKind::StringLiteral(_) => matches!(&next.kind, TokKind::StringLiteral(_)),
            TokKind::NumberLiteral(_) => matches!(&next.kind, TokKind::NumberLiteral(_)),
            _ => next.kind == kind,
        };

        if !valid {
            return Err(AppErr {
                reason: ErrorReason::Syntax,
                message: format!("unexpted token {next}, expected {kind}"),
                pos: next.pos.clone(),
            });
        }

        Ok(next)
    }

    fn read_until_token_kind(&mut self, kind: TokKind) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.is_eof() && self.peek().kind != kind {
            tokens.push(self.next().clone());
        }
        tokens
    }

    fn infix_op_precedence(op: TokKind) -> i32 {
        match op {
            TokKind::Plus | TokKind::Minus => 40,
            TokKind::Times | TokKind::Divide => 50,
            TokKind::Modulus => 80,
            TokKind::Eq
            | TokKind::Greater
            | TokKind::Less
            | TokKind::Geq
            | TokKind::Leq
            | TokKind::Neq => 30,
            TokKind::And => 20,
            TokKind::Xor => 15,
            TokKind::Or => 10,
            TokKind::PushArrow => 1, // assignment-like semantics
            _ => -1,
        }
    }
}

// concrete AstNode parse functions
impl Parser {
    fn parse_assignment(&mut self, left: Box<dyn AstNode>) -> Result<Box<dyn AstNode>> {
        if self.peek().kind != TokKind::Assign && self.peek().kind != TokKind::NonlocalAssign {
            return Ok(left);
        }

        let next = self.next().clone();
        let node = AssignmentNode {
            is_local: next.kind == TokKind::Assign,
            left: Some(left),
            right: None,
            tok: Some(next),
        };

        // TODO: generate right node

        Ok(Box::new(node))
    }

    // parseUnit is responsible for parsing the smallest complete syntactic "units"
    // of Oak's syntax, like literals including function literals, grouped
    // expressions in blocks, and if/with expressions.
    fn parse_unit(&mut self) -> Result<Box<dyn AstNode>> {
        let tok = self.next().clone();
        match &tok.kind {
            TokKind::Qmark => Ok(Box::new(NullNode { tok })),
            TokKind::StringLiteral(payload) => {
                let mut payload_builder = Vec::new();
                let mut chars = payload.chars().peekable();

                while let Some(c) = chars.peek() {
                    match c {
                        '\\' => {
                            chars.next(); // consume the backslash

                            match chars.next() {
                                Some('t') => payload_builder.push(b'\t'),
                                Some('n') => payload_builder.push(b'\n'),
                                Some('r') => payload_builder.push(b'\r'),
                                Some('f') => payload_builder.push(0x0C),
                                Some('x') => {
                                    let hex_code = chars
                                        .next()
                                        .and_then(|high| chars.next().map(|low| (high, low)))
                                        .and_then(|(high, low)| {
                                            u8::from_str_radix(&format!("{high}{low}"), 16).ok()
                                        });

                                    match hex_code {
                                        Some(code) => payload_builder.push(code),
                                        None => payload_builder.push(b'x'),
                                    }
                                }
                                Some(c) => payload_builder.extend(c.to_string().as_bytes()),
                                _ => break,
                            }
                        }
                        _ => {
                            payload_builder.extend(c.to_string().as_bytes());
                            chars.next();
                        }
                    }
                }

                Ok(Box::new(StringNode {
                    payload: payload_builder,
                    tok: Some(tok),
                }))
            }

            TokKind::NumberLiteral(payload) => {
                if payload.contains(".") {
                    match payload.parse::<f64>() {
                        Ok(f) => {
                            return Ok(Box::new(FloatNode { payload: f, tok }));
                        }
                        Err(err) => {
                            return Err(AppErr {
                                reason: ErrorReason::System,
                                message: err.to_string(),
                                pos: tok.pos,
                            })
                        }
                    };
                }

                match payload.parse::<i64>() {
                    Ok(i) => return Ok(Box::new(IntNode { payload: i, tok })),
                    Err(err) => {
                        return Err(AppErr {
                            reason: ErrorReason::System,
                            message: err.to_string(),
                            pos: tok.pos,
                        })
                    }
                }
            }
            TokKind::TrueLiteral => Ok(Box::new(BoolNode { payload: true, tok })),
            TokKind::FalseLiteral => Ok(Box::new(BoolNode {
                payload: false,
                tok,
            })),
            TokKind::Colon => match &self.peek().kind {
                TokKind::Identifiers(_) => {
                    return Ok(Box::new(AtomNode {
                        payload: self.next().kind.to_string(),
                        tok,
                    }))
                }
                TokKind::IfKeyword => {
                    self.next();
                    return Ok(Box::new(AtomNode {
                        payload: "if".to_string(),
                        tok,
                    }));
                }
                TokKind::FnKeyword => {
                    self.next();
                    return Ok(Box::new(AtomNode {
                        payload: "fn".to_string(),
                        tok,
                    }));
                }
                TokKind::WithKeyword => {
                    self.next();
                    return Ok(Box::new(AtomNode {
                        payload: "with".to_string(),
                        tok,
                    }));
                }
                TokKind::TrueLiteral => {
                    self.next();
                    return Ok(Box::new(AtomNode {
                        payload: "true".to_string(),
                        tok,
                    }));
                }
                TokKind::FalseLiteral => {
                    self.next();
                    return Ok(Box::new(AtomNode {
                        payload: "false".to_string(),
                        tok,
                    }));
                }
                _ => Err(AppErr {
                    reason: ErrorReason::Syntax,
                    message: format!("expected identifier after ':', got {}", self.peek()),
                    pos: tok.pos,
                }),
            },
            TokKind::LeftBracket => {
                self.push_min_prec(0);
                let _ = crate::parser::ScopeCall {
                    c: Some(|| -> () { self.pop_min_prec() }),
                };

                let mut item_nodes = Vec::new();
                while !self.is_eof() && self.peek().kind != TokKind::RightBracket {
                    let node = self.parse_node()?;
                    self.expect(TokKind::Comma)?;

                    item_nodes.push(node);
                }

                self.expect(TokKind::RightBracket)?;

                Ok(Box::new(ListNode {
                    elems: item_nodes,
                    tok,
                }))
            }

            TokKind::LeftBrace => {
                self.push_min_prec(0);
                let _ = crate::parser::ScopeCall {
                    c: Some(|| -> () { self.pop_min_prec() }),
                };

                // empty {} is always considerd an object -- an empty block is illegal
                if self.peek().kind == TokKind::RightBrace {
                    self.next(); // eat the right brace
                    return Ok(Box::new(ObjectNode {
                        entries: Vec::new(),
                        tok,
                    }));
                }

                let first_expr = self.parse_node()?;
                if self.is_eof() {
                    return Err(AppErr {
                        reason: ErrorReason::Syntax,
                        message: "unexpected end of input inside block or object".to_string(),
                        pos: tok.pos,
                    });
                }

                if self.peek().kind == TokKind::Colon {
                    // it's am object
                    self.next(); // eat the coln
                    let val_expr = self.parse_node()?;
                    self.expect(TokKind::Comma)?;
                    let mut entries = vec![ObjectEntry {
                        key: first_expr,
                        val: val_expr,
                    }];

                    while !self.is_eof() && self.peek().kind != TokKind::RightBrace {
                        let key = self.parse_node()?;
                        self.expect(TokKind::Colon)?;
                        let val = self.parse_node()?;
                        self.expect(TokKind::Colon)?;
                        entries.push(ObjectEntry { key, val });
                    }
                    self.expect(TokKind::RightBrace)?;
                    return Ok(Box::new(ObjectNode { entries, tok }));
                }

                // it's a block
                let mut exprs = vec![first_expr];
                self.expect(TokKind::Comma)?;

                while !self.is_eof() && self.peek().kind != TokKind::RightBrace {
                    let expr = self.parse_node()?;
                    self.expect(TokKind::Comma)?;
                    exprs.push(expr)
                }

                self.expect(TokKind::RightBrace)?;
                return Ok(Box::new(BlockNode { exprs, tok }));
            }
            TokKind::FnKeyword => {
                self.push_min_prec(0);
                let _ = crate::parser::ScopeCall {
                    c: Some(|| -> () { self.pop_min_prec() }),
                };

                let mut name = String::new();
                match &self.peek().kind {
                    TokKind::Identifiers(payload) => {
                        // optional name fn
                        name = payload.clone();
                        self.next();
                    }
                    _ => {}
                }

                let mut args = Vec::new();
                let mut rest_arg = String::new();
                if self.peek().kind == TokKind::LeftParan {
                    // optional argument list
                    self.next(); // eat the leftParen
                    while !self.is_eof() && self.peek().kind != TokKind::RightParan {
                        match self.expect(TokKind::Identifiers("".to_string())) {
                            Ok(arg) => {
                                let new_arg = arg.clone();
                                // maybe this is a rest arg
                                if self.peek().kind == TokKind::Elispe {
                                    match new_arg.kind {
                                        TokKind::Identifiers(payload) => {
                                            rest_arg = payload.clone();
                                        }
                                        _ => {}
                                    }
                                    self.expect(TokKind::Comma)?;
                                    break;
                                }

                                args.push(rest_arg.clone());
                                self.expect(TokKind::Comma)?;
                            }
                            Err(_) => {
                                self.back(); // try again
                                self.expect(TokKind::Underscore)?;

                                args.push("".to_string());
                                self.expect(TokKind::Comma)?;
                                continue;
                            }
                        }
                    }
                    self.expect(TokKind::RightParan)?;
                }

                let mut body = self.parse_node()?;

                // Exception to the "{} is empty object" rule is that `fn {}` parses as
                // a function with an empty block as a bod
                if let Some(obj_body) = body
                    .as_any()
                    .downcast_ref::<ObjectNode>()
                    .map(|obj_body| (obj_body.entries.len() == 0).then_some(obj_body))
                    .unwrap()
                {
                    body = Box::new(BlockNode {
                        exprs: Vec::new(),
                        tok: obj_body.tok.clone(),
                    })
                }

                Ok(Box::new(FnNode {
                    name,
                    args,
                    rest_arg,
                    body,
                    tok,
                }))
            }
            TokKind::Underscore => Ok(Box::new(EmptyNode { tok })),
            TokKind::Identifiers(payload) => Ok(Box::new(IdentifierNode {
                payload: payload.clone(),
                tok,
            })),
            TokKind::Minus | TokKind::Exclam => {
                let right = self.parse_sub_node()?;

                Ok(Box::new(UnaryNode { right, tok }))
            }
            _ => todo!(),
        }
    }

    fn parse_sub_node(&mut self) -> Result<Box<dyn AstNode>> {
        todo!()
    }

    fn parse_node(&mut self) -> Result<Box<dyn AstNode>> {
        todo!()
    }

    fn parse(mut self) -> Result<Vec<Box<dyn AstNode>>> {
        let mut nodes = Vec::new();

        while self.is_eof() {
            let node = self.parse_node()?;
            if self.expect(TokKind::Comma).is_err() {
                return Ok(nodes);
            }

            nodes.push(node);
        }

        Ok(nodes)
    }
}

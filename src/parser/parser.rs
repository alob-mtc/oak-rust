use std::rc::Rc;

use crate::parser::ast::Node;
use crate::{
    error::error::{Error, Result},
    lexer::token::{Pos, TokKind, Token},
};

pub struct Parser {
    tokens: Vec<Token>,
    nodes: Vec<Node>,
    index: i32,
    min_binary_prec: Vec<i32>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            nodes: Vec::new(),
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

    pub fn pop_min_prec(&mut self) {
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

    fn handle_pop_case(&mut self, node: Result<Node>) -> Result<Node> {
        node.and_then(|node| -> Result<Node> {
            self.pop_min_prec();
            Ok(node)
        })
        .or_else(|err| -> Result<Node> {
            self.pop_min_prec();
            Err(err)
        })
    }

    fn expect(&mut self, kind: TokKind) -> Result<&Token> {
        if self.is_eof() {
            return Err(Error::Syntax(format!(
                "unexpted end of input, expected {kind}"
            )));
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
            return Err(Error::Syntax(format!(
                "unexpted token {next}, expected {kind}"
            )));
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

    fn infix_op_precedence(op: &TokKind) -> i32 {
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

impl Parser {
    // parseUnit is responsible for parsing the smallest complete syntactic "units"
    // of Oak's syntax, like literals including function literals, grouped
    // expressions in blocks, and if/with expressions.
    fn parse_unit(&mut self) -> Result<Node> {
        let tok = self.next().clone();
        match &tok.kind {
            TokKind::Qmark => Ok(Node::NullNode { tok }),
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

                Ok(Node::StringNode {
                    payload: payload_builder,
                    tok,
                })
            }

            TokKind::NumberLiteral(payload) => {
                if payload.contains(".") {
                    return match payload.parse::<f64>() {
                        Ok(f) => Ok(Node::FloatNode { payload: f, tok }),
                        Err(err) => Err(Error::System(err.to_string())),
                    };
                }

                match payload.parse::<i64>() {
                    Ok(i) => return Ok(Node::IntNode { payload: i, tok }),
                    Err(err) => Err(Error::System(err.to_string())),
                }
            }
            TokKind::TrueLiteral => Ok(Node::BoolNode { payload: true, tok }),
            TokKind::FalseLiteral => Ok(Node::BoolNode {
                payload: false,
                tok,
            }),
            TokKind::Colon => match &self.peek().kind {
                TokKind::Identifiers(_) => {
                    return Ok(Node::AtomNode {
                        payload: self.next().kind.to_string(),
                        tok,
                    })
                }
                TokKind::IfKeyword => {
                    self.next();
                    return Ok(Node::AtomNode {
                        payload: "if".to_string(),
                        tok,
                    });
                }
                TokKind::FnKeyword => {
                    self.next();
                    return Ok(Node::AtomNode {
                        payload: "fn".to_string(),
                        tok,
                    });
                }
                TokKind::WithKeyword => {
                    self.next();
                    return Ok(Node::AtomNode {
                        payload: "with".to_string(),
                        tok,
                    });
                }
                TokKind::TrueLiteral => {
                    self.next();
                    return Ok(Node::AtomNode {
                        payload: "true".to_string(),
                        tok,
                    });
                }
                TokKind::FalseLiteral => {
                    self.next();
                    return Ok(Node::AtomNode {
                        payload: "false".to_string(),
                        tok,
                    });
                }
                _ => Err(Error::Syntax(format!(
                    "expected identifier after ':', got {}",
                    self.peek()
                ))),
            },
            TokKind::LeftBracket => {
                let scope = || -> Result<Node> {
                    self.push_min_prec(0);

                    let mut item_nodes = Vec::new();
                    while !self.is_eof() && self.peek().kind != TokKind::RightBracket {
                        let node = self.parse_node()?;
                        self.expect(TokKind::Comma)?;
                        item_nodes.push(node);
                    }

                    self.expect(TokKind::RightBracket)?;

                    Ok(Node::ListNode {
                        elems: item_nodes,
                        tok,
                    })
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }

            TokKind::LeftBrace => {
                let scope = || -> Result<Node> {
                    self.push_min_prec(0);
                    // empty {} is always considerd an object -- an empty block is illegal
                    if self.peek().kind == TokKind::RightBrace {
                        self.next(); // eat the right brace
                        return Ok(Node::ObjectNode {
                            entries: Vec::new(),
                            tok,
                        });
                    }

                    let first_expr = self.parse_node()?;
                    if self.is_eof() {
                        return Err(Error::Syntax(
                            "unexpected end of input inside block or object".to_string(),
                        ));
                    }

                    if self.peek().kind == TokKind::Colon {
                        // it's an object
                        self.next(); // eat the coln
                        let val_expr = self.parse_node()?;
                        self.expect(TokKind::Padding)?;
                        let mut entries = vec![Node::ObjectEntry {
                            key: Box::new(first_expr),
                            val: Box::new(val_expr),
                        }];

                        while !self.is_eof() && self.peek().kind != TokKind::RightBrace {
                            let key = self.parse_node()?;
                            self.expect(TokKind::Colon)?;
                            let val = self.parse_node()?;
                            self.expect(TokKind::Padding)?;
                            entries.push(Node::ObjectEntry {
                                key: Box::new(key),
                                val: Box::new(val),
                            });
                        }
                        self.expect(TokKind::RightBrace)?;
                        return Ok(Node::ObjectNode { entries, tok });
                    }

                    // it's a block
                    let mut exprs = vec![first_expr];
                    self.expect(TokKind::Padding)?;

                    while !self.is_eof() && self.peek().kind != TokKind::RightBrace {
                        let expr = self.parse_node()?;
                        self.expect(TokKind::Comma)?;
                        exprs.push(expr)
                    }

                    self.expect(TokKind::RightBrace)?;
                    return Ok(Node::BlockNode { exprs, tok });
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }
            TokKind::FnKeyword => {
                let scope = || -> Result<Node> {
                    self.push_min_prec(0);
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

                                    args.push(new_arg.get_payload());
                                    if self.peek().kind == TokKind::RightParan {
                                        break;
                                    }
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
                    body = match body {
                        Node::ObjectNode { entries, tok, .. } if entries.is_empty() => {
                            Node::BlockNode {
                                exprs: Vec::new(),
                                tok,
                            }
                        }
                        _ => body,
                    };

                    Ok(Node::FnNode {
                        name,
                        args,
                        rest_arg,
                        body: Box::new(body),
                        tok,
                    })
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }
            TokKind::Underscore => Ok(Node::EmptyNode { tok }),
            TokKind::Identifiers(payload) => Ok(Node::IdentifierNode {
                payload: payload.clone(),
                tok,
            }),
            TokKind::Minus | TokKind::Exclam => {
                let right = self.parse_sub_node()?;

                Ok(Node::UnaryNode {
                    right: Box::new(right),
                    tok,
                })
            }
            TokKind::IfKeyword => {
                let scope = || -> Result<Node> {
                    self.push_min_prec(0);

                    let cond_node: Node;
                    let mut branches = Vec::new();

                    // if no explicit condition is provided (i.e. if the keyword is
                    // followed by a { ... }), we assume the condition is "true" to allow
                    // for the useful `if { case, case ... }` pattern.

                    if self.peek().kind == TokKind::LeftBrace {
                        cond_node = Node::BoolNode {
                            payload: true,
                            tok: tok.clone(),
                        }
                    } else {
                        cond_node = self.parse_node()?;
                    }

                    // `if cond -> body` desugars to `if cond { true -> body }`. Note that
                    // in this form, there can only be one condition expression; `if a, b,
                    // c -> body` is not legal. However, `if a | b | c -> body` is
                    // equivalent and valid.
                    if self.peek().kind == TokKind::BranchArrow {
                        let arrow_tok = self.next().clone();
                        let body = self.parse_node()?;

                        // comma here marks end of the ifExpr, not end of branch, so we do
                        // not consume it here.
                        branches.push(Node::IfBranch {
                            target: Box::from(Node::BoolNode {
                                payload: true,
                                tok: arrow_tok,
                            }),
                            body: Rc::new(body),
                        });

                        return Ok(Node::IfExprNode {
                            cond: Box::from(cond_node),
                            branches,
                            tok,
                        });
                    }

                    self.expect(TokKind::LeftBrace)?;

                    while !self.is_eof() && self.peek().kind != TokKind::RightBrace {
                        let mut targets = Vec::new();
                        while !self.is_eof() && self.peek().kind != TokKind::BranchArrow {
                            let target = self.parse_node()?;
                            if self.peek().kind != TokKind::BranchArrow {
                                self.expect(TokKind::Comma)?;
                            }
                            targets.push(target);
                        }
                        self.expect(TokKind::BranchArrow)?;
                        let body = Rc::new(self.parse_node()?);
                        self.expect(TokKind::Padding)?;

                        // We want to support multi-target branches, but don't want to
                        // incur the performance overhead in the interpreter/evaluator of
                        // keeping every single target as a Go slice, when the vast
                        // majority of targets will be single-value, which requires just a
                        // pointer to an astNode.
                        //
                        // So instead of doing that, we penalize the multi-value case by
                        // essentially considering it syntax sugar and splitting such
                        // branches into multiple AST branches, each with one target value.
                        for target in targets {
                            //TODO: if target will always be a single value then we can remove this loop
                            branches.push(Node::IfBranch {
                                target: Box::new(target),
                                body: body.clone(),
                            })
                        }
                    }
                    self.expect(TokKind::RightBrace)?;
                    Ok(Node::IfExprNode {
                        cond: Box::from(cond_node),
                        branches,
                        tok,
                    })
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }
            TokKind::WithKeyword => {
                let mut scope = || -> Result<Node> {
                    self.push_min_prec(0);
                    let mut with_expr_base = self.parse_node()?;

                    match &mut with_expr_base {
                        Node::FnCalNode { args, .. } => {
                            let with_expr_last_arg = self.parse_node()?;
                            args.push(with_expr_last_arg);
                            Ok(with_expr_base)
                        }
                        _ => Err(Error::Syntax(format!(
                            "expected function call after with, got {}",
                            with_expr_base
                        ))),
                    }
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }
            TokKind::LeftParan => {
                let scope = || -> Result<Node> {
                    self.push_min_prec(0);
                    let mut exprs = Vec::new();

                    while !self.is_eof() && self.peek().kind != TokKind::RightParan {
                        let expr = self.parse_node()?;
                        self.expect(TokKind::Comma)?;
                        exprs.push(expr);
                    }

                    self.expect(TokKind::RightParan)?;
                    // TODO: If only one body expr and body expr is identifier or literal,
                    // unwrap the blockNode and just return the bare child

                    Ok(Node::BlockNode { exprs, tok })
                };
                let temp_node = scope();
                self.handle_pop_case(temp_node)
            }
            _ => Err(Error::Syntax(
                format_args!("unexpected token {tok} at start of unit").to_string(),
            )),
        }
    }
}

// concrete AstNode parse functions
impl Parser {
    fn parse_assignment(&mut self, left: Node) -> Result<Node> {
        if self.peek().kind != TokKind::Assign && self.peek().kind != TokKind::NonlocalAssign {
            return Ok(left);
        }

        let next = self.next().clone();
        let mut node = Node::AssignmentNode {
            is_local: next.kind == TokKind::Assign,
            left: Box::new(left),
            right: None,
            tok: next,
        };

        match &mut node {
            Node::AssignmentNode { right, .. } => *right = Some(Box::new(self.parse_node()?)),
            _ => {}
        }

        Ok(node)
    }

    fn parse_sub_node(&mut self) -> Result<Node> {
        let mut scope = || -> Result<Node> {
            self.push_min_prec(0);

            let mut node = self.parse_unit()?;

            while !self.is_eof() {
                match self.peek().kind {
                    TokKind::Dot => {
                        let next = self.next().clone();
                        let right = self.parse_unit()?;

                        node = Node::PropertyAccessNode {
                            left: Box::new(node),
                            right: Box::new(right),
                            tok: next,
                        }
                    }
                    TokKind::LeftParan => {
                        let next = self.next().clone();
                        let mut args = Vec::new();
                        let mut rest_arg: Option<Box<Node>> = None;

                        while !self.is_eof() && self.peek().kind != TokKind::RightParan {
                            let arg = self.parse_node()?;
                            if self.peek().kind == TokKind::Elispe {
                                self.next(); //eat the ellipsis
                                self.expect(TokKind::Comma)?;
                                rest_arg = Some(Box::new(arg));
                                break;
                            } else {
                                args.push(arg)
                            }

                            if self.peek().kind == TokKind::RightParan {
                                break;
                            } else if self.peek().kind == TokKind::Padding {
                                self.expect(TokKind::Padding)?;
                                continue;
                            } else {
                                self.expect(TokKind::Comma)?;
                            }
                        }
                        self.expect(TokKind::RightParan)?;

                        node = Node::FnCalNode {
                            r#fn: Box::new(node),
                            args,
                            rest_arg,
                            tok: next,
                        }
                    }
                    _ => return Ok(node),
                }
            }

            Ok(node)
        };
        let temp_node = scope();
        self.handle_pop_case(temp_node)
    }

    fn parse_node(&mut self) -> Result<Node> {
        let mut node = self.parse_sub_node()?;

        while !self.is_eof() && self.peek().kind != TokKind::Padding {
            match self.peek().kind {
                // whatever follows an assignment expr cannot bind to the
                // assignment expression itself by syntax rule, so we simply return
                TokKind::Assign | TokKind::NonlocalAssign => return self.parse_assignment(node),

                TokKind::Plus
                | TokKind::Minus
                | TokKind::Times
                | TokKind::Divide
                | TokKind::Modulus
                | TokKind::Xor
                | TokKind::And
                | TokKind::Or
                | TokKind::PushArrow
                | TokKind::Greater
                | TokKind::Less
                | TokKind::Eq
                | TokKind::Geq
                | TokKind::Leq
                | TokKind::Neq => {
                    // this case implements a mini Pratt parser threaded through the
                    // larger Oak syntax parser, using the parser struct itself to keep
                    // track of the power / precedence stack since other forms may be
                    // parsed in between, as in 1 + f(g(x := y)) + 2
                    let min_prec = self.last_min_prec();

                    loop {
                        if self.is_eof() {
                            return Err(Error::Syntax("Incomplete binary expression".to_string()));
                        }

                        let peeked = self.peek().clone();
                        let prec = Parser::infix_op_precedence(&peeked.kind);
                        if prec <= min_prec {
                            break;
                        }
                        self.next(); // eat the operator
                        if self.is_eof() {
                            return Err(Error::Syntax(format!(
                                "Incomplete binary expression with {}",
                                peeked.kind
                            )));
                        }
                        self.push_min_prec(prec);
                        let right = self.parse_node()?;
                        self.pop_min_prec();

                        node = Node::BinaryNode {
                            left: Box::new(node),
                            right: Box::new(right),
                            tok: peeked,
                        }
                    }
                    // whatever follows a binary expr cannot bind to the binary
                    // expression by syntax rule, so we simply return
                    return Ok(node);
                }
                TokKind::PipeArrow => {
                    let pos = self.next().pos.clone(); // eat the pipe
                    let mut pipe_right = self.parse_sub_node()?;
                    match &mut pipe_right {
                        Node::FnCalNode { args, .. } => {
                            args.insert(0, node);
                            node = pipe_right;
                        }
                        _ => {
                            return Err(Error::Syntax(format!(
                                "Expected function call after |>, got {}",
                                pipe_right
                            )))
                        }
                    }
                }
                _ => return Ok(node),
            }
        }
        // the trailing comma is handled as necessary in callers of parseNode
        return Ok(node);
    }

    pub fn parse(mut self) -> Result<Vec<Node>> {
        let mut nodes = Vec::new();

        while !self.is_eof() {
            let node = self.parse_node()?;
            println!("node: {:?}", node);
            self.expect(TokKind::Padding)?;
            nodes.push(node);
        }

        Ok(nodes)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::token::{Pos, TokKind, Token},
        parser,
    };

    #[test]
    fn test_parser() {
        let _expected_output: Vec<()> = vec![];

        let tokens = vec![
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

        let input = tokens
            .iter()
            .map(|t| Token {
                kind: t.clone(),
                pos: Pos(0, 0),
            })
            .collect();

        let parser = parser::parser::Parser::new(input);
        let nodes = parser.parse().unwrap();
        println!("{:?}", nodes);
        assert_eq!(nodes.len(), 8);
    }
}

use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use crate::parser::ast::Node;
use crate::parser::utils::Utils;
use crate::{
    error::error::{Error, Result},
    lexer::token::{TokKind, Token},
};

struct Precedence {
    min_binary_prec: Vec<i32>,
}

impl Precedence {
    pub fn new() -> Self {
        Self {
            min_binary_prec: vec![0],
        }
    }

    pub fn infix_op_precedence(op: &TokKind) -> i32 {
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

    fn last_min_prec(&self) -> i32 {
        self.min_binary_prec[self.min_binary_prec.len() - 1]
    }

    fn push_min_prec(&mut self, prec: i32) {
        self.min_binary_prec.push(prec)
    }

    fn pop_min_prec(&mut self) {
        self.min_binary_prec.pop().unwrap();
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
}

// parseUnit is responsible for parsing the smallest complete syntactic "units"
// of Oak's syntax, like literals including function literals, grouped
// expressions in blocks, and if/with expressions.
fn parse_unit(tokens: &mut Peekable<IntoIter<Token>>, pre_ctr: &mut Precedence) -> Result<Node> {
    let tok = tokens.next().unwrap();
    match &tok.kind {
        TokKind::Comment(_) => {
            // comments are not part of the AST
            Ok(Node::EmptyNode { tok })
        }
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
        TokKind::Colon => match &tokens.peek().unwrap().kind {
            TokKind::Identifiers(_) => Ok(Node::AtomNode {
                payload: tokens.next().unwrap().kind.to_string(),
                tok,
            }),
            TokKind::IfKeyword => {
                tokens.next();
                Ok(Node::AtomNode {
                    payload: "if".to_string(),
                    tok,
                })
            }
            TokKind::FnKeyword => {
                tokens.next();
                Ok(Node::AtomNode {
                    payload: "fn".to_string(),
                    tok,
                })
            }
            TokKind::WithKeyword => {
                tokens.next();
                Ok(Node::AtomNode {
                    payload: "with".to_string(),
                    tok,
                })
            }
            TokKind::TrueLiteral => {
                tokens.next();
                Ok(Node::AtomNode {
                    payload: "true".to_string(),
                    tok,
                })
            }
            TokKind::FalseLiteral => {
                tokens.next();
                Ok(Node::AtomNode {
                    payload: "false".to_string(),
                    tok,
                })
            }
            _ => Err(Error::Syntax(format!(
                "expected identifier after ':', got {}",
                tokens.peek().unwrap()
            ))),
        },
        TokKind::LeftBracket => {
            let scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);

                let mut item_nodes = Vec::new();
                while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightBracket {
                    let node = parse_node(tokens, pre_ctr)?;
                    if tokens.peek().unwrap().kind == TokKind::RightBracket {
                        break;
                    }
                    tokens.expect(TokKind::Comma)?;
                    item_nodes.push(node);
                }

                tokens.expect(TokKind::RightBracket)?;

                Ok(Node::ListNode {
                    elems: item_nodes,
                    tok,
                })
            };
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }

        TokKind::LeftBrace => {
            let scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);
                // empty {} is always considerd an object -- an empty block is illegal
                if tokens.peek().unwrap().kind == TokKind::RightBrace {
                    tokens.next(); // eat the right brace
                    return Ok(Node::ObjectNode {
                        entries: Vec::new(),
                        tok,
                    });
                }

                let first_expr = parse_node(tokens, pre_ctr)?;
                if tokens.is_eof() {
                    return Err(Error::Syntax(
                        "unexpected end of input inside block or object".to_string(),
                    ));
                }

                if tokens.peek().unwrap().kind == TokKind::Colon {
                    // it's an object
                    tokens.next(); // eat the coln
                    let val_expr = parse_node(tokens, pre_ctr)?;
                    tokens.expect(TokKind::Padding)?;
                    let mut entries = vec![Node::ObjectEntry {
                        key: Box::new(first_expr),
                        val: Box::new(val_expr),
                    }];

                    while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightBrace {
                        let key = parse_node(tokens, pre_ctr)?;
                        tokens.expect(TokKind::Colon)?;
                        let val = parse_node(tokens, pre_ctr)?;
                        tokens.expect(TokKind::Padding)?;
                        entries.push(Node::ObjectEntry {
                            key: Box::new(key),
                            val: Box::new(val),
                        });
                    }
                    tokens.expect(TokKind::RightBrace)?;
                    return Ok(Node::ObjectNode { entries, tok });
                }

                // it's a block
                let mut exprs = vec![first_expr];
                tokens.expect(TokKind::Padding)?;

                while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightBrace {
                    let expr = parse_node(tokens, pre_ctr)?;
                    tokens.expect(TokKind::Comma)?;
                    exprs.push(expr)
                }

                tokens.expect(TokKind::RightBrace)?;
                return Ok(Node::BlockNode { exprs, tok });
            };
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }
        TokKind::FnKeyword => {
            let scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);
                let mut name = String::new();
                match &tokens.peek().unwrap().kind {
                    TokKind::Identifiers(payload) => {
                        // optional name fn
                        name = payload.clone();
                        tokens.next();
                    }
                    _ => {}
                }

                let mut args = Vec::new();
                let mut rest_arg = String::new();
                if tokens.peek().unwrap().kind == TokKind::LeftParan {
                    // optional argument list
                    tokens.next(); // eat the leftParen
                    while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightParan {
                        match matches!(tokens.peek().unwrap().kind, TokKind::Identifiers(_)) {
                            true => {
                                let arg = tokens.next().unwrap();
                                // maybe this is a rest arg
                                if tokens.peek().unwrap().kind == TokKind::Ellipsis {
                                    match arg.kind {
                                        TokKind::Identifiers(payload) => {
                                            rest_arg = payload.clone();
                                        }
                                        _ => {}
                                    }
                                    tokens.expect(TokKind::Comma)?;
                                    break;
                                }

                                args.push(arg.get_payload());
                                if tokens.peek().unwrap().kind == TokKind::RightParan {
                                    break;
                                }
                                tokens.expect(TokKind::Comma)?;
                            }
                            false => {
                                tokens.expect(TokKind::Underscore)?;
                                args.push("".to_string());
                                tokens.expect(TokKind::Comma)?;
                                continue;
                            }
                        }
                    }
                    tokens.expect(TokKind::RightParan)?;
                }

                let mut body = parse_node(tokens, pre_ctr)?;

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
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }
        TokKind::Underscore => Ok(Node::EmptyNode { tok }),
        TokKind::Identifiers(payload) => Ok(Node::IdentifierNode {
            payload: payload.clone(),
            tok,
        }),
        TokKind::Minus | TokKind::Exclam => {
            let right = parse_sub_node(tokens, pre_ctr)?;

            Ok(Node::UnaryNode {
                right: Box::new(right),
                tok,
            })
        }
        TokKind::IfKeyword => {
            let scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);

                let cond_node: Node;
                let mut branches = Vec::new();

                // if no explicit condition is provided (i.e. if the keyword is
                // followed by a { ... }), we assume the condition is "true" to allow
                // for the useful `if { case, case ... }` pattern.

                if tokens.peek().unwrap().kind == TokKind::LeftBrace {
                    cond_node = Node::BoolNode {
                        payload: true,
                        tok: tok.clone(),
                    }
                } else {
                    cond_node = parse_node(tokens, pre_ctr)?;
                }

                // `if cond -> body` desugars to `if cond { true -> body }`. Note that
                // in this form, there can only be one condition expression; `if a, b,
                // c -> body` is not legal. However, `if a | b | c -> body` is
                // equivalent and valid.
                if tokens.peek().unwrap().kind == TokKind::BranchArrow {
                    let arrow_tok = tokens.next().unwrap();
                    let body = parse_node(tokens, pre_ctr)?;

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

                tokens.expect(TokKind::LeftBrace)?;

                while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightBrace {
                    let mut targets = Vec::new();
                    while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::BranchArrow {
                        let target = parse_node(tokens, pre_ctr)?;
                        if tokens.peek().unwrap().kind != TokKind::BranchArrow {
                            tokens.expect(TokKind::Comma)?;
                        }
                        targets.push(target);
                    }
                    tokens.expect(TokKind::BranchArrow)?;
                    let body = Rc::new(parse_node(tokens, pre_ctr)?);
                    tokens.expect(TokKind::Padding)?;

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
                tokens.expect(TokKind::RightBrace)?;
                Ok(Node::IfExprNode {
                    cond: Box::from(cond_node),
                    branches,
                    tok,
                })
            };
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }
        TokKind::WithKeyword => {
            let mut scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);
                let mut with_expr_base = parse_node(tokens, pre_ctr)?;

                match &mut with_expr_base {
                    Node::FnCalNode { args, .. } => {
                        let with_expr_last_arg = parse_node(tokens, pre_ctr)?;
                        args.push(with_expr_last_arg);
                        Ok(with_expr_base)
                    }
                    _ => Err(Error::Syntax(format!(
                        "expected function call after with, got {}",
                        with_expr_base
                    ))),
                }
            };
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }
        TokKind::LeftParan => {
            let scope = || -> Result<Node> {
                pre_ctr.push_min_prec(0);
                let mut exprs = Vec::new();

                while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightParan {
                    let expr = parse_node(tokens, pre_ctr)?;
                    tokens.expect(TokKind::Comma)?;
                    exprs.push(expr);
                }

                tokens.expect(TokKind::RightParan)?;
                // TODO: If only one body expr and body expr is identifier or literal,
                // unwrap the blockNode and just return the bare child

                Ok(Node::BlockNode { exprs, tok })
            };
            let node = scope();
            pre_ctr.handle_pop_case(node)
        }
        _ => Err(Error::Syntax(
            format_args!("unexpected token {tok} at start of unit").to_string(),
        )),
    }
}

// concrete AstNode parse functions
fn parse_assignment(
    tokens: &mut Peekable<IntoIter<Token>>,
    left: Node,
    pre_ctr: &mut Precedence,
) -> Result<Node> {
    if tokens.peek().unwrap().kind != TokKind::Assign
        && tokens.peek().unwrap().kind != TokKind::NonlocalAssign
    {
        return Ok(left);
    }

    let next = tokens.next().unwrap();
    let mut node = Node::AssignmentNode {
        is_local: next.kind == TokKind::Assign,
        left: Box::new(left),
        right: None,
        tok: next,
    };

    match &mut node {
        Node::AssignmentNode { right, .. } => *right = Some(Box::new(parse_node(tokens, pre_ctr)?)),
        _ => {}
    }

    Ok(node)
}

fn parse_sub_node(
    tokens: &mut Peekable<IntoIter<Token>>,
    prec_ctr: &mut Precedence,
) -> Result<Node> {
    let mut scope = || -> Result<Node> {
        prec_ctr.push_min_prec(0);

        let mut node = parse_unit(tokens, prec_ctr)?;

        while !tokens.is_eof() {
            match tokens.peek().unwrap().kind {
                TokKind::Dot => {
                    let next = tokens.next().unwrap().clone();
                    let right = parse_unit(tokens, prec_ctr)?;

                    node = Node::PropertyAccessNode {
                        left: Box::new(node),
                        right: Box::new(right),
                        tok: next,
                    }
                }
                TokKind::LeftParan => {
                    let next = tokens.next().unwrap().clone();
                    let mut args = Vec::new();
                    let mut rest_arg: Option<Box<Node>> = None;

                    while !tokens.is_eof() && tokens.peek().unwrap().kind != TokKind::RightParan {
                        let arg = parse_node(tokens, prec_ctr)?;
                        if tokens.peek().unwrap().kind == TokKind::Ellipsis {
                            tokens.next(); //eat the ellipsis
                            tokens.expect(TokKind::Comma)?;
                            rest_arg = Some(Box::new(arg));
                            break;
                        } else {
                            args.push(arg)
                        }

                        if tokens.peek().unwrap().kind == TokKind::RightParan {
                            break;
                        } else if tokens.peek().unwrap().kind == TokKind::Padding {
                            tokens.expect(TokKind::Padding)?;
                            continue;
                        } else {
                            tokens.expect(TokKind::Comma)?;
                        }
                    }
                    tokens.expect(TokKind::RightParan)?;

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
    let node = scope();
    prec_ctr.handle_pop_case(node)
}

fn parse_node(tokens: &mut Peekable<IntoIter<Token>>, prec_ctr: &mut Precedence) -> Result<Node> {
    let mut node = parse_sub_node(tokens, prec_ctr)?;

    while !tokens.is_eof() && !tokens.has_padding() {
        match tokens.peek().unwrap().kind {
            // whatever follows an assignment expr cannot bind to the
            // assignment expression itself by syntax rule, so we simply return
            TokKind::Assign | TokKind::NonlocalAssign => {
                return parse_assignment(tokens, node, prec_ctr)
            }

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
                let min_prec = prec_ctr.last_min_prec();

                loop {
                    if tokens.is_eof() {
                        return Err(Error::Syntax("Incomplete binary expression".to_string()));
                    }

                    let peeked = tokens.peek().unwrap().clone();
                    let prec = Precedence::infix_op_precedence(&peeked.kind);
                    if prec <= min_prec {
                        break;
                    }
                    tokens.next(); // eat the operator
                    if tokens.is_eof() {
                        return Err(Error::Syntax(format!(
                            "Incomplete binary expression with {}",
                            peeked.kind
                        )));
                    }
                    prec_ctr.push_min_prec(prec);
                    let right = parse_node(tokens, prec_ctr)?;
                    prec_ctr.pop_min_prec();

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
                let pos = tokens.next().unwrap().pos.clone(); // eat the pipe
                let mut pipe_right = parse_sub_node(tokens, prec_ctr)?;
                match &mut pipe_right {
                    Node::FnCalNode { args, .. } => {
                        args.insert(0, node);
                        node = pipe_right;
                    }
                    _ => {
                        return Err(Error::Syntax(format!(
                            "Expected function call after |>, got {} as {pos}",
                            pipe_right
                        )))
                    }
                }
            }
            _ => return Ok(node),
        }
    }
    // the trailing comma is handled as necessary in callers of parseNode
    Ok(node)
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Node>> {
    let mut nodes = Vec::new();
    let mut prec_ctr = Precedence::new();
    let mut tokens = tokens.into_iter().peekable();

    while !tokens.is_eof() {
        let node = parse_node(&mut tokens, &mut prec_ctr)?;
        tokens.expect(TokKind::Padding)?;
        nodes.push(node);
    }

    Ok(nodes)
}

#[cfg(test)]
mod tests {
    use crate::parser::parser::parse;
    use crate::{
        lexer::token::{Pos, TokKind, Token},
        lexer::tokenizer::tokenize,
    };

    #[test]
    fn test_parser_with_integration_with_tokenizer() {
        let source_string: &[u8] = b"
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
        let mut reader = std::io::BufReader::new(source_string);
        let input = tokenize(&mut reader, true).unwrap();

        let nodes = parse(input).unwrap();
        println!("{:?}", nodes);
        assert_eq!(nodes.len(), 3);
    }

    #[test]
    fn test_parser_operator_precedence() {
        let tokens = vec![
            TokKind::Identifiers("a".to_string()),
            TokKind::Assign,
            TokKind::NumberLiteral("1".to_string()),
            TokKind::Plus,
            TokKind::NumberLiteral("2".to_string()),
            TokKind::Times,
            TokKind::NumberLiteral("3".to_string()),
            TokKind::Padding,
        ];

        let input = tokens
            .iter()
            .map(|t| Token {
                kind: t.clone(),
                pos: Pos(0, 0),
            })
            .collect();

        let nodes = parse(input).unwrap();
        println!("{:?}", nodes);
        assert_eq!(nodes.len(), 1);
    }

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

        let nodes = parse(input).unwrap();
        println!("{:?}", nodes);
        assert_eq!(nodes.len(), 8);
    }
}

use std::fmt::Formatter;
use std::rc::Rc;
use std::{
    any::Any,
    fmt::{self, Display},
};

use crate::lexer::token::{Pos, TokKind, Token};

pub trait AstNode: Display {
    fn pos(&self) -> Option<&Pos>;
}

pub enum Node {
    EmptyNode {
        tok: Token,
    },
    NullNode {
        tok: Token,
    },
    StringNode {
        payload: Vec<u8>,
        tok: Token,
    },
    IntNode {
        payload: i64,
        tok: Token,
    },
    FloatNode {
        payload: f64,
        tok: Token,
    },
    BoolNode {
        payload: bool,
        tok: Token,
    },
    AtomNode {
        payload: String,
        tok: Token,
    },
    ListNode {
        elems: Vec<Node>,
        tok: Token,
    },
    ObjectEntry {
        key: Box<Node>,
        val: Box<Node>,
    },
    ObjectNode {
        entries: Vec<Node>,
        tok: Token,
    },
    FnNode {
        name: String,
        args: Vec<String>,
        rest_arg: String,
        body: Box<Node>,
        tok: Token,
    },
    IdentifierNode {
        payload: String,
        tok: Token,
    },
    AssignmentNode {
        is_local: bool,
        left: Box<Node>,
        right: Option<Box<Node>>,
        tok: Token,
    },
    PropertyAccessNode {
        left: Box<Node>,
        right: Box<Node>,
        tok: Token,
    },
    UnaryNode {
        right: Box<Node>,
        tok: Token,
    },
    BinaryNode {
        left: Box<Node>,
        right: Box<Node>,
        tok: Token,
    },
    FnCalNode {
        r#fn: Box<Node>,
        args: Vec<Node>,
        rest_arg: Option<Box<Node>>,
        tok: Token,
    },
    IfBranch {
        target: Box<Node>,
        body: Rc<Node>,
    },
    IfExprNode {
        cond: Box<Node>,
        branches: Vec<Node>,
        tok: Token,
    },
    BlockNode {
        exprs: Vec<Node>,
        tok: Token,
    },
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Node::EmptyNode { .. } => write!(f, "_"),
            Node::NullNode { .. } => write!(f, "?"),
            Node::StringNode { payload, .. } => write!(
                f,
                r#""{}""#,
                payload.iter().map(|&c| c as char).collect::<String>()
            ),
            Node::IntNode { payload, .. } => write!(f, "{}", payload),
            Node::FloatNode { payload, .. } => write!(f, "{}", payload),
            Node::BoolNode { payload, .. } => write!(f, "{}", payload),
            Node::AtomNode { payload, .. } => write!(f, "{}", payload),
            Node::ListNode { elems, .. } => write!(
                f,
                "[{}]",
                elems
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::ObjectEntry { key, val } => write!(f, "{}: {}", key, val),
            Node::ObjectNode { entries, .. } => write!(
                f,
                "{{{}}}",
                entries
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::FnNode {
                name,
                args,
                rest_arg,
                body,
                ..
            } => {
                let mut head: String;
                if name == "" {
                    head = "fn".to_string();
                } else {
                    head = String::from("fn ");
                    head.push_str(&name)
                }

                let mut arg_strings = args.clone();
                if rest_arg != "" {
                    arg_strings.push(format!("{}...", rest_arg))
                }
                head.push_str(&format!("({})", arg_strings.join(", ")));

                write!(f, "{head} {}", body)
            }
            Node::IdentifierNode { payload, .. } => write!(f, "{}", payload),
            Node::AssignmentNode {
                is_local,
                left,
                right,
                ..
            } => {
                if *is_local {
                    return write!(f, "{} := {}", left, right.as_ref().unwrap());
                }
                write!(f, "{} <- {}", left, right.as_ref().unwrap())
            }
            Node::PropertyAccessNode { left, right, .. } => write!(f, "({}.{})", left, right),
            Node::UnaryNode { tok, right, .. } => write!(f, "{}{}", tok.kind, right),
            Node::BinaryNode {
                tok, left, right, ..
            } => write!(f, "({} {} {})", left, tok.kind, right),
            Node::FnCalNode {
                args,
                rest_arg,
                r#fn,
                ..
            } => {
                let mut arg_strings = Vec::new();
                for arg in args {
                    arg_strings.push(arg.to_string())
                }
                if let Some(rest_arg) = rest_arg {
                    arg_strings.push(rest_arg.to_string() + "...")
                }

                write!(f, "call[{}]({})", r#fn, arg_strings.join(", "))
            }
            Node::IfBranch { target, body, .. } => {
                write!(f, "{} -> {}", target.to_string(), body.to_string())
            }
            Node::IfExprNode { cond, branches, .. } => write!(
                f,
                "if {} {{{}}}",
                cond.to_string(),
                branches
                    .iter()
                    .map(|b| b.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::BlockNode { exprs, .. } => write!(
                f,
                "{{ {} }}",
                exprs
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl AstNode for Node {
    fn pos(&self) -> Option<&Pos> {
        match self {
            Node::EmptyNode { tok }
            | Node::NullNode { tok }
            | Node::StringNode { tok, .. }
            | Node::IntNode { tok, .. }
            | Node::FloatNode { tok, .. }
            | Node::BoolNode { tok, .. }
            | Node::AtomNode { tok, .. }
            | Node::ListNode { tok, .. }
            | Node::ObjectNode { tok, .. }
            | Node::FnNode { tok, .. }
            | Node::IdentifierNode { tok, .. }
            | Node::AssignmentNode { tok, .. }
            | Node::PropertyAccessNode { tok, .. }
            | Node::UnaryNode { tok, .. }
            | Node::BinaryNode { tok, .. }
            | Node::FnCalNode { tok, .. }
            | Node::IfExprNode { tok, .. }
            | Node::BlockNode { tok, .. } => Some(&tok.pos),
            _ => None,
        }
    }
}

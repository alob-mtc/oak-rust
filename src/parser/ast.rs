use std::fmt::{self, Display};

use crate::lexer::token::{Pos, TokKind, Token};

trait AstNode: Display {
    fn pos(&self) -> &Pos;
}

struct EmptyNode {
    tok: Option<Token>,
}

impl AstNode for EmptyNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for EmptyNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "_")
    }
}

struct NullNode {
    tok: Option<Token>,
}

impl AstNode for NullNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for NullNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "?")
    }
}

struct StringNode {
    payload: Vec<u8>,
    tok: Option<Token>,
}

impl AstNode for StringNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for StringNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            r#""{}""#,
            self.payload.iter().map(|&c| c as char).collect::<String>()
        )
    }
}

struct IntNode {
    payload: i64,
    tok: Option<Token>,
}

impl AstNode for IntNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.payload)
    }
}

struct FloatNode {
    payload: f64,
    tok: Option<Token>,
}

impl AstNode for FloatNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for FloatNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.payload)
    }
}

struct BoolNode {
    payload: bool,
    tok: Option<Token>,
}

impl AstNode for BoolNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for BoolNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.payload)
    }
}

struct AtomNode {
    payload: String,
    tok: Option<Token>,
}

impl AstNode for AtomNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for AtomNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.payload)
    }
}

struct ListNode {
    elems: Vec<Box<dyn Display>>,
    tok: Option<Token>,
}

impl AstNode for ListNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

impl Display for ListNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elems
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

struct ObjectEntry {
    key: Box<dyn Display>,
    val: Box<dyn Display>,
}

impl Display for ObjectEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.val)
    }
}

struct ObjectNode {
    entries: Vec<ObjectEntry>,
    tok: Option<Token>,
}

impl Display for ObjectNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.entries
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl AstNode for ObjectNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct FnNode {
    name: String,
    args: Vec<String>,
    rest_arg: String,
    body: Box<dyn AstNode>,
    tok: Option<Token>,
}

impl Display for FnNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut head = String::new();
        if self.name == "" {
            head = "fn".to_string();
        } else {
            head = String::from("fn ");
            head.push_str(&self.name)
        }

        let mut arg_strings = self.args.clone();
        if self.rest_arg != "" {
            arg_strings.push(format!("{}...", self.rest_arg))
        }
        head.push_str(&format!("({})", arg_strings.join(", ")));

        write!(f, "{head} {}", self.body)
    }
}

impl AstNode for FnNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct IdentifierNode {
    payload: String,
    tok: Option<Token>,
}

impl Display for IdentifierNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.payload)
    }
}

impl AstNode for IdentifierNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct AssignmentNode {
    is_local: bool,
    left: Box<dyn AstNode>,
    right: Box<dyn AstNode>,
    tok: Option<Token>,
}

impl Display for AssignmentNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_local {
            return write!(f, "{} := {}", self.left, self.right);
        }
        write!(f, "{} <- {}", self.left, self.right)
    }
}

impl AstNode for AssignmentNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct PropertyAccessNode {
    left: Box<dyn AstNode>,
    right: Box<dyn AstNode>,
    tok: Option<Token>,
}

impl Display for PropertyAccessNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}.{})", self.left, self.right)
    }
}

impl AstNode for PropertyAccessNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct UnaryNode {
    op: TokKind,
    right: Box<dyn AstNode>,
    tok: Option<Token>,
}

impl Display for UnaryNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.op, self.right)
    }
}

impl AstNode for UnaryNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct BinaryNode {
    op: TokKind,
    left: Box<dyn AstNode>,
    right: Box<dyn AstNode>,
    tok: Option<Token>,
}

impl Display for BinaryNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

impl AstNode for BinaryNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct FnCalNode {
    r#fn: Box<dyn AstNode>,
    args: Vec<Box<dyn AstNode>>,
    rest_arg: Option<Box<dyn AstNode>>,
    tok: Option<Token>,
}

impl Display for FnCalNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut arg_strings = Vec::new();
        for arg in &self.args {
            arg_strings.push(arg.to_string())
        }
        if let Some(rest_arg) = &self.rest_arg {
            arg_strings.push(rest_arg.to_string() + "...")
        }

        write!(f, "call[{}]({})", self.r#fn, arg_strings.join(", "))
    }
}

impl AstNode for FnCalNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct IfBranch {
    target: Box<dyn AstNode>,
    body: Box<dyn AstNode>,
}

impl Display for IfBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.target.to_string(),
            self.body.to_string()
        )
    }
}

struct IfExprNode {
    cond: Box<dyn AstNode>,
    branches: Vec<IfBranch>,
    tok: Option<Token>,
}

impl Display for IfExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if {} {{{}}}",
            self.cond.to_string(),
            self.branches
                .iter()
                .map(|b| b.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl AstNode for IfExprNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

struct BlockNode {
    exprs: Vec<Box<dyn AstNode>>,
    tok: Option<Token>,
}

impl Display for BlockNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.exprs
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl AstNode for BlockNode {
    fn pos(&self) -> &Pos {
        &self.tok.as_ref().unwrap().pos
    }
}

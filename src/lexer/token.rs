use std::fmt::{self, Display};

#[derive(Debug, Clone, Default)]
pub struct Pos {
    pub line: i32,
    pub col: i32,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}:{}]", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokKind,
    pub pos: Pos,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokKind {
    // sentinel
    Unknown,
    Comment(String),
    // language tokens
    Comma,
    Dot,
    LeftParan,
    RightParan,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Assign,
    NonlocalAssign,
    PipeArrow,
    BranchArrow,
    PushArrow,
    Colon,
    Elispe,
    Qmark,
    Exclam,
    // binary operators
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    Xor,
    And,
    Or,
    Greater,
    Less,
    Eq,
    Geq,
    Leq,
    Neq,
    // keywords
    IfKeyword,
    FnKeyword,
    WithKeyword,
    // identidifiers and literals
    Underscore,
    Identifiers(String),
    TrueLiteral,
    FalseLiteral,
    StringLiteral(String),
    NumberLiteral(String),
}

impl fmt::Display for TokKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokKind::Comment(payload) => write!(f, "//({payload})"),
            TokKind::Comma => write!(f, ","),
            TokKind::Dot => write!(f, ","),
            TokKind::LeftParan => write!(f, "("),
            TokKind::RightParan => write!(f, ")"),
            TokKind::LeftBracket => write!(f, "["),
            TokKind::RightBracket => write!(f, "]"),
            TokKind::LeftBrace => write!(f, "{{"),
            TokKind::RightBrace => write!(f, "}}"),
            TokKind::Assign => write!(f, ":="),
            TokKind::NonlocalAssign => write!(f, "<-"),
            TokKind::PipeArrow => write!(f, "|>"),
            TokKind::BranchArrow => write!(f, "->"),
            TokKind::PushArrow => write!(f, "<<"),
            TokKind::Colon => write!(f, ":"),
            TokKind::Elispe => write!(f, "..."),
            TokKind::Qmark => write!(f, "?"),
            TokKind::Exclam => write!(f, "!"),
            TokKind::Plus => write!(f, "+"),
            TokKind::Minus => write!(f, "-"),
            TokKind::Times => write!(f, "*"),
            TokKind::Divide => write!(f, "/"),
            TokKind::Modulus => write!(f, "%"),
            TokKind::Xor => write!(f, "^"),
            TokKind::And => write!(f, "&"),
            TokKind::Or => write!(f, "|"),
            TokKind::Greater => write!(f, ">"),
            TokKind::Less => write!(f, "<"),
            TokKind::Eq => write!(f, "="),
            TokKind::Geq => write!(f, ">="),
            TokKind::Leq => write!(f, "<="),
            TokKind::Neq => write!(f, "!="),
            TokKind::IfKeyword => write!(f, "if"),
            TokKind::FnKeyword => write!(f, "fn"),
            TokKind::WithKeyword => write!(f, "with"),
            TokKind::Underscore => write!(f, "_"),
            TokKind::Identifiers(payload) => write!(f, "var({payload})"),
            TokKind::TrueLiteral => write!(f, "true"),
            TokKind::FalseLiteral => write!(f, "false"),
            TokKind::StringLiteral(payload) => write!(f, r#"string("{payload}")"#),
            TokKind::NumberLiteral(payload) => write!(f, "number({payload})",),
            _ => write!(f, "(unknown token)"),
        }
    }
}

use std::fmt::{self, Display};

#[derive(PartialEq, Debug, Clone, Default)]
pub struct Pos(pub usize, pub usize);

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}:{}]", self.0, self.1)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokKind,
    pub pos: Pos,
}

impl Token {
    pub fn get_payload(&self) -> String {
        self.kind.get_payload()
    }
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
    // identifiers and literals
    Underscore,
    Identifiers(String),
    TrueLiteral,
    FalseLiteral,
    StringLiteral(String),
    NumberLiteral(String),
    // special
    Padding,
}

impl TokKind {
    fn get_payload(&self) -> String {
        match self {
            TokKind::Comment(payload) => payload.to_string(),
            TokKind::Identifiers(payload) => payload.to_string(),
            TokKind::StringLiteral(payload) => payload.to_string(),
            TokKind::NumberLiteral(payload) => payload.to_string(),
            _ => "".to_string(),
        }
    }

    pub fn allow_padding(tok_kind: &TokKind) -> bool {
        match tok_kind {
            TokKind::Comma
            | TokKind::LeftParan
            | TokKind::LeftBracket
            | TokKind::LeftBrace
            | TokKind::Plus
            | TokKind::Minus
            | TokKind::Times
            | TokKind::Divide
            | TokKind::Modulus
            | TokKind::Xor
            | TokKind::And
            | TokKind::Or
            | TokKind::Exclam
            | TokKind::Greater
            | TokKind::Less
            | TokKind::Eq
            | TokKind::Geq
            | TokKind::Leq
            | TokKind::Assign
            | TokKind::NonlocalAssign
            | TokKind::Dot
            | TokKind::Colon
            | TokKind::FnKeyword
            | TokKind::IfKeyword
            | TokKind::WithKeyword
            | TokKind::PipeArrow
            | TokKind::BranchArrow
            | TokKind::PushArrow => false,
            _ => true,
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokKind {
    match ident {
        "fn" => TokKind::FnKeyword,
        "true" => TokKind::TrueLiteral,
        "false" => TokKind::FalseLiteral,
        "if" => TokKind::IfKeyword,
        "with" => TokKind::WithKeyword,
        _ => TokKind::Identifiers(ident.to_string()),
    }
}

impl fmt::Display for TokKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokKind::Comment(payload) => write!(f, "//({payload})"),
            TokKind::Comma => write!(f, ","),
            TokKind::Dot => write!(f, "."),
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
            TokKind::Padding => write!(f, "padding"),
            _ => write!(f, "(unknown token)"),
        }
    }
}

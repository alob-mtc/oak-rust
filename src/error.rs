use std::{fmt::Display, io::Error};

use crate::lexer::token::Pos;

pub type Result<T> = core::result::Result<T, Err>;

#[derive(Debug)]
pub enum ErrorReason {
    Syntax,
    Runtime,
    System,
}

impl Display for ErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorReason::Syntax => write!(f, "Syntx error"),
            ErrorReason::Runtime => write!(f, "Runtime error"),
            ErrorReason::System => write!(f, "System error"),
        }
    }
}

#[derive(Debug)]
pub struct Err {
    pub reason: ErrorReason,
    pub message: String,
    pub pos: Pos,
}

impl Display for Err {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}: {}", self.reason, self.pos, self.message)
    }
}

impl From<Error> for Err {
    fn from(err: Error) -> Self {
        Err {
            reason: ErrorReason::System,
            message: err.to_string(),
            pos: Pos::default(),
        }
    }
}

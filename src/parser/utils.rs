use crate::error::error::Error;
use crate::lexer::token::{TokKind, Token};
use std::iter::Peekable;
use std::vec::IntoIter;

pub type TokenIter = Peekable<IntoIter<Token>>;

pub trait Utils {
    fn is_eof(&self) -> bool;
    fn has_padding(&mut self) -> bool;
    fn expect(&mut self, kind: TokKind) -> crate::error::error::Result<Token>;
}

impl Utils for TokenIter {
    fn is_eof(&self) -> bool {
        self.len() == 0
    }

    fn has_padding(&mut self) -> bool {
        if let Some(tok) = self.peek() {
            tok.kind == TokKind::Padding
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokKind) -> crate::error::error::Result<Token> {
        if self.is_eof() {
            return Err(Error::Syntax(format!(
                "unexpted end of input, expected {kind}"
            )));
        }

        let next = self.next().unwrap();

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
}

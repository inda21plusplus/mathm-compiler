use std::error::Error as StdError;
use std::fmt;

use crate::Span;

// todo: add more stuff
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub at: Span,
}

impl Error {
    pub fn new(at: Span) -> Self {
        Self { at }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parsing error at {}", self.at)
    }
}

impl StdError for Error {}

use core::fmt;
use std::{cmp, ops::Range};

use crate::error::ParsingError;
use combinators::{MapParser, OptionalParser, ParserCombinator, TryMapParser};

mod combinators;
mod keywords;

/// A collection of parsers that parse specific things
pub mod parsers {
    mod basic;
    mod number;

    pub use basic::{CharParser, IntegerParser, SpanParser, StrParser, Ws};
    pub use number::IntegerLiteralParser;
}

pub trait Parser {
    type Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError>;

    /// Because of orphan rules, operators cannot be implemented for any `Parser`, so this must be
    /// used to wrap Parsers in a `ParserCombinator`
    fn c(self) -> ParserCombinator<Self>
    where
        Self: Sized,
    {
        ParserCombinator(self)
    }

    fn map<T, F: FnOnce(Self::Output) -> T>(self, f: F) -> MapParser<Self, T, F>
    where
        Self: Sized,
    {
        MapParser(self, f)
    }

    fn try_map<T, F: FnOnce(Self::Output) -> Result<T, ParsingError>>(
        self,
        f: F,
    ) -> TryMapParser<Self, T, F>
    where
        Self: Sized,
    {
        TryMapParser(self, f)
    }

    fn optional(self) -> OptionalParser<Self>
    where
        Self: Sized,
    {
        OptionalParser(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span(pub Range<usize>); // todo: in which file?

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Self(range)
    }
    pub fn single(at: usize) -> Self {
        Self(at..at + 1)
    }
    pub fn len(&self) -> usize {
        self.0.end - self.0.start
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(if self.0.start < other.0.start {
            cmp::Ordering::Less
        } else if self.0.start > other.0.start {
            cmp::Ordering::Greater
        } else if self.0.end < other.0.end {
            cmp::Ordering::Less
        } else if self.0.start > other.0.start {
            cmp::Ordering::Greater
        } else {
            cmp::Ordering::Equal
        })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.0.start, self.0.end)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Input<'s> {
    /// the location of the input, in bytes
    pub location: usize,
    pub s: &'s str,
}

impl<'s> Input<'s> {
    pub fn from_str(s: &'s str) -> Input<'s> {
        Self { location: 0, s }
    }
    fn next(&self) -> Option<(Self, char)> {
        self.s.chars().next().map(|c| {
            (
                Self {
                    location: self.location + c.len_utf8(),
                    s: &self.s[c.len_utf8()..],
                },
                c,
            )
        })
    }
}

impl<'s> std::ops::Index<Span> for Input<'s> {
    type Output = str;

    fn index(&self, i: Span) -> &Self::Output {
        &self.s[i.0.start - self.location..i.0.end - self.location]
    }
}

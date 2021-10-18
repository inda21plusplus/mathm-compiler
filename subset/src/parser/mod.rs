use core::fmt;
use std::{cmp, ops::Range};

use crate::error::ParsingError;

mod keywords;
mod ops;
mod parser;

pub use parser::{AndParser, Either, EitherParser, OrParser, Parser};

#[derive(Debug, PartialEq, Eq)]
pub struct Span(pub Range<usize>); // todo: in which file?

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Self(range)
    }
    pub fn single(at: usize) -> Self {
        Self(at..at + 1)
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
                    location: self.location + 1,
                    s: &self.s[1..],
                },
                c,
            )
        })
    }
}

pub struct CharParser(pub char);

impl Parser for CharParser {
    type Output = char;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        match input.next() {
            Some((rest, c)) if c == self.0 => Ok((rest, c)),
            _ => Err(ParsingError {
                at: Span::single(input.location),
            }),
        }
    }
}

pub struct StrParser<'s>(pub &'s str);

impl<'s> Parser for StrParser<'s> {
    type Output = &'s str;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        for parser in self.0.chars().map(CharParser) {
            input = parser.parse(input)?.0;
        }

        Ok((input, self.0))
    }
}

pub struct SpanParser<P: Fn(char) -> bool>(P);

impl<P: Fn(char) -> bool> Parser for SpanParser<P> {
    type Output = Span;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let start = input.location;
        while let Some((rest, ch)) = input.next() {
            if self.0(ch) {
                input = rest;
            } else {
                break;
            }
        }
        Ok((input, Span::new(start..input.location)))
    }
}

pub fn ws() -> SpanParser<impl Fn(char) -> bool> {
    SpanParser(char::is_whitespace)
}

pub struct Ws;

impl Parser for Ws {
    type Output = Span;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let start = input.location;
        while let Some((rest, ch)) = input.next() {
            if ch.is_whitespace() {
                input = rest;
            } else {
                break;
            }
        }
        Ok((input, Span::new(start..input.location)))
    }
}

// todo: remove, just for testing
pub struct BoolParser;

impl Parser for BoolParser {
    type Output = bool;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        (StrParser("true").c() | StrParser("false"))
            .parse(input)
            .map(|(rest, output)| match output {
                "true" => (rest, true),
                "false" => (rest, false),
                _ => unreachable!(),
            })
    }
}

pub struct IntegerParser;

impl Parser for IntegerParser {
    type Output = u64;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        // todo: different bases and suffixes indicating type e.g. 0b101010usize
        SpanParser(|ch| ch.is_digit(10))
            .map(|span| {
                input.s[span.0.clone()]
                    .parse()
                    .map_err(|_| ParsingError::new(Span::single(0)))
            })
            .parse(input)
    }
}

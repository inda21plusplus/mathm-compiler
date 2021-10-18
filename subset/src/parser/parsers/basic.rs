use super::super::{Input, Parser, Span};

use crate::error::ParsingError;

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub struct Ws;

impl Parser for Ws {
    type Output = Span;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        SpanParser(char::is_whitespace).parse(input)
    }
}

// todo: remove, just for testing
#[derive(Debug, Clone, Copy)]
pub struct BoolParser;

impl Parser for BoolParser {
    type Output = bool;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        (StrParser("true").map(|_| true).c() | StrParser("false").map(|_| false)).parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntegerParser;

impl Parser for IntegerParser {
    type Output = u64;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        // todo: different bases and suffixes indicating type e.g. 0b101010usize
        SpanParser(|ch| ch.is_digit(10))
            .try_map(|span| {
                input.s[span.0.clone()]
                    .parse()
                    // since all characters are digits, an error must mean that we got no
                    // characters
                    .map_err(|_| ParsingError::new(Span::single(0)))
            })
            .parse(input)
    }
}
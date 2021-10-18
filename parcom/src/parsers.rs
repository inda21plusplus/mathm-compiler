//! A collection of parsers that parse specific things

use crate::{Error, Input, Parser, Span};

#[derive(Debug, Clone, Copy)]
pub struct CharParser(pub char);

impl Parser for CharParser {
    type Output = char;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        match input.next() {
            Some((rest, c)) if c == self.0 => Ok((rest, c)),
            _ => Err(Error {
                at: Span::single(input.location),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrParser<'s>(pub &'s str);

impl<'s> Parser for StrParser<'s> {
    type Output = &'s str;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        for parser in self.0.chars().map(CharParser) {
            input = parser.parse(input)?.0;
        }

        Ok((input, self.0))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpanParser<P: Fn(char) -> bool>(pub P);

impl<P: Fn(char) -> bool> Parser for SpanParser<P> {
    type Output = Span;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        SpanParser(char::is_whitespace).parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntegerParser {
    pub base: u32,
}

impl Parser for IntegerParser {
    // todo: bigger?
    type Output = u128;

    // todo: clean up?
    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let digits = &"0123456789abcdef"[0..self.base as usize];
        SpanParser(|ch| digits.contains(ch))
            .try_map(|span| {
                if span.len() > 0 {
                    eprintln!("input={:?}, span={:?}", input, span);
                    input[span.clone()].chars().try_fold(0u128, |acc, digit| {
                        acc.checked_mul(10)
                            .map(|v| v.checked_add(digits.find(digit).unwrap() as u128))
                            .flatten()
                            .ok_or(Error::new(span.clone()))
                    })
                } else {
                    Err(Error::new(Span::single(input.location)))
                }
            })
            .parse(input)
    }
}

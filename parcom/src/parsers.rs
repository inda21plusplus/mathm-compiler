//! A collection of parsers that parse specific things

use crate::{Error, Input, Parser, Span};

#[derive(Debug, Clone, Copy)]
pub struct CharParser(pub char);

impl Parser for CharParser {
    type Output = Span;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        PredicateParser(move |c| c == self.0).parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrParser<'s>(pub &'s str);

impl<'s> Parser for StrParser<'s> {
    type Output = Span;

    fn parse(self, mut input: Input) -> Result<(Input, Self::Output), Error> {
        let start = input.location;
        for parser in self.0.chars().map(CharParser) {
            input = parser.parse(input)?.0;
        }

        Ok((input, Span::new(start..input.location)))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PredicateParser<P: FnOnce(char) -> bool>(pub P);

impl<P: FnOnce(char) -> bool> Parser for PredicateParser<P> {
    type Output = Span;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        match input.next() {
            Some((rest, c)) if self.0(c) => Ok((rest, Span::first(&input))),
            _ => Err(Error::new(Span::first(&input))),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpanParser<P: Fn(char) -> bool>(pub P);

impl<P: Fn(char) -> bool> Parser for SpanParser<P> {
    type Output = Span;

    fn parse(self, mut input: Input) -> Result<(Input, Self::Output), Error> {
        let mut span = Span::new(input.location..input.location);
        while let Some((rest, ch)) = input.next() {
            if self.0(ch) {
                span = span.merge(Span::first(&input));
                input = rest;
            } else {
                break;
            }
        }
        if !span.is_empty() {
            Ok((input, span))
        } else {
            Err(Error::new(Span::first(&input)))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Ws;

impl Parser for Ws {
    type Output = Option<Span>;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        SpanParser(char::is_whitespace).optional().parse(input)
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
    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        let digits = &"0123456789abcdef"[0..self.base as usize];
        SpanParser(|ch| digits.contains(ch))
            .try_map(|span| {
                if !span.is_empty() {
                    input[span].chars().try_fold(0u128, |acc, digit| {
                        acc.checked_mul(self.base.into())
                            .map(|v| v.checked_add(digits.find(digit).unwrap() as u128))
                            .flatten()
                            .ok_or_else(|| Error::new(span))
                    })
                } else {
                    Err(Error::new(Span::first(&input)))
                }
            })
            .parse(input)
    }
}

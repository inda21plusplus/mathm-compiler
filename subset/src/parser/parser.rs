use super::{combinators::ParserCombinator, Input};
use crate::error::ParsingError;

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
}

pub struct PureParser<T>(pub T);

impl<T> Parser for PureParser<T> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        Ok((input, self.0))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MapParser<P: Parser, T, F: FnOnce(P::Output) -> T>(P, F);

impl<P: Parser, T, F: FnOnce(P::Output) -> T> Parser for MapParser<P, T, F> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (rest, output) = self.0.parse(input)?;
        Ok((rest, self.1(output)))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TryMapParser<P: Parser, T, F: FnOnce(P::Output) -> Result<T, ParsingError>>(P, F);

impl<P: Parser, T, F: FnOnce(P::Output) -> Result<T, ParsingError>> Parser
    for TryMapParser<P, T, F>
{
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (rest, output) = self.0.parse(input)?;
        Ok((rest, self.1(output)?))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OrParser<O, L: Parser<Output = O>, R: Parser<Output = O>>(pub L, pub R);
#[derive(Debug, Clone, Copy)]
pub struct AndParser<A: Parser, B: Parser>(pub A, pub B);
#[derive(Debug, Clone, Copy)]
pub struct LeftParser<L: Parser, R: Parser>(pub L, pub R);
#[derive(Debug, Clone, Copy)]
pub struct RightParser<A: Parser, B: Parser>(pub A, pub B);

impl<O, L: Parser<Output = O>, R: Parser<Output = O>> Parser for OrParser<O, L, R> {
    type Output = O;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let err0 = match self.0.parse(input) {
            Ok(output) => return Ok(output),
            Err(err) => err,
        };
        let err1 = match self.1.parse(input) {
            Ok(output) => return Ok(output),
            Err(err) => err,
        };
        Err(if err0.at > err1.at { err0 } else { err1 })
    }
}

impl<A: Parser, B: Parser> Parser for AndParser<A, B> {
    type Output = (A::Output, B::Output);

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (input, a) = self.0.parse(input)?;
        let (input, b) = self.1.parse(input)?;

        Ok((input, (a, b)))
    }
}

impl<L: Parser, R: Parser> Parser for LeftParser<L, R> {
    type Output = L::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (input, output) = self.0.parse(input)?;
        let (input, _) = self.1.parse(input)?;

        Ok((input, output))
    }
}

impl<L: Parser, R: Parser> Parser for RightParser<L, R> {
    type Output = R::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (input, _) = self.0.parse(input)?;
        let (input, output) = self.1.parse(input)?;

        Ok((input, output))
    }
}

use super::{ops::ParserCombinator, Input};
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

    fn map<T, F: FnOnce(Self::Output) -> Result<T, ParsingError>>(
        self,
        f: F,
    ) -> MapParser<Self, T, F>
    where
        Self: Sized,
    {
        MapParser(self, f)
    }
}

pub struct PureParser<T>(pub T);

impl<T> Parser for PureParser<T> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        Ok((input, self.0))
    }
}

pub struct MapParser<P: Parser, T, F: FnOnce(P::Output) -> Result<T, ParsingError>>(P, F);

impl<P: Parser, T, F: FnOnce(P::Output) -> Result<T, ParsingError>> Parser for MapParser<P, T, F> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let (rest, output) = self.0.parse(input)?;
        Ok((rest, self.1(output)?))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Either<L, R> {
    L(L),
    R(R),
}
pub struct EitherParser<L: Parser, R: Parser>(pub L, pub R);
pub struct OrParser<P: Parser>(pub P, pub P);
pub struct AndParser<A: Parser, B: Parser>(pub A, pub B);
pub struct LeftParser<L: Parser, R: Parser>(pub L, pub R);
pub struct RightParser<A: Parser, B: Parser>(pub A, pub B);

impl<L: Parser, R: Parser> Parser for EitherParser<L, R> {
    type Output = Either<L::Output, R::Output>;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
        let err0 = match self.0.parse(input) {
            Ok((rest, output)) => return Ok((rest, Either::L(output))),
            Err(err) => err,
        };
        let err1 = match self.1.parse(input) {
            Ok((rest, output)) => return Ok((rest, Either::R(output))),
            Err(err) => err,
        };
        Err(if err0.at > err1.at { err0 } else { err1 })
    }
}

impl<P: Parser> Parser for OrParser<P> {
    type Output = P::Output;

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

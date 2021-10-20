use std::ops;

use crate::{Error, Input, Parser, Span};

#[derive(Debug, Clone, Copy)]
pub struct ParserCombinator<P: Parser>(pub P);

impl<P: Parser> Parser for ParserCombinator<P> {
    type Output = P::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        self.0.parse(input)
    }
}

impl<L: Parser + Sized, R: Parser + Sized> ops::Rem<R> for ParserCombinator<L> {
    type Output = ParserCombinator<R>;

    fn rem(self, rhs: R) -> Self::Output {
        ParserCombinator(rhs)
    }
}

impl<L: Parser + Sized, R: Parser + Sized> ops::Shl<R> for ParserCombinator<L> {
    type Output = ParserCombinator<LeftParser<L, R>>;

    fn shl(self, rhs: R) -> Self::Output {
        ParserCombinator(LeftParser(self.0, rhs))
    }
}

impl<L: Parser + Sized, R: Parser + Sized> ops::Shr<R> for ParserCombinator<L> {
    type Output = ParserCombinator<RightParser<L, R>>;

    fn shr(self, rhs: R) -> Self::Output {
        ParserCombinator(RightParser(self.0, rhs))
    }
}

impl<L: Parser + Sized, R: Parser + Sized> ops::Add<R> for ParserCombinator<L> {
    type Output = ParserCombinator<AndParser<L, R>>;

    fn add(self, rhs: R) -> Self::Output {
        ParserCombinator(AndParser(self.0, rhs))
    }
}

impl<O, L: Parser<Output = O> + Sized, R: Parser<Output = O> + Sized> ops::BitOr<R>
    for ParserCombinator<L>
{
    type Output = ParserCombinator<OrParser<O, L, R>>;

    fn bitor(self, rhs: R) -> Self::Output {
        ParserCombinator(OrParser(self.0, rhs))
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (input, a) = self.0.parse(input)?;
        let (input, b) = self.1.parse(input)?;

        Ok((input, (a, b)))
    }
}

impl<L: Parser, R: Parser> Parser for LeftParser<L, R> {
    type Output = L::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (input, output) = self.0.parse(input)?;
        let (input, _) = self.1.parse(input)?;

        Ok((input, output))
    }
}

impl<L: Parser, R: Parser> Parser for RightParser<L, R> {
    type Output = R::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (input, _) = self.0.parse(input)?;
        let (input, output) = self.1.parse(input)?;

        Ok((input, output))
    }
}

pub struct PureParser<T>(pub T);

impl<T> Parser for PureParser<T> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        Ok((input, self.0))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MapParser<P: Parser, T, F: FnOnce(P::Output) -> T>(pub P, pub F);

impl<P: Parser, T, F: FnOnce(P::Output) -> T> Parser for MapParser<P, T, F> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (rest, output) = self.0.parse(input)?;
        Ok((rest, self.1(output)))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TryMapParser<P: Parser, T, F: FnOnce(P::Output) -> Result<T, Error>>(pub P, pub F);

impl<P: Parser, T, F: FnOnce(P::Output) -> Result<T, Error>> Parser for TryMapParser<P, T, F> {
    type Output = T;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (rest, output) = self.0.parse(input)?;
        Ok((rest, self.1(output)?))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OptionalParser<P: Parser>(pub P);

impl<P: Parser> Parser for OptionalParser<P> {
    type Output = Option<P::Output>;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        match self.0.parse(input) {
            Ok((rest, output)) => Ok((rest, Some(output))),
            Err(_) => Ok((input, None)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct WhenParser<P: Parser>(pub P, pub bool);

impl<P: Parser> Parser for WhenParser<P> {
    type Output = P::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        if !self.1 {
            Err(Error::new(Span::first(&input)))
        } else {
            self.0.parse(input)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SepByParser<P: Parser, S: Parser> {
    pub parser: P,
    pub separator: S,
}

impl<P: Parser + Clone, S: Parser + Clone> Parser for SepByParser<P, S> {
    type Output = Vec<P::Output>;

    fn parse<'i>(self, mut input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let mut elements = vec![];

        while let Ok((rest, element)) = self.parser.clone().parse(input) {
            input = rest;
            elements.push(element);
            if let Ok((rest, _)) = self.separator.clone().parse(input) {
                input = rest;
            } else {
                break;
            }
        }

        Ok((input, elements))
    }
}

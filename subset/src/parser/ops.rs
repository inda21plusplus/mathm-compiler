use std::ops;

use crate::error::ParsingError;

use super::{
    parser::{LeftParser, RightParser},
    AndParser, EitherParser, Input, OrParser, Parser,
};

pub struct ParserCombinator<P: Parser>(pub P);

impl<P: Parser> Parser for ParserCombinator<P> {
    type Output = P::Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), ParsingError> {
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

impl<P: Parser + Sized> ops::BitOr<P> for ParserCombinator<P> {
    type Output = ParserCombinator<OrParser<P>>;

    fn bitor(self, rhs: P) -> Self::Output {
        ParserCombinator(OrParser(self.0, rhs))
    }
}

impl<L: Parser + Sized, R: Parser + Sized> ops::Div<R> for ParserCombinator<L> {
    type Output = ParserCombinator<EitherParser<L, R>>;

    fn div(self, rhs: R) -> Self::Output {
        ParserCombinator(EitherParser(self.0, rhs))
    }
}

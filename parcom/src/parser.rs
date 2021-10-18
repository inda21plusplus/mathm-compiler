use crate::{
    combinators::{MapParser, OptionalParser, ParserCombinator, TryMapParser},
    Error, Input,
};

pub trait Parser {
    type Output;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error>;

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

    fn try_map<T, F: FnOnce(Self::Output) -> Result<T, Error>>(
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

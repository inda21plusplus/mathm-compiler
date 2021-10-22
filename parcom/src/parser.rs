use crate::{
    combinators::{
        MapParser, OptionalParser, ParserCombinator, SepByParser, TryMapParser, WhenParser,
    },
    Error, Input,
};

pub trait Parser {
    type Output;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error>;

    /// Because of orphan rules, operators cannot be implemented for any `Parser`, so this must be
    /// used to wrap Parsers in a `ParserCombinator`
    fn c(self) -> ParserCombinator<Self>
    where
        Self: Sized,
    {
        ParserCombinator(self)
    }

    fn map<T, F>(self, f: F) -> MapParser<Self, T, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> T,
    {
        MapParser(self, f)
    }

    fn try_map<T, F>(self, f: F) -> TryMapParser<Self, T, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> Result<T, Error>,
    {
        TryMapParser(self, f)
    }

    fn optional(self) -> OptionalParser<Self>
    where
        Self: Sized,
    {
        OptionalParser(self)
    }

    fn when(self, when: bool) -> WhenParser<Self>
    where
        Self: Sized,
    {
        WhenParser(self, when)
    }

    fn sep_by<S>(self, separator: S) -> SepByParser<Self, S>
    where
        Self: Sized + Clone,
        S: Parser + Clone,
    {
        SepByParser {
            parser: self,
            separator,
        }
    }
}

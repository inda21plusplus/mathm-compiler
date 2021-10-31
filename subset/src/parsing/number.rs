use parcom::{
    parsers::{CharParser, IntegerParser},
    Error, Input, Parser, Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub span: Span,
    pub base: u32,
    pub value: u128,
    // todo: type suffixes
}

#[derive(Debug, Clone, Copy)]
pub struct IntegerLiteralParser;

impl Parser for IntegerLiteralParser {
    type Output = IntegerLiteral;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        let start = input.location;

        let prefix_parser = (CharParser('0').c()
            >> (CharParser('b').map(|_| 2).c()
                | CharParser('x').map(|_| 16)
                | CharParser('o').map(|_| 8)))
        .optional()
        .map(|base| base.unwrap_or(10u32));

        let (input, base) = prefix_parser.parse(input)?;

        let (input, value) = IntegerParser { base }.parse(input)?;

        let (input, exponent) = (CharParser('e').c() >> IntegerParser { base })
            .optional()
            .map(|exponent| exponent.unwrap_or(0))
            .parse(input)?;

        if exponent > u32::MAX.into() {
            return Err(Error::new(Span::new(start..input.location)));
        }
        let value = value
            * (base as u128)
                .checked_pow(exponent as u32)
                .ok_or_else(|| Error::new(Span::new(start..input.location)))?;

        if input.next().map(|(_, next_char)| next_char.is_alphabetic()) == Some(true) {
            return Err(Error::new(Span::first(&input)));
        }

        Ok((
            input,
            Self::Output {
                span: Span::new(start..input.location),
                base,
                value,
            },
        ))
    }
}

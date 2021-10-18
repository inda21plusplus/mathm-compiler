use parcom::{
    parsers::{CharParser, IntegerParser},
    Error, Input, Parser, Span,
};

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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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
                .ok_or(Error::new(Span::new(start..input.location)))?;

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

// #[derive(Debug, Clone, Copy)]
// pub struct FloatParser;

// impl Parser for FloatParser {
//     type Output = f64;

//     fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
//         todo!("{:?}", input)
//     }
// }

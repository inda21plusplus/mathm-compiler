mod expr;
mod number;
mod stmt;
mod string;
mod type_;

use parcom::{parsers::PredicateParser, Error, Input, Parser, Span};
pub use stmt::Stmt;

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Copy)]
pub struct IdentifierParser;

impl Parser for IdentifierParser {
    type Output = Identifier;

    fn parse<'i>(self, original_input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (mut input, mut span) = PredicateParser(char::is_alphabetic).parse(original_input)?;
        let mut name = original_input[span].to_string();
        while let Ok((rest, char_span)) = PredicateParser(char::is_alphanumeric).parse(input) {
            input = rest;
            span = span.merge(char_span);
            name.push_str(&original_input[char_span]);
        }

        if ["if", "else", "null"].contains(&name.as_ref()) {
            Err(Error::new(Span::first(&original_input)))
        } else {
            Ok((input, Identifier { span, name }))
        }
    }
}

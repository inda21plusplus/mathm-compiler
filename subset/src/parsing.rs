pub mod expr;
pub mod number;
pub mod stmt;
pub mod string;
pub mod type_;

use parcom::{
    parsers::{EofParser, PredicateParser, Ws},
    Error, Input, Parser, Span,
};

pub use expr::Expr;
pub use stmt::Stmt;
pub use type_::Type;

#[derive(Debug, Clone)]
pub struct Module {
    pub stmts: Vec<Stmt>,
}

impl Module {
    pub fn parse(input: Input) -> Result<Self, Error> {
        (Ws.c() >> Stmt::parser().sep_by(Ws) << EofParser)
            .map(|stmts| Self { stmts })
            .parse(input)
            .map(|(_, module)| module)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Copy)]
pub struct IdentifierParser;

impl Parser for IdentifierParser {
    type Output = Identifier;

    fn parse(self, original_input: Input) -> Result<(Input, Self::Output), Error> {
        let (mut input, mut span) = PredicateParser(char::is_alphabetic).parse(original_input)?;
        let mut name = original_input[span].to_string();
        while let Ok((rest, char_span)) =
            PredicateParser(|c| c.is_alphanumeric() || c == '_').parse(input)
        {
            input = rest;
            span = span.merge(char_span);
            name.push_str(&original_input[char_span]);
        }

        if [
            "if", "else", "null", "break", "return", "fn", "true", "false", "and", "or", "let",
        ]
        .contains(&name.as_ref())
        {
            Err(Error::new(span))
        } else {
            Ok((input, Identifier { span, name }))
        }
    }
}

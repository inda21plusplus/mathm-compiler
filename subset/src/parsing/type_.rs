use parcom::{
    parsers::{CharParser, StrParser},
    Parser, Span,
};

use super::Identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Usize(Span),
    Bool(Span),
    String(Span),
    Null(Span),
    Reference(Reference),
}

impl Type {
    pub fn span(&self) -> Span {
        match *self {
            Self::Usize(u) => u,
            Self::Bool(b) => b,
            Self::String(s) => s,
            Self::Null(n) => n,
            Self::Reference(ref r) => r.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeParser;

impl Parser for TypeParser {
    type Output = Type;

    fn parse(self, input: parcom::Input) -> Result<(parcom::Input, Self::Output), parcom::Error> {
        (StrParser("usize").map(Type::Usize).c()
            | StrParser("bool").map(Type::Bool)
            | StrParser("string").map(Type::String)
            | StrParser("null").map(Type::Null)
            | ReferenceParser.map(Type::Reference))
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DotAccess {
    pub span: Span,
    pub left: Box<Type>,
    pub ident: Identifier,
}

#[derive(Debug, Clone, Copy)]
pub struct DotAccessParser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference {
    pub span: Span,
    pub to: Box<Type>,
    // todo: add optional, owned, maybe more
}

#[derive(Debug, Clone, Copy)]
pub struct ReferenceParser;

impl Parser for ReferenceParser {
    type Output = Reference;

    fn parse(self, input: parcom::Input) -> Result<(parcom::Input, Self::Output), parcom::Error> {
        (CharParser('&').c() + TypeParser.map(Box::new))
            .map(|(amp_span, to)| Reference {
                span: amp_span.merge(to.span()),
                to,
            })
            .parse(input)
    }
}

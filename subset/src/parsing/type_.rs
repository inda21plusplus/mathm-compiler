use parcom::{
    parsers::{CharParser, Ws},
    Parser, Span,
};

use super::{Identifier, IdentifierParser};

#[derive(Debug, Clone)]
pub enum Type {
    Named(Identifier),
    DotAccess(DotAccess),
    Reference(Reference),
    Struct(Struct),
    // List(List),
    // Function(Function),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Self::Named(i) => i.span,
            Self::DotAccess(d) => d.span,
            Self::Reference(r) => r.span,
            Self::Struct(s) => s.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeParser;

impl Parser for TypeParser {
    type Output = Type;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        let (mut input, mut type_) = (IdentifierParser.map(Type::Named).c()
            | ReferenceParser.map(Type::Reference)
            | StructParser.map(Type::Struct))
        .parse(input)?;

        while let Ok((rest, _)) = (Ws.c() + CharParser('.') + Ws).parse(input) {
            let (rest, ident) = IdentifierParser.parse(rest)?;
            input = rest;
            type_ = Type::DotAccess(DotAccess {
                span: type_.span().merge(ident.span),
                left: Box::new(type_),
                ident,
            });
        }

        Ok((input, type_))
    }
}

#[derive(Debug, Clone)]
pub struct DotAccess {
    pub span: Span,
    pub left: Box<Type>,
    pub ident: Identifier,
}

#[derive(Debug, Clone, Copy)]
pub struct DotAccessParser;

#[derive(Debug, Clone)]
pub struct Reference {
    pub span: Span,
    pub to: Box<Type>,
    // todo: add optional, owned, maybe more
}

#[derive(Debug, Clone, Copy)]
pub struct ReferenceParser;

impl Parser for ReferenceParser {
    type Output = Reference;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        (CharParser('&').c() + TypeParser.map(Box::new))
            .map(|(amp_span, to)| Reference {
                span: amp_span.merge(to.span()),
                to,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub span: Span,
    pub types: Vec<StructType>,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub span: Span,
    pub key: Option<Identifier>,
    pub type_: Type,
}

#[derive(Debug, Clone, Copy)]
pub struct StructParser;

impl Parser for StructParser {
    type Output = Struct;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        ((CharParser('(').c() << Ws)
            + StructTypeParser.sep_by(Ws.c() + CharParser(',') + Ws)
            + (Ws.c() >> CharParser(')')))
        .map(|((lparen_span, types), rparen_span)| Struct {
            span: lparen_span.merge(rparen_span),
            types,
        })
        .parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructTypeParser;

impl Parser for StructTypeParser {
    type Output = StructType;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        ((IdentifierParser.map(Some).c() + (Ws.c() >> TypeParser)).c()
            | TypeParser.map(|t| (None, t)))
        .map(|(key, type_)| StructType {
            span: key
                .as_ref()
                .map_or(type_.span(), |k| k.span)
                .merge(type_.span()),
            key,
            type_,
        })
        .parse(input)
    }
}

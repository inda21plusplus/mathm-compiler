use parcom::{
    parsers::{StrParser, Ws},
    Error, Input, Parser, Span,
};

use super::{
    expr::{Block, BlockParser, Expr},
    type_::{Struct, StructParser, Type, TypeParser},
    Identifier, IdentifierParser,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(Let),
    TypeDecl(TypeDecl),
    Assign(Assignment),
    Function(Function),
}

impl Stmt {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        LetParser.map(Self::Let).c()
            | FunctionParser.map(Self::Function)
            | TypeDecl::parser().map(Self::TypeDecl)
            | AssignmentParser.map(Self::Assign)
            | Expr::parser(0).map(Self::Expr)
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type, // todo: make optional
    pub value: Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct LetParser;

impl Parser for LetParser {
    type Output = Let;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        ((((StrParser("let").c() + IdentifierParser << Ws) + TypeParser)
            << Ws
            << StrParser("=")
            << Ws)
            + Expr::parser(0))
        .map(|(((let_span, ident), type_), value)| Let {
            span: let_span.merge(value.span()),
            ident,
            type_,
            value,
        })
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub span: Span,
    pub left: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct AssignmentParser;

impl Parser for AssignmentParser {
    type Output = Assignment;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        ((Expr::parser(0).c() << Ws << StrParser("=") << Ws) + Expr::parser(0))
            .map(|(left, value)| Assignment {
                span: left.span().merge(value.span()),
                left,
                value,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub ident: Identifier,
    pub params: Struct,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionParser;

impl Parser for FunctionParser {
    type Output = Function;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((((StrParser("fn").c() << Ws) + IdentifierParser + StructParser) << Ws)
            + (TypeParser.c() << Ws)
            + BlockParser)
            .map(
                |((((fn_span, ident), params), return_type), body)| Function {
                    span: fn_span.merge(body.span),
                    ident,
                    params,
                    return_type,
                    body,
                },
            )
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type,
}

impl TypeDecl {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        ((StrParser("type").c() << Ws) + (IdentifierParser.c() << Ws) + TypeParser).map(
            |((type_span, ident), type_)| TypeDecl {
                span: type_span.merge(type_.span()),
                ident,
                type_,
            },
        )
    }
}

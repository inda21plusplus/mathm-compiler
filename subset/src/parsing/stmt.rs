use parcom::{
    parsers::{StrParser, Ws},
    Parser, Span,
};

use super::{
    expr::Expr,
    type_::{Type, TypeParser},
    Identifier, IdentifierParser,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(VarDeclaration),
    Assign(Assignment),
}

impl Stmt {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        VarDeclParser.map(Self::VarDecl).c()
            | AssignmentParser.map(Self::Assign)
            | Expr::parser(0).map(Self::Expr)
    }
}

#[derive(Debug, Clone)]
pub struct VarDeclaration {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Option<Type>,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct VarDeclParser;

impl Parser for VarDeclParser {
    type Output = VarDeclaration;

    fn parse<'i>(
        self,
        input: parcom::Input<'i>,
    ) -> Result<(parcom::Input<'i>, Self::Output), parcom::Error> {
        ((((IdentifierParser.c() << Ws) + TypeParser.optional()) << Ws << StrParser("<-") << Ws)
            + Expr::parser(0))
        .map(|((ident, type_), value)| VarDeclaration {
            span: ident.span.merge(value.span()),
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
        ((Expr::parser(0).c() << Ws << StrParser(":=") << Ws) + Expr::parser(0))
            .map(|(left, value)| Assignment {
                span: left.span().merge(value.span()),
                left,
                value,
            })
            .parse(input)
    }
}

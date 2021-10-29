use parcom::{
    parsers::{CharParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

use super::{
    expr::{Block, BlockParser},
    type_::{Struct, StructParser, Type, TypeParser},
    Expr, Identifier, IdentifierParser,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprStmt),
    Let(Let),
    TypeDef(TypeDef),
    Assign(Assignment),
    Function(Function),
}

impl Stmt {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        LetParser.map(Self::Let).c()
            | FunctionParser.map(Self::Function)
            | TypeDef::parser().map(Self::TypeDef)
            | AssignmentParser.map(Self::Assign)
            | ExprStmt::parser().map(Self::Expr)
    }
    pub fn span(&self) -> Span {
        match self {
            Self::Expr(e) => e.span,
            Self::Let(s) => s.span,
            Self::TypeDef(s) => s.span,
            Self::Assign(s) => s.span,
            Self::Function(s) => s.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub span: Span,
    pub expr: Expr,
    pub semi: bool,
}

impl ExprStmt {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        (Expr::parser(0).c() + (Ws.c() >> CharParser(';').optional())).map(|(expr, semi_span)| {
            Self {
                span: expr.span().merge_optional(semi_span),
                expr,
                semi: semi_span.is_some(),
            }
        })
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

    fn parse(self, input: parcom::Input) -> Result<(parcom::Input, Self::Output), parcom::Error> {
        ((StrParser("let").c() << Ws)
            + (IdentifierParser.c() << Ws)
            + (TypeParser.c() << Ws << StrParser("=") << Ws)
            + (Expr::parser(0).c() << Ws)
            + CharParser(';'))
        .map(|((((let_span, ident), type_), value), semi_span)| Let {
            span: let_span.merge(semi_span),
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

    fn parse(self, input: parcom::Input) -> Result<(parcom::Input, Self::Output), parcom::Error> {
        ((Expr::parser(0).c() << Ws << StrParser("=") << Ws) + Expr::parser(0)
            << Ws
            << CharParser(';'))
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

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
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
pub struct TypeDef {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type,
}

impl TypeDef {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        ((StrParser("type").c() << Ws) + (IdentifierParser.c() << Ws) + TypeParser).map(
            |((type_span, ident), type_)| TypeDef {
                span: type_span.merge(type_.span()),
                ident,
                type_,
            },
        )
    }
}

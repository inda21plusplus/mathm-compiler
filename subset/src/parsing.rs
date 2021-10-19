use parcom::{
    parsers::{CharParser, PredicateParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

mod number;
mod string;

pub use number::{IntegerLiteral, IntegerLiteralParser};
pub use string::{StringLiteral, StringLiteralParser};

use crate::get_span::GetSpan;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(Identifier),
    DotAccess(DotAccess),
    UnaryOperation(UnaryOperation),
    // BinaryOperation(BinaryOperation),
    // Constuction(Constuction),
    // Evocation(Evocation),

    // If(IfExpr),
    // Loop(LoopExpr),
    // Block(Block),
    // Closure(Closure),
}

mod precedence_levels {
    pub const UNARY_OPERATION: usize = 1;
    pub const DOT_ACCESS: usize = 2;
}

impl Expr {
    pub fn parser(precedence_level: usize) -> impl Parser<Output = Self> {
        UnaryOperationParser
            .when(precedence_level <= precedence_levels::UNARY_OPERATION)
            .map(Self::UnaryOperation)
            .c()
            | DotAccessParser.when(precedence_level < precedence_levels::DOT_ACCESS)
            | Literal::parser().map(Self::Literal)
            | IdentifierParser.map(Self::Ident)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(BoolLiteral),
    Null(NullLiteral),
    Integer(number::IntegerLiteral),
    Str(StringLiteral),
}

impl Literal {
    pub fn parser() -> impl Parser<Output = Self> {
        BoolLiteral::parser().map(Self::Bool).c()
            | NullLiteral::parser().map(Self::Null)
            | IntegerLiteralParser.map(Self::Integer)
            | StringLiteralParser.map(Self::Str)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    pub span: Span,
    pub value: bool,
}

impl BoolLiteral {
    pub fn parser() -> impl Parser<Output = Self> {
        let new = |value| move |span| Self { span, value };
        StrParser("true").map(new(true)).c() | StrParser("false").map(new(false))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NullLiteral {
    pub span: Span,
}

impl NullLiteral {
    pub fn parser() -> impl Parser<Output = Self> {
        StrParser("null").map(|span| Self { span })
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

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

        Ok((input, Identifier { span, name }))
    }
}

#[derive(Debug, Clone)]
pub struct DotAccess {
    pub span: Span,
    pub left: Box<Expr>,
    pub ident: Identifier,
}

#[derive(Debug, Clone, Copy)]
pub struct DotAccessParser;

impl Parser for DotAccessParser {
    type Output = Expr;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (mut input, mut expr) = Expr::parser(precedence_levels::DOT_ACCESS).parse(input)?;

        while let Ok((rest, ident)) =
            (Ws.c() >> CharParser('.') >> Ws >> IdentifierParser).parse(input)
        {
            input = rest;
            expr = Expr::DotAccess(DotAccess {
                span: expr.span().merge(ident.span),
                left: Box::new(expr),
                ident,
            });
        }

        Ok((input, expr))
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub span: Span,
    pub op: UnaryOperatorKind,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Neg,
    Not,
    Ref,
    Deref,
}

#[derive(Debug, Clone, Copy)]
pub struct UnaryOperationParser;

impl Parser for UnaryOperationParser {
    type Output = UnaryOperation;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((CharParser('-')
            .map(|span| (span, UnaryOperatorKind::Neg))
            .c()
            | CharParser('¬').map(|span| (span, UnaryOperatorKind::Not))
            | CharParser('&').map(|span| (span, UnaryOperatorKind::Ref))
            | CharParser('*').map(|span| (span, UnaryOperatorKind::Deref)))
            + Expr::parser(precedence_levels::UNARY_OPERATION).map(Box::new))
        .map(|((span, op), expr)| UnaryOperation {
            span: span.merge(expr.span()),
            op,
            expr,
        })
        .parse(input)
    }
}

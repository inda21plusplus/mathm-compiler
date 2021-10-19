use parcom::{parsers::StrParser, Parser, Span};

mod number;
mod string;

pub use number::{IntegerLiteral, IntegerLiteralParser};
pub use string::{StringLiteral, StringLiteralParser};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    // BinaryOperation(BinaryOperation),
    // UnaryOperation(UnaryOperation),
    // Evocation(Evocation),
    // DotAccess(DotAccess),
    // Constuct(Constuct),
    // If(IfExpr),
    // Loop(LoopExpr),
    // Block(Block),
    // Closure(Closure),
}

impl Expr {
    pub fn parser() -> impl Parser<Output = Self> {
        Literal::parser().map(Self::Literal)
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
    pub fn parser() -> impl Parser<Output = Self> + Copy {
        let new = |value| move |span| Self { span, value };
        StrParser("true").map(new(true)).c() | StrParser("false").map(new(false))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NullLiteral {
    pub span: Span,
}

impl NullLiteral {
    pub fn parser() -> impl Parser<Output = Self> + Copy {
        StrParser("null").map(|span| Self { span })
    }
}

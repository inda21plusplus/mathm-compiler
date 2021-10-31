use core::fmt;

use parcom::{
    parsers::{CharParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

use super::Stmt;
use super::{
    number::{IntegerLiteral, IntegerLiteralParser},
    IdentifierParser,
};
use super::{
    string::{StringLiteral, StringLiteralParser},
    Identifier,
};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(Identifier),
    UnaryOperation(UnaryOperation),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Block(Block),
    If(If),
    Loop(Loop),
    Break(Break),
    Return(Return),
}

mod precedence_levels {
    pub const BINARY_OR_AND: usize = 1;
    pub const BINARY_EQ_NEQ: usize = 2;
    pub const BINARY_ORDERING: usize = 3;
    pub const BINARY_TERMS: usize = 4;
    pub const BINARY_FACTORS: usize = 5;
    pub const BINARY_MOD: usize = 6;
    pub const UNARY_OPERATION: usize = 7;
    pub const CALL: usize = 8;
}

impl Expr {
    pub fn parser(precedence_level: usize) -> impl Parser<Output = Self> + Clone {
        BinaryOperationParser(precedence_level)
            .when(precedence_level < precedence_levels::UNARY_OPERATION)
            .c()
            | UnaryOperationParser
                .when(precedence_level <= precedence_levels::UNARY_OPERATION)
                .map(Self::UnaryOperation)
            | CallParser
                .when(precedence_level < precedence_levels::CALL)
                .map(Self::Call)
            | BlockParser.map(Self::Block)
            | IfParser.map(Self::If)
            | LoopParser.map(Self::Loop)
            | BreakParser.map(Self::Break)
            | ReturnParser.map(Self::Return)
            | IdentifierParser.map(Self::Ident)
            | Literal::parser().map(Self::Literal)
    }
    pub fn span(&self) -> Span {
        match self {
            Self::Literal(l) => l.span(),
            Self::Ident(i) => i.span,
            Self::UnaryOperation(u) => u.span,
            Self::BinaryOperation(b) => b.span,
            Self::Call(c) => c.span,
            Self::Block(b) => b.span,
            Self::If(i) => i.span,
            Self::Loop(l) => l.span,
            Self::Break(b) => b.span,
            Self::Return(r) => r.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(BoolLiteral),
    Null(NullLiteral),
    Integer(IntegerLiteral),
    Str(StringLiteral),
}

impl Literal {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        BoolLiteral::parser().map(Self::Bool).c()
            | NullLiteral::parser().map(Self::Null)
            | IntegerLiteralParser.map(Self::Integer)
            | StringLiteralParser.map(Self::Str)
    }
    pub fn span(&self) -> Span {
        match self {
            Self::Bool(b) => b.span,
            Self::Null(n) => n.span,
            Self::Integer(i) => i.span,
            Self::Str(s) => s.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    pub span: Span,
    pub value: bool,
}

impl BoolLiteral {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        let new = |value| move |span| Self { span, value };
        StrParser("true").map(new(true)).c() | StrParser("false").map(new(false))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NullLiteral {
    pub span: Span,
}

impl NullLiteral {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        StrParser("null").map(|span| Self { span })
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

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        ((CharParser('-')
            .map(|span| (span, UnaryOperatorKind::Neg))
            .c()
            | StrParser("not").map(|span| (span, UnaryOperatorKind::Not))
            | CharParser('&').map(|span| (span, UnaryOperatorKind::Ref))
            | CharParser('*').map(|span| (span, UnaryOperatorKind::Deref)))
            + (Ws.c() >> Expr::parser(precedence_levels::UNARY_OPERATION).map(Box::new)))
        .map(|((span, op), expr)| UnaryOperation {
            span: span.merge(expr.span()),
            op,
            expr,
        })
        .parse(input)
    }
}

impl fmt::Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Neg => "-",
                Self::Not => "!",
                Self::Ref => "&",
                Self::Deref => "*",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub span: Span,
    pub left: Box<Expr>,
    pub op: BinaryOperatorKind,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
#[rustfmt::skip]
pub enum BinaryOperatorKind {
    Add, Sub, Mul, Div, Mod,
    And, Or, Eq, Neq,
    Lt, Leq, Gt, Geq,
}

impl BinaryOperatorKind {
    fn parser() -> impl Parser<Output = Self> {
        CharParser('+').map(|_| Self::Add).c()
            | CharParser('-').map(|_| Self::Sub)
            | CharParser('*').map(|_| Self::Mul)
            | CharParser('/').map(|_| Self::Div)
            | CharParser('%').map(|_| Self::Mod)
            | StrParser("and").map(|_| Self::And)
            | StrParser("or").map(|_| Self::Or)
            | StrParser("==").map(|_| Self::Eq)
            | StrParser("!=").map(|_| Self::Neq)
            | CharParser('<').map(|_| Self::Lt)
            | StrParser("<=").map(|_| Self::Leq)
            | CharParser('>').map(|_| Self::Gt)
            | StrParser(">=").map(|_| Self::Geq)
    }
    fn precedence_level(&self) -> usize {
        use BinaryOperatorKind::*;
        match self {
            Mod => precedence_levels::BINARY_MOD,
            Mul | Div => precedence_levels::BINARY_FACTORS,
            Add | Sub => precedence_levels::BINARY_TERMS,
            Or | And => precedence_levels::BINARY_OR_AND,
            Eq | Neq => precedence_levels::BINARY_EQ_NEQ,
            Lt | Leq | Gt | Geq => precedence_levels::BINARY_ORDERING,
        }
    }
}

impl fmt::Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Mod => "%",
                Self::And => "and",
                Self::Or => "or",
                Self::Eq => "==",
                Self::Neq => "!=",
                Self::Lt => "<",
                Self::Leq => "<=",
                Self::Gt => ">",
                Self::Geq => ">=",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryOperationParser(pub usize);

impl Parser for BinaryOperationParser {
    type Output = Expr;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        let (mut input, mut expr) = Expr::parser(precedence_levels::UNARY_OPERATION)
            .map(Box::new)
            .parse(input)?;

        while let Ok((rest, op)) = (Ws.c() >> BinaryOperatorKind::parser() << Ws).parse(input) {
            if op.precedence_level() <= self.0 {
                break;
            }
            let (rest, right) = Expr::parser(op.precedence_level())
                .map(Box::new)
                .parse(rest)?;
            input = rest;
            expr = Box::new(Expr::BinaryOperation(BinaryOperation {
                span: expr.span().merge(right.span()),
                left: expr,
                op,
                right,
            }));
        }

        Ok((input, *expr))
    }
}

#[derive(Debug, Clone)]
pub struct DotAccess {
    pub span: Span,
    pub left: Box<Expr>,
    pub ident: Identifier,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub span: Span,
    pub func: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallParser;

impl Parser for CallParser {
    type Output = Call;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        let (mut input, mut func) = Expr::parser(precedence_levels::CALL)
            .map(Box::new)
            .parse(input)?;

        while let Ok((rest, ((lparen_span, params), rparen_span))) = (CharParser('(').c()
            + (Ws.c() >> (Expr::parser(0).sep_by(Ws.c() + CharParser(',') + Ws)))
            + (Ws.c() >> CharParser(')')))
        .parse(input)
        {
            input = rest;
            func = Box::new(Expr::Call(Call {
                span: lparen_span.merge(rparen_span),
                func,
                params,
            }));
        }

        match *func {
            Expr::Call(call) => Ok((input, call)),
            _ => Err(Error::new(Span::first(&input))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockParser;

impl Parser for BlockParser {
    type Output = Block;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        let (input, ((lb_s, stmts), rb_s)) =
            ((CharParser('{').c() << Ws) + Stmt::parser().sep_by(Ws) + (Ws.c() >> CharParser('}')))
                .parse(input)?;
        Ok((
            input,
            Block {
                span: lb_s.merge(rb_s),
                stmts,
            },
        ))
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub span: Span,
    pub condition: Box<Expr>,
    pub then: Block,
    pub elze: Option<Block>,
}

#[derive(Debug, Clone, Copy)]
pub struct IfParser;

impl Parser for IfParser {
    type Output = If;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        ((StrParser("if").c() << Ws)
            + (Expr::parser(0).map(Box::new).c() << Ws)
            + (BlockParser.c() << Ws)
            + (StrParser("else").c() >> Ws >> BlockParser).optional())
        .map(|(((if_span, condition), then), elze)| If {
            span: if_span.merge(elze.as_ref().map_or(then.span, |elze| elze.span)),
            condition,
            then,
            elze,
        })
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub span: Span,
    pub block: Block,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopParser;

impl Parser for LoopParser {
    type Output = Loop;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        ((StrParser("loop").c() << Ws) + BlockParser)
            .map(|(loop_span, block)| Loop {
                span: loop_span.merge(block.span),
                block,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Break {
    pub span: Span,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakParser;

impl Parser for BreakParser {
    type Output = Break;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        ((StrParser("break").c() << Ws) + Expr::parser(0).map(Box::new).optional())
            .map(|(break_span, value)| Break {
                span: break_span.merge_optional(value.as_ref().map(|e| e.span())),
                value,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub span: Span,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ReturnParser;

impl Parser for ReturnParser {
    type Output = Return;

    fn parse(self, input: Input) -> Result<(Input, Self::Output), Error> {
        ((StrParser("return").c() << Ws) + Expr::parser(0).map(Box::new).optional())
            .map(|(break_span, value)| Return {
                span: break_span.merge_optional(value.as_ref().map(|e| e.span())),
                value,
            })
            .parse(input)
    }
}

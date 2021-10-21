use parcom::{
    parsers::{CharParser, StrParser, Ws},
    Error, Input, Parser, Span,
};

use super::{
    number::{IntegerLiteral, IntegerLiteralParser},
    type_::TypeParser,
    IdentifierParser,
};
use super::{
    string::{StringLiteral, StringLiteralParser},
    Identifier,
};
use super::{type_::Type, Stmt};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(Identifier),
    DotAccess(DotAccess),
    UnaryOperation(UnaryOperation),
    BinaryOperation(BinaryOperation),
    Constuction(Constuction),
    Block(Block),
    If(If),
    Loop(Loop),
    Break(Break),
    Return(Return),
    Function(Function),
    // // Cast(Cast),
}

mod precedence_levels {
    pub const BINARY_OR_AND: usize = 1;
    pub const BINARY_EQ_NEQ: usize = 2;
    pub const BINARY_ORDERING: usize = 3;
    pub const BINARY_TERMS: usize = 4;
    pub const BINARY_FACTORS: usize = 5;
    pub const BINARY_MOD: usize = 6;
    pub const UNARY_OPERATION: usize = 7;
    pub const SUFFIX: usize = 8; // foo.bar and foo()
}

impl Expr {
    pub fn parser(precedence_level: usize) -> impl Parser<Output = Self> + Clone {
        BinaryOperationParser(precedence_level)
            .when(precedence_level < precedence_levels::UNARY_OPERATION)
            .c()
            | UnaryOperationParser
                .when(precedence_level <= precedence_levels::UNARY_OPERATION)
                .map(Self::UnaryOperation)
            | ConstuctionParenParser.map(Self::Constuction)
            | SuffixParser.when(precedence_level < precedence_levels::SUFFIX)
            | BlockParser.map(Self::Block)
            | IfParser.map(Self::If)
            | LoopParser.map(Self::Loop)
            | BreakParser.map(Self::Break)
            | ReturnParser.map(Self::Return)
            | FunctionParser.map(Self::Function)
            | Literal::parser().map(Self::Literal)
            | IdentifierParser.map(Self::Ident)
    }
    pub fn span(&self) -> Span {
        match self {
            Self::Literal(l) => l.span(),
            Self::Ident(i) => i.span,
            Self::DotAccess(d) => d.span,
            Self::UnaryOperation(u) => u.span,
            Self::BinaryOperation(b) => b.span,
            Self::Constuction(c) => c.span,
            Self::Block(b) => b.span,
            Self::If(i) => i.span,
            Self::Loop(l) => l.span,
            Self::Break(b) => b.span,
            Self::Return(r) => r.span,
            Self::Function(f) => f.span,
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
    fn span(&self) -> Span {
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((CharParser('¯')
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
            | CharParser('×').map(|_| Self::Mul)
            | CharParser('/').map(|_| Self::Div)
            | CharParser('%').map(|_| Self::Mod)
            | CharParser('∧').map(|_| Self::And)
            | CharParser('∨').map(|_| Self::Or)
            | CharParser('=').map(|_| Self::Eq)
            | CharParser('≠').map(|_| Self::Neq)
            | CharParser('<').map(|_| Self::Lt)
            | CharParser('≤').map(|_| Self::Leq)
            | CharParser('>').map(|_| Self::Gt)
            | CharParser('≥').map(|_| Self::Geq)
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

#[derive(Debug, Clone, Copy)]
pub struct BinaryOperationParser(pub usize);

impl Parser for BinaryOperationParser {
    type Output = Expr;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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

// clamp(val <- input, min <- 0, max <- 1)
// clamp(input, 0, 1)
// clamp(<-input, 0, 1)
// Person(name <- "Mathias", age <- 20)
// Person("Mathias", 20)
// Person(<-name, <-age)
// (rest, expr)
// (input <- rest, expr)
// (1 + 2) * 3
// target(param1, param2)
#[derive(Debug, Clone)]
pub struct Constuction {
    pub span: Span,
    pub left: Option<Box<Expr>>,
    pub params: Vec<ConstructionParameter>,
}

#[derive(Debug, Clone)]
pub struct ConstructionParameter {
    pub key: Option<Identifier>,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct SuffixParser;

impl Parser for SuffixParser {
    type Output = Expr;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        let (mut input, mut left) = Expr::parser(precedence_levels::SUFFIX)
            .map(Box::new)
            .parse(input)?;

        loop {
            if let Ok((rest, constr)) = ConstuctionParenParser.parse(input) {
                input = rest;
                left = Box::new(Expr::Constuction(Constuction {
                    span: left.span().merge(constr.span),
                    left: Some(left),
                    params: constr.params,
                }));
            } else if let Ok((rest, ident)) =
                (Ws.c() >> CharParser('.') >> Ws >> IdentifierParser).parse(input)
            {
                input = rest;
                left = Box::new(Expr::DotAccess(DotAccess {
                    span: left.span().merge(ident.span),
                    left,
                    ident,
                }));
            } else {
                break;
            }
        }

        Ok((input, *left))
    }
}

#[derive(Debug, Clone, Copy)]
struct ConstuctionParenParser;

impl Parser for ConstuctionParenParser {
    type Output = Constuction;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((CharParser('(').c()
            + (Ws.c()
                >> (ConstructionParameter::parser()
                    .sep_by(Ws.c() + CharParser(',') + Ws)
                    .c()))
            + (Ws.c() >> CharParser(')')))
        .map(|((lparen_span, params), rparen_span)| Constuction {
            span: lparen_span.merge(rparen_span),
            left: None,
            params,
        }))
        .parse(input)
    }
}

impl ConstructionParameter {
    fn parser() -> impl Parser<Output = Self> + Clone {
        ((IdentifierParser.c() << Ws << StrParser("<-") << Ws) + Expr::parser(0))
            .map(|(key, value)| Self {
                key: Some(key),
                value,
            })
            .c()
            | Expr::parser(0).map(|value| Self { key: None, value })
            | StrParser("<-").c()
                >> Ws
                >> IdentifierParser.map(|key| Self {
                    key: Some(key.clone()),
                    value: Expr::Ident(key),
                })
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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
    pub true_expr: Box<Expr>,
    pub else_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone, Copy)]
pub struct IfParser;

impl Parser for IfParser {
    type Output = If;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((StrParser("if").c() << Ws)
            + (Expr::parser(0).map(Box::new).c() << Ws)
            + (Expr::parser(0).map(Box::new).c() << Ws)
            + (StrParser("else").c() >> Ws >> Expr::parser(0).map(Box::new)).optional())
        .map(|(((if_span, condition), true_expr), else_expr)| If {
            span: if_span.merge(else_expr.as_ref().map_or(true_expr.span(), |eb| eb.span())),
            condition,
            true_expr,
            else_expr,
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

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
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
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct BreakParser;

impl Parser for BreakParser {
    type Output = Break;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((StrParser("break").c() << Ws) + Expr::parser(0).map(Box::new))
            .map(|(break_span, value)| Break {
                span: break_span.merge(value.span()),
                value,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub span: Span,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct ReturnParser;

impl Parser for ReturnParser {
    type Output = Return;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((StrParser("return").c() << Ws) + Expr::parser(0).map(Box::new))
            .map(|(break_span, value)| Return {
                span: break_span.merge(value.span()),
                value,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub params: Vec<FunctionParameter>,
    pub return_type: Type,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type, // todo: make optional
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionParser;

impl Parser for FunctionParser {
    type Output = Function;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((StrParser("fn").c() << Ws << CharParser('(') << Ws)
            + (FunctionParameterParser
                .sep_by(Ws.c() + CharParser(',') + Ws)
                .c()
                << Ws
                << CharParser(')')
                << Ws)
            + (TypeParser.c() << Ws << StrParser("=>") << Ws)
            + Expr::parser(0).map(Box::new))
        .map(|(((fn_span, params), return_type), body)| Function {
            span: fn_span.merge(body.span()),
            params,
            return_type,
            body,
        })
        .parse(input)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionParameterParser;

impl Parser for FunctionParameterParser {
    type Output = FunctionParameter;

    fn parse<'i>(self, input: Input<'i>) -> Result<(Input<'i>, Self::Output), Error> {
        ((IdentifierParser.c() << Ws) + TypeParser)
            .map(|(ident, type_)| FunctionParameter {
                span: ident.span.merge(type_.span()),
                ident,
                type_,
            })
            .parse(input)
    }
}

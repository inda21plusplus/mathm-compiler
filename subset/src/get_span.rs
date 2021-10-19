use parcom::Span;

use crate::parsing::{Expr, Literal};

pub trait GetSpan {
    fn span(&self) -> Span;
}

impl GetSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Literal(l) => l.span(),
            Self::Ident(i) => i.span,
            Self::DotAccess(d) => d.span,
            Self::UnaryOperation(u) => u.span,
            Self::BinaryOperation(b) => b.span,
        }
    }
}

impl GetSpan for Literal {
    fn span(&self) -> Span {
        match self {
            Self::Bool(b) => b.span,
            Self::Null(n) => n.span,
            Self::Integer(i) => i.span,
            Self::Str(s) => s.span,
        }
    }
}

use parcom::Span;

use crate::parsing::{BoolLiteral, Expr, IntegerLiteral, Literal, NullLiteral, StringLiteral};

pub trait GetSpan {
    fn span(&self) -> Span;
}

impl GetSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Literal(l) => l.span(),
        }
    }
}

impl GetSpan for Literal {
    fn span(&self) -> Span {
        match self {
            Self::Bool(b) => b.span(),
            Self::Null(n) => n.span(),
            Self::Integer(i) => i.span(),
            Self::Str(s) => s.span(),
        }
    }
}

impl GetSpan for BoolLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl GetSpan for NullLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl GetSpan for IntegerLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

impl GetSpan for StringLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

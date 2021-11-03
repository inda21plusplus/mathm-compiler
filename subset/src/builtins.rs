use crate::parsing::Identifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[rustfmt::skip]
pub enum Builtin {
    Not,
    Plus, Minus, Asterisk, Slash, Percent,
    And, Or, Eq, Neq,
    Lt, Leq, Gt, Geq,

    Print,
}

impl Builtin {
    pub fn lookup(ident: &Identifier) -> Option<Builtin> {
        match ident.name.as_ref() {
            "not" => Some(Self::Not),
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "*" => Some(Self::Asterisk),
            "/" => Some(Self::Slash),
            "%" => Some(Self::Percent),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "==" => Some(Self::Eq),
            "!=" => Some(Self::Neq),
            "<" => Some(Self::Lt),
            "<=" => Some(Self::Leq),
            ">" => Some(Self::Gt),
            ">=" => Some(Self::Geq),
            "print" => Some(Self::Print),
            _ => None,
        }
    }
}

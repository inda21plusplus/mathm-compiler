use crate::parsing::Identifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Plus,
}

impl Builtin {
    pub fn lookup(ident: &Identifier) -> Option<Builtin> {
        match ident.name.as_ref() {
            "+" => Some(Self::Plus),
            _ => None,
        }
    }
}

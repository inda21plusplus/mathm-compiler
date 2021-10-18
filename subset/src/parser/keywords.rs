use super::Span;

pub struct Keyword {
    at: Span,
    kind: KeywordKind,
}

pub enum KeywordKind {
    Bool(bool),
}

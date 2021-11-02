use std::collections::HashMap;

use parcom::Span;

use crate::{
    parsing::{
        expr::BoolLiteral, number::IntegerLiteral, string::StringLiteral, Identifier, Module, Type,
    },
    Builtin,
};

mod hirgen;

/// High-level IR. Stack based. Based.
#[derive(Debug, Clone)]
pub struct Hir {
    pub symbols: Vec<Symbol>,
}

impl Hir {
    pub fn generate(module: Module) -> Result<Self, Vec<hirgen::HirGenError>> {
        hirgen::HirGen::default().generate(module)
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(Variable),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type,
    pub body: BasicBlock,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub ident: Identifier,
    pub params: Vec<(Identifier, Type)>,
    pub ret_type: Type,
    pub locals: Vec<Local>,
    pub body: HashMap<BasicBlockId, BasicBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type,
}

// Last instruction must be something that moves execution elsewhere is functions. Blocks are not
// ordered so there must be no fall-through.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
}

/// The block with id `BasicBlockId::default()` is always executed first
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BasicBlockId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    IntegerLiteral(IntegerLiteral),
    NullLiteral(Span),
    StringLiteral(StringLiteral),
    BoolLiteral(BoolLiteral),
    Push(Resolved),
    Save(Resolved),
    Call(Span, usize),
    Branch(BasicBlockId, BasicBlockId),
    Jump(BasicBlockId),
    Return,
    Discard(usize),
    Invalid { stack_delta: isize },
}

impl Instruction {
    pub fn stack_delta(&self) -> isize {
        match *self {
            Self::IntegerLiteral(_)
            | Self::NullLiteral(_)
            | Self::StringLiteral(_)
            | Self::BoolLiteral(_)
            | Self::Push(_) => 1,
            Self::Save(_) => -1,
            Self::Call(_, params) => -(params as isize), // removes params, removes ident, pushes return value onto stack
            Self::Branch(_, _) => -1,
            Self::Jump(_) | Self::Return => 0,
            Self::Invalid { stack_delta } => stack_delta,
            Self::Discard(amount) => -(amount as isize),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolved {
    Local(Span, usize),
    Parameter(Span, usize),
    Symbol(Span, usize),
    Builtin(Span, Builtin),
}

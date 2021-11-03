use parcom::Span;

use crate::{
    hir::{BasicBlockId, Instruction, Symbol},
    Builtin,
};

use super::{Hir, Resolved};

pub struct Runner {
    pub hir: Hir,
    frames: Vec<Frame>,
    // symbols: HashMap<usize, Value>,
}

#[derive(Debug, Clone, Default)]
pub struct Frame {
    locals: Vec<Value>,
    parameters: Vec<Value>,
    stack: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct TypedValue {
    type_: Type,
    value: Value,
}

#[derive(Debug, Clone)]
pub enum Type {
    Usize,
    Void,
    String,
    Bool,
    Reference(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone)]
pub enum Value {
    Usize(usize),
    Null, // this is dumb. Null isn't a value
    String(String),
    Bool(bool),
    Builtin(Builtin),
    Reference(Reference),
}

// impl Value {
//     pub fn get_type(&self) -> Type {
//         match self {
//             Self::Usize(_) => Type::Usize,
//             Self::Null => Type::Void,
//             Self::String(_) => Type::String,
//             Self::Bool(_) => Type::Bool,
//             Self::Builtin(_) => todo!(),
//             Self::Reference(_) => todo!(),
//         }
//     }
// }

#[derive(Debug, Clone)]
enum Reference {
    Local { frame: usize, index: usize },
    Parameter { frame: usize, index: usize },
    Symbol { index: usize },
}

impl Runner {
    pub fn new(hir: Hir) -> Self {
        Self {
            hir,
            frames: vec![],
        }
    }

    pub fn run(&mut self) {
        // TODO: run initializers for global variables
        let main = match self.hir.entypoint_index {
            Some(main) => main,
            None => {
                eprintln!("No entypoint found. Nothing to run");
                return;
            }
        };
        self.call(Resolved::Symbol(Span::default(), main));
    }

    pub fn call(&mut self, callee: Resolved) {
        self.frames.push(Frame {
            locals: self
                .frames
                .last()
                .map_or(vec![], |frame| frame.parameters.clone()),
            ..Default::default()
        });
        match callee {
            Resolved::Local(span, _) | Resolved::Parameter(span, _) => {
                eprintln!("Can't call something that's not a function at {}", span)
            }
            Resolved::Symbol(_span, index) => match &self.hir.symbols[index] {
                Symbol::Variable(_) => eprintln!("Can't call something that's not a function"),
                Symbol::Function(_) => self.call_function(index),
            },
            Resolved::Builtin(_span, _builtin) => todo!(),
        }
    }

    pub fn call_function(&mut self, func_index: usize) {
        macro_rules! push {
            ($value:expr) => {
                self.frames.last_mut().unwrap().stack.push($value)
            };
        }
        macro_rules! pop {
            () => {
                self.frames.last_mut().unwrap().stack.pop().unwrap()
            };
        }
        macro_rules! frame {
            () => {
                self.frames.last_mut().unwrap()
            };
        }

        let func = match &self.hir.symbols[func_index] {
            Symbol::Function(func) => func,
            _ => unreachable!(),
        };
        let mut block_id = BasicBlockId::default();
        let mut rip = 0;
        loop {
            match func.body.get(&block_id).unwrap().instructions[rip] {
                Instruction::IntegerLiteral(i) => {
                    push!(Value::Usize(i.value as usize));
                    rip += 1;
                }
                Instruction::NullLiteral(_) => {
                    push!(Value::Null);
                    rip += 1;
                }
                Instruction::StringLiteral(ref s) => {
                    push!(Value::String(s.value.clone()));
                    rip += 1;
                }
                Instruction::BoolLiteral(b) => {
                    push!(Value::Bool(b.value));
                    rip += 1;
                }
                Instruction::Push(Resolved::Local(_, i)) => {
                    let local = frame!().locals[i].clone();
                    push!(local);
                    rip += 1;
                }
                Instruction::Push(Resolved::Parameter(_, i)) => {
                    let param = frame!().parameters[i].clone();
                    push!(param);
                    rip += 1;
                }
                Instruction::Push(Resolved::Symbol(_, _i)) => {
                    todo!()
                }
                Instruction::Push(Resolved::Builtin(_, builtin)) => {
                    push!(Value::Builtin(builtin));
                    rip += 1;
                }
                Instruction::Save(Resolved::Local(_, i)) => {
                    frame!().locals[i] = pop!();
                    rip += 1;
                }
                Instruction::Save(Resolved::Parameter(_, i)) => {
                    let value = pop!();
                    if func.params[i].1 != value.get_type() {
                        panic!("Incorrect types");
                    }
                    frame!().parameters[i] = value;
                    rip += 1;
                }
                Instruction::Save(Resolved::Symbol(_, _i)) => {
                    todo!();
                }
                Instruction::Save(Resolved::Builtin(_, builtin)) => {
                    panic!("Cannot write to built in (function) {:?}", builtin);
                }
                Instruction::PushReference(Resolved::Local(_, index)) => {
                    let frame = self.frames.len() - 1;
                    push!(Value::Reference(Reference::Local { frame, index }));
                }
                Instruction::PushReference(Resolved::Parameter(_, index)) => {
                    let frame = self.frames.len() - 1;
                    push!(Value::Reference(Reference::Parameter { frame, index }));
                }
                Instruction::PushReference(Resolved::Symbol(_, index)) => {
                    push!(Value::Reference(Reference::Symbol { index }));
                }
                Instruction::PushReference(Resolved::Builtin(_, builtin)) => {
                    panic!("Cannot take reference to built in (function) {:?}", builtin);
                }
                Instruction::Call(_span, _param_count) => todo!(),
                Instruction::Branch(span, true_id, false_id) => {
                    let value = pop!();
                    match value {
                        Value::Bool(true) => {
                            block_id = true_id;
                            rip = 0;
                        }
                        Value::Bool(false) => {
                            block_id = false_id;
                            rip = 0;
                        }
                        other => {
                            panic!(
                                "Can only use boolean in if condition, got {:?} at {}",
                                other, span
                            );
                        }
                    }
                }
                Instruction::Jump(id) => {
                    block_id = id;
                    rip = 0;
                }
                Instruction::Return => todo!(),
                Instruction::Discard(count) => {
                    for _ in 0..count {
                        pop!();
                    }
                }
                Instruction::Invalid { .. } => unreachable!(),
            }
        }
    }
}

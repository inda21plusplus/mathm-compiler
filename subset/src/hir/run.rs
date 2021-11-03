use crate::{
    hir::{BasicBlockId, Instruction, Symbol},
    parsing, Builtin,
};

use super::{Hir, Resolved};

pub struct Runner {
    pub hir: Hir,
    frames: Vec<Frame>,
}

#[derive(Debug, Clone, Default)]
pub struct Frame {
    locals: Vec<TypedValue>,
    parameters: Vec<TypedValue>,
    stack: Vec<TypedValue>,
}

#[derive(Debug, Clone)]
pub struct TypedValue(Type, Value);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Usize,
    Void,
    String,
    Bool,
    Reference(Box<Type>),
    Function,
}

impl Type {
    pub fn is(&self, t: &parsing::Type) -> bool {
        match t {
            parsing::Type::Usize(_) => *self == Self::Usize,
            parsing::Type::Bool(_) => *self == Self::Bool,
            parsing::Type::String(_) => *self == Self::String,
            parsing::Type::Void(_) => *self == Self::Void,
            parsing::Type::Reference(r) => match self {
                Self::Reference(to) => to.is(&r.to),
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Uninit,
    Usize(usize),
    Void,
    String(String),
    Bool(bool),
    Builtin(Builtin),
    Reference(Reference),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reference {
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
        self.call(
            TypedValue(
                Type::Function,
                Value::Reference(Reference::Symbol { index: main }),
            ),
            vec![],
        );
    }

    pub fn call(&mut self, callee: TypedValue, arguments: Vec<TypedValue>) -> TypedValue {
        self.frames.push(Frame {
            locals: self
                .frames
                .last()
                .map_or(vec![], |frame| frame.parameters.clone()),
            parameters: arguments,
            ..Default::default()
        });
        let ret = match callee.1 {
            Value::Reference(Reference::Symbol { index }) => self.call_function(index),
            Value::Builtin(builtin) => self.call_builtin(builtin),
            _ => panic!("You cant call {:?}", callee),
        };
        self.frames.pop();
        ret
    }

    fn call_function(&mut self, func_index: usize) -> TypedValue {
        // Learn this one simple trick to excel at Rust!
        // Borrow checker hates him!
        // 10/10 would check again!
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
        macro_rules! func {
            ($i:expr) => {
                match &self.hir.symbols[$i] {
                    Symbol::Function(func) => func,
                    _ => unreachable!(),
                }
            };
        }

        for local in &func!(func_index).locals {
            frame!().locals.push(TypedValue(
                match local.type_ {
                    parsing::Type::Usize(_) => Type::Usize,
                    parsing::Type::Bool(_) => Type::Bool,
                    parsing::Type::String(_) => Type::String,
                    parsing::Type::Void(_) => Type::Void,
                    parsing::Type::Reference(_) => Type::Reference(Box::new(Type::Void)), // TODO
                },
                Value::Uninit,
            ));
        }

        let mut block_id = BasicBlockId::default();
        let mut rip = 0;
        loop {
            match func!(func_index).body.get(&block_id).unwrap().instructions[rip] {
                Instruction::IntegerLiteral(i) => {
                    push!(TypedValue(Type::Usize, Value::Usize(i.value as usize)));
                    rip += 1;
                }
                Instruction::NullLiteral(_) => {
                    push!(TypedValue(Type::Void, Value::Void));
                    rip += 1;
                }
                Instruction::StringLiteral(ref s) => {
                    push!(TypedValue(Type::String, Value::String(s.value.clone())));
                    rip += 1;
                }
                Instruction::BoolLiteral(b) => {
                    push!(TypedValue(Type::Bool, Value::Bool(b.value)));
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
                Instruction::Push(Resolved::Symbol(_, index)) => {
                    push!(TypedValue(
                        Type::Function,
                        Value::Reference(Reference::Symbol { index })
                    ));
                    rip += 1;
                }
                Instruction::Push(Resolved::Builtin(_, builtin)) => {
                    push!(TypedValue(Type::Function, Value::Builtin(builtin)));
                    rip += 1;
                }
                Instruction::Save(Resolved::Local(_, i)) => {
                    frame!().locals[i] = pop!();
                    rip += 1;
                }
                Instruction::Save(Resolved::Parameter(span, i)) => {
                    let value = pop!();
                    if !value.0.is(&func!(func_index).params[i].1) {
                        panic!("Incorrect types at {}", span);
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
                Instruction::PushReference(Resolved::Local(_, _index)) => {
                    todo!()
                    // let frame = self.frames.len() - 1;
                    // push!(TypedValue(
                    //     Type::Reference(),
                    //     Value::Reference(Reference::Local { frame, index })
                    // ));
                }
                Instruction::PushReference(Resolved::Parameter(_, _index)) => {
                    todo!()
                    // let frame = self.frames.len() - 1;
                    // push!(Value::Reference(Reference::Parameter { frame, index }));
                }
                Instruction::PushReference(Resolved::Symbol(_, _index)) => {
                    todo!()
                    // push!(Value::Reference(Reference::Symbol { index }));
                }
                Instruction::PushReference(Resolved::Builtin(_, builtin)) => {
                    panic!("Cannot take reference to built in (function) {:?}", builtin);
                }
                Instruction::Call(_span, param_count) => {
                    let func = pop!();
                    let mut args = vec![];
                    for _ in 0..param_count {
                        args.push(pop!());
                    }
                    let ret_val = self.call(func, args);
                    push!(ret_val);
                    rip += 1;
                }
                Instruction::Branch(span, true_id, false_id) => {
                    let value = pop!();
                    match value.1 {
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
                Instruction::Return => {
                    break pop!();
                }
                Instruction::Discard(count) => {
                    for _ in 0..count {
                        pop!();
                    }
                    rip += 1;
                }
                Instruction::Invalid { .. } => unreachable!(),
            }
        }
    }

    fn call_builtin(&mut self, builtin: Builtin) -> TypedValue {
        let params = &self.frames.last().unwrap().parameters;
        match builtin {
            Builtin::Not => run_builtin_not(params),
            Builtin::Plus => run_builtin_plus(params),
            Builtin::Minus => run_builtin_minus(params),
            Builtin::Asterisk => run_builtin_asterisk(&self.frames),
            Builtin::Slash => run_builtin_slash(params),
            Builtin::Percent => run_builtin_percent(params),
            Builtin::And => run_builtin_and(params),
            Builtin::Or => run_builtin_or(params),
            Builtin::Eq => run_builtin_eq(params),
            Builtin::Neq => run_builtin_neq(params),
            Builtin::Lt => run_builtin_lt(params),
            Builtin::Leq => run_builtin_leq(params),
            Builtin::Gt => run_builtin_gt(params),
            Builtin::Geq => run_builtin_geq(params),
            Builtin::Print => run_builtin_print(params),
        }
    }
}

fn run_builtin_not(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Bool(b))] => TypedValue(Type::Bool, Value::Bool(!b)),
        _ => panic!("Invalid arguments {:?} for !", params),
    }
}

fn run_builtin_plus(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Usize, Value::Usize(a + b))
        }
        _ => panic!("Invalid arguments {:?} for +", params),
    }
}

fn run_builtin_minus(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Usize, Value::Usize(a - b))
        }
        _ => panic!("Invalid arguments {:?} for -", params),
    }
}

fn run_builtin_asterisk(frames: &[Frame]) -> TypedValue {
    match &frames.last().unwrap().parameters[..] {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Usize, Value::Usize(a * b))
        }
        [TypedValue(_, Value::Reference(Reference::Local { frame, index }))] => {
            frames[*frame].locals[*index].clone()
        }
        [TypedValue(_, Value::Reference(Reference::Parameter { frame, index }))] => {
            frames[*frame].parameters[*index].clone()
        }
        [TypedValue(_, Value::Reference(Reference::Symbol { index: _ }))] => {
            todo!()
        }
        _ => panic!(
            "Invalid arguments {:?} for *",
            &frames.last().unwrap().parameters[..]
        ),
    }
}

fn run_builtin_slash(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Usize, Value::Usize(a / b))
        }
        _ => panic!("Invalid arguments {:?} for /", params),
    }
}

fn run_builtin_percent(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Usize, Value::Usize(a % b))
        }
        _ => panic!("Invalid arguments {:?} for %", params),
    }
}

fn run_builtin_and(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Bool(a)), TypedValue(_, Value::Bool(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a && *b))
        }
        _ => panic!("Invalid arguments {:?} for and", params),
    }
}

fn run_builtin_or(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Bool(a)), TypedValue(_, Value::Bool(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a || *b))
        }
        _ => panic!("Invalid arguments {:?} for or", params),
    }
}

fn run_builtin_eq(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, a), TypedValue(_, b)] => TypedValue(Type::Bool, Value::Bool(*a == *b)),
        _ => panic!("Invalid arguments {:?} for ==", params),
    }
}

fn run_builtin_neq(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, a), TypedValue(_, b)] => TypedValue(Type::Bool, Value::Bool(*a != *b)),
        _ => panic!("Invalid arguments {:?} for !=", params),
    }
}

fn run_builtin_lt(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a < *b))
        }
        _ => panic!("Invalid arguments {:?} for <", params),
    }
}
fn run_builtin_leq(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a <= *b))
        }
        _ => panic!("Invalid arguments {:?} for <=", params),
    }
}
fn run_builtin_gt(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a > *b))
        }
        _ => panic!("Invalid arguments {:?} for >", params),
    }
}
fn run_builtin_geq(params: &[TypedValue]) -> TypedValue {
    match params {
        [TypedValue(_, Value::Usize(a)), TypedValue(_, Value::Usize(b))] => {
            TypedValue(Type::Bool, Value::Bool(*a >= *b))
        }
        _ => panic!("Invalid arguments {:?} for >=", params),
    }
}

fn run_builtin_print(params: &[TypedValue]) -> TypedValue {
    for param in params {
        match param.1 {
            Value::Usize(u) => print!("{}", u),
            Value::Uninit => {
                println!("WARING! UNINITIALIZED! WARING! WARNIGN! WANGING! WARNIGIVGN!")
            }
            Value::Void => print!("()"),
            Value::String(ref s) => print!("{}", s),
            Value::Bool(b) => print!("{}", b),
            Value::Builtin(b) => print!("{:?}", b),
            Value::Reference(_) => {
                print!("&somethingimtoolazytodereferencethiscommonwhyyougottaprintit");
            }
        }
    }
    TypedValue(Type::Void, Value::Void)
}

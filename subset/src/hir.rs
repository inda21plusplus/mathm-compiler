use std::{collections::HashMap, mem, ops};

use parcom::Span;

use crate::{
    parsing::{
        self, expr, number::IntegerLiteral, stmt, string::StringLiteral, Expr, Identifier, Stmt,
        Type,
    },
    Builtin,
};

#[derive(Debug, PartialEq, Eq)]
pub enum HirGenError {
    InvalidTopLevelStatement(Span),
    InvalidAssignmentTarget(Span),
    UnkownIdentifier(Span),
    NoBreakDestination(Span),
}

/// High-level IR
#[derive(Debug, Clone)]
pub struct Hir {
    pub functions: Vec<Function>,
}

impl Hir {
    pub fn generate(module: parsing::Module) -> Result<Self, Vec<HirGenError>> {
        HirGen::default().generate(module)
    }
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

// Last instruction must be something that moves execution elsewhere. Blocks are not ordered so
// there must be no fall-through.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
}

impl ops::Deref for BasicBlock {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.instructions
    }
}

/// The block with id `BasicBlockId::default()` is always executed first
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BasicBlockId(usize);

impl ops::DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.instructions
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    IntegerLiteral(IntegerLiteral),
    NullLiteral(Span),
    StringLiteral(StringLiteral),
    Push(Resolved),
    Save(Resolved),
    Call(Span, usize),
    Branch(BasicBlockId, BasicBlockId),
    Jump(BasicBlockId),
    Return,
    Invalid { stack_change: isize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolved {
    Local(Span, usize),
    Parameter(Span, usize),
    Global(Span, usize),
    Builtin(Span, Builtin),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct HirGen {
    basic_block_id_counter: BasicBlockId,
    generated_basic_blocks: HashMap<BasicBlockId, BasicBlock>,
    current_locals: Vec<Local>,
    current_basic_block: BasicBlock,
    errors: Vec<HirGenError>,
    scope: Box<Scope>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct Scope {
    parameters: HashMap<String, usize>,
    locals: HashMap<String, usize>,
    globals: HashMap<String, usize>,
    break_destination: Option<BasicBlockId>,
    continue_destination: Option<BasicBlockId>,
    parent: Option<Box<Self>>,
}

impl Scope {
    fn pop(&mut self) {
        let parent = mem::take(&mut self.parent);
        *self = *parent.unwrap_or_default();
    }

    fn push(&mut self, child: Self) {
        let scope = mem::take(self);
        *self = Scope {
            parent: Some(Box::new(scope)),
            ..child
        };
    }

    fn lookup(self: &Box<Self>, ident: &Identifier) -> Option<Resolved> {
        if let Some(&local) = self.locals.get(&ident.name) {
            Some(Resolved::Local(ident.span, local))
        } else if let Some(&param) = self.parameters.get(&ident.name) {
            Some(Resolved::Parameter(ident.span, param))
        } else if let Some(&global) = self.globals.get(&ident.name) {
            Some(Resolved::Global(ident.span, global))
        } else if let Some(ref parent) = self.parent {
            parent.lookup(ident)
        } else {
            None
        }
    }

    fn get_break_destination(self: &Box<Self>) -> Option<BasicBlockId> {
        if self.break_destination.is_some() {
            self.break_destination
        } else if let Some(ref parent) = self.parent {
            parent.get_break_destination()
        } else {
            None
        }
    }

    fn get_continue_destination(self: &Box<Self>) -> Option<BasicBlockId> {
        if self.continue_destination.is_some() {
            self.continue_destination
        } else if let Some(ref parent) = self.parent {
            parent.get_continue_destination()
        } else {
            None
        }
    }
}

impl HirGen {
    fn generate(mut self, module: parsing::Module) -> Result<Hir, Vec<HirGenError>> {
        let mut functions = vec![];
        for stmt in module.stmts {
            match stmt {
                Stmt::Expr(expr) => self.error(HirGenError::InvalidTopLevelStatement(expr.span)),
                Stmt::Let(_) => todo!(),
                Stmt::Assign(ass) => self.error(HirGenError::InvalidTopLevelStatement(ass.span)),
                Stmt::Function(f) => {
                    functions.push(self.generate_func(f));
                    assert!(self.generated_basic_blocks.is_empty());
                    assert!(self.current_locals.is_empty());
                    assert!(self.current_basic_block.is_empty());
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Hir { functions })
        } else {
            Err(self.errors)
        }
    }

    fn generate_func(&mut self, func: stmt::Function) -> Function {
        if !self.generated_basic_blocks.is_empty() {
            todo!("Already generating a function, wait until this is done before generating a new one");
        }

        let tmp = mem::take(&mut self.scope);
        self.scope = Box::new(Scope {
            parameters: func
                .params
                .iter()
                .enumerate()
                .map(|(i, (ident, _))| (ident.name.clone(), i))
                .collect(),
            parent: Some(tmp),
            ..Default::default()
        });

        let end_basic_block_id = self.next_basic_block_id();
        self.generate_block(func.body, end_basic_block_id);
        self.current_basic_block.push(Instruction::Return);
        self.finish_basic_block();

        let locals = mem::replace(&mut self.current_locals, vec![]);
        let body = mem::replace(&mut self.generated_basic_blocks, HashMap::new());

        Function {
            span: func.span,
            ident: func.ident,
            params: func.params,
            ret_type: func.return_type,
            locals,
            body,
        }
    }

    fn generate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.generate_expr(expr_stmt.expr);
                // make sure stack wasn't changed or something like that
            }
            Stmt::Assign(assign) => self.generate_assignment(assign),
            Stmt::Let(leet) => self.generate_local(leet),
            _ => todo!(),
        }
    }

    fn generate_local(&mut self, leet: stmt::Let) {
        let name = leet.ident.name.clone();
        let index = self.current_locals.len();
        self.current_locals.push(Local {
            span: leet.span,
            ident: leet.ident,
            type_: leet.type_,
        });
        self.scope.locals.insert(name, index);
        self.generate_expr(leet.value);
        self.current_basic_block
            .push(Instruction::Save(Resolved::Local(leet.span, index)));
    }

    fn generate_assignment(&mut self, assignment: stmt::Assignment) {
        self.generate_expr(assignment.value);
        match assignment.left {
            Expr::Ident(ident) => {
                if let Some(resolved) = self.scope.lookup(&ident) {
                    self.current_basic_block.push(Instruction::Save(resolved));
                } else {
                    self.error(HirGenError::UnkownIdentifier(ident.span));
                    self.current_basic_block
                        .push(Instruction::Invalid { stack_change: -1 });
                }
            }
            expr => self.error(HirGenError::InvalidAssignmentTarget(expr.span())),
        }
    }

    fn generate_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(expr::Literal::Integer(i)) => self
                .current_basic_block
                .push(Instruction::IntegerLiteral(i)),
            Expr::Literal(expr::Literal::Null(n)) => self
                .current_basic_block
                .push(Instruction::NullLiteral(n.span)),
            Expr::Literal(expr::Literal::Str(s)) => {
                self.current_basic_block.push(Instruction::StringLiteral(s))
            }
            Expr::Ident(ident) => self.generate_ident(ident),
            Expr::BinaryOperation(expr) => self.generate_binary_op_expr(expr),
            Expr::UnaryOperation(expr) => self.generate_unary_op_expr(expr),
            Expr::Call(call) => self.generate_call(call),
            Expr::If(eef) => self.generate_if(eef),
            Expr::Loop(looop) => self.generate_loop(looop),
            Expr::Break(brake) => self.generate_break(brake),
            Expr::Block(block) => {
                // TODO: generate_block should perhaps do more so that it actually generates a
                // block fully, and not just part of it. But Because of `if`s and stuff it can't do
                // the same every time.
                let continue_at = self.next_basic_block_id();
                let block_id = self.next_basic_block_id();
                self.current_basic_block.push(Instruction::Jump(block_id));
                self.finish_basic_block();
                self.current_basic_block.id = block_id;
                self.generate_block(block, continue_at);
                self.current_basic_block.id = continue_at;
            }
            _ => todo!(),
        }
    }

    fn generate_block(&mut self, block: expr::Block, continue_at: BasicBlockId) {
        self.scope.push(Default::default());

        for stmt in block.stmts.into_iter() {
            self.generate_stmt(stmt);
        }

        self.current_basic_block
            .push(Instruction::Jump(continue_at));

        self.finish_basic_block();

        self.scope.pop();
    }

    fn generate_if(&mut self, eef: expr::If) {
        self.generate_expr(*eef.condition);
        let true_basic_block_id = self.next_basic_block_id();
        let false_basic_block_id = self.next_basic_block_id();
        self.current_basic_block.push(Instruction::Branch(
            true_basic_block_id,
            false_basic_block_id,
        ));
        let next_id = self.next_basic_block_id();
        self.finish_basic_block();
        self.current_basic_block.id = true_basic_block_id;
        self.generate_block(eef.then, next_id);
        self.current_basic_block.id = false_basic_block_id;
        if let Some(elze) = eef.elze {
            self.generate_block(elze, next_id);
        } else {
            self.current_basic_block.push(Instruction::Jump(next_id));
            self.finish_basic_block();
        }

        self.current_basic_block.id = next_id;
    }

    fn generate_loop(&mut self, looop: expr::Loop) {
        let loop_basic_block_id = self.next_basic_block_id();
        let after_loop_basic_block_id = self.next_basic_block_id();
        self.scope.push(Scope {
            break_destination: Some(after_loop_basic_block_id),
            continue_destination: Some(loop_basic_block_id),
            ..Default::default()
        });
        self.current_basic_block
            .push(Instruction::Jump(loop_basic_block_id));
        self.finish_basic_block();
        self.current_basic_block.id = loop_basic_block_id;
        self.generate_block(looop.block, loop_basic_block_id);
        self.current_basic_block.id = after_loop_basic_block_id;
        self.scope.pop();
    }

    fn generate_break(&mut self, brake: expr::Break) {
        if let Some(dest) = self.scope.get_break_destination() {
            if let Some(expr) = brake.value {
                self.generate_expr(*expr);
            }
            self.current_basic_block.push(Instruction::Jump(dest));
        } else {
            self.error(HirGenError::NoBreakDestination(brake.span));
        }
    }

    fn generate_ident(&mut self, ident: Identifier) {
        if let Some(builtin) = Builtin::lookup(&ident) {
            self.current_basic_block
                .push(Instruction::Push(Resolved::Builtin(ident.span, builtin)));
        } else if let Some(resolved) = self.scope.lookup(&ident) {
            self.current_basic_block.push(Instruction::Push(resolved));
        } else {
            self.error(HirGenError::UnkownIdentifier(ident.span));
            self.current_basic_block
                .push(Instruction::Invalid { stack_change: 1 });
        }
    }

    fn generate_call(&mut self, call: expr::Call) {
        let params_len = call.params.len();
        for param in call.params.into_iter().rev() {
            self.generate_expr(param);
        }

        self.generate_expr(*call.func);
        self.current_basic_block
            .push(Instruction::Call(call.span, params_len));
    }

    fn generate_binary_op_expr(&mut self, expr: expr::BinaryOperation) {
        self.generate_expr(*expr.left);
        self.generate_expr(*expr.right);
        self.generate_ident(Identifier {
            span: expr.span,
            name: expr.op.to_string(),
        });
        self.current_basic_block
            .push(Instruction::Call(expr.span, 2));
    }

    fn generate_unary_op_expr(&mut self, expr: expr::UnaryOperation) {
        self.generate_expr(*expr.expr);
        self.generate_ident(Identifier {
            span: expr.span,
            name: expr.op.to_string(),
        });
        self.current_basic_block
            .push(Instruction::Call(expr.span, 1));
    }

    fn error(&mut self, err: HirGenError) {
        self.errors.push(err);
    }

    fn next_basic_block_id(&mut self) -> BasicBlockId {
        self.basic_block_id_counter = BasicBlockId(self.basic_block_id_counter.0 + 1);
        self.basic_block_id_counter
    }

    /// Moves `self.current_basic_block` into `self.generated_blocks`
    fn finish_basic_block(&mut self) {
        let id = self.next_basic_block_id();
        let basic_block = mem::replace(
            &mut self.current_basic_block,
            BasicBlock {
                id,
                instructions: vec![],
            },
        );
        self.generated_basic_blocks
            .insert(basic_block.id, basic_block);
    }
}

/// let skip usize = 1;
/// fn sus_fib(n usize, m &usize) usize {
///     let a usize = 0;
///     let b usize = 1;
///     loop {
///         if n <= 0 {
///             break a;
///         }
///         let c usize = a + b % *m;
///         a = b;
///         b = c;
///         n = n - skip;
///     }
/// }
///
/// global skip usize = 1;
///
/// fn sus_fib(n usize) usize {
///     let (a usize, b usize, c usize);
///
/// block0:
///     push literal 0             // r0 = literal 0           // r0
///     save ident a               // ident a = r0             //
///     push literal 1             // r1 = literal 1           // r1
///     save ident b               // ident b = r1             //
///     jump loop_block
///
/// if_block:
///     // break a
///     push ident a
///     jump end_loop_block
///
/// loop_block:
///     // if n <= 0
///     push ident n               // r2 = ident n             // r2
///     push literal 0             // r3 = literal 0           // r2 r3
///     binary leq                 // r4 = binary leq r2 r3    // r4
///     jump_true if_block         // jump_if r4 if_block      //
///
///     // let c usize = a + b % *m;
///     push ident a               // r5 = ident a             // r5
///     push ident b               // r6 = ident b             // r5 r6
///     push ident m               // r7 = ident m             // r5 r6 r7
///     unary deref                // r8 = unary deref r7      // r7 r6 r8
///     binary mod                 // r9 = binary mod r6 r8    // r7 r9
///     binary add                 // r10 = binary add r7 r9   // r10
///     save ident c               // ident c = r10            //
///
///     // a = b
///     push ident b
///     save ident a
///
///     // b = c
///     push ident c
///     save ident b
///
///     // n = n - skip
///     push ident n
///     push ident skip
///     binary sub
///     save ident n
///
///     jump loop_block
///
/// end_loop_block:
///     jump end_block
///
/// end_block:
///     // return value is on stack
/// }
///
/// global skip usize = 1
///
/// fn sus_fib(n usize) usize {
///     let (a usize, b usize, c usize)
/// block0:
///     r0 = literal 0
///     save (ident a) r0
///     r1 = literal 1
///     save (ident b) r1
///
/// if_block:
///     load r_end_loop (ident a)
///     jump end_loop_block
///
/// loop_block:
///     load r2 (ident n)
///     r3 = literal 0
///     r4 = binary eq r2 r3
///     jump_if r4 if_block
///
///     load r5 (ident a)
///     load r6 (ident b)
///     load r7 (ident m)
///     r8 = unary deref r7
///     r9 = binary mod r6 r8
///     r10 = binary add r5 r9
///     save (ident c) r10
///     load r11 (ident b)
///     save (ident a) r11
///     load r12 (ident c)
///     save (ident b) r12
///     load r13 (ident n)
///     load r14 (global skip)
///     r15 = binary sub r13 r14
///     save (ident n) r15
///     jump loop_block
/// end_loop_block:
///     return = r_end_loop
/// }
///
/// ```
/// type LinkedList (
///     value usize,
///     next: &LinkedList,
/// )
///
/// fn int_list(size usize) LinkedList {
///     let root LinkedList = LinkedList(0, null);
///     let curr &LinkedList = &root;
///
///     let i usize = 0;
///     loop {
///         if i == n { break; }
///         curr.next = alloc();
///         if i % 2 == 0 {
///             *curr.next = LinkedList(curr.value + 1, null);
///         } else {
///             curr.next.value = curr.value + 1;
///             curr.next.next = null;
///         }
///         curr = curr.next;
///
///         i = i + 1;
///     }
///
///     return root;
/// }
/// ```
///
/// fn int_list(size usize) LinkedList {
///     let (root LinkedList, curr &LinkedList, i usize)
///
/// block0:
///     // let root LinkedList = LinkedList(0, null);
///     push literal 0
///     push literal null
///     construct LinkedList
///     save ident root
///
///     // let curr &LinkedList = &root;
///     push ident root
///     unary reference
///     save ident curr
///
///     // let i usize = 0;
///     push literal 0
///     save ident i
///
///     jump loop_block0
///
/// if_block:
///     jump loop_end_block
///
/// if2_block:
///     // *curr.next = LinkedList(curr.value + 1, null);
///     push ident curr
///     access value
///     push literal 1
///     binary add
///     push literal null
///     construct LinkedList
///     push ident curr
///     access next
///     save memory
///     jump loop_block1
///
/// if2else_block:
///     // curr.next.value = curr.value + 1;
///     push ident curr
///     access value
///     push literal 1
///     binary add
///     push ident curr
///     access next
///     save access value
///
///     // curr.next.next = null;
///     push literal null
///     push ident curr
///     access next
///     save access next
///
///     jump loop_block1
///
/// loop_block0:
///     // if i == n
///     push ident i
///     push ident n
///     binary eq
///     jump_if if_block
///
///     // curr.next = alloc();
///     push ident curr
///     access next
///     construct alloc
///     assign
///
///     // if i % 2 == 0
///     push ident i
///     push literal 2
///     binary mod
///     push literal 0
///     binary eq
///     jump_if if2_block
///     jump if2else_block
///
/// loop_block1:
///     // // curr.next.next.next.value = 1;
///     // push literal 1
///     // push ident curr
///     // access next
///     // access next
///     // access next
///     // save access value
///
///     // curr = curr.next
///     push ident curr
///     access next
///     save ident curr
///
///     // i = i + 1;
///     push ident i
///     push literal 1
///     binary add
///     save ident i
///
///     jump loop_end_block
///
/// loop_end_block:
///     jump block1
///
/// block1:
///     // return root
///     push ident root
/// }
struct _Dummy;

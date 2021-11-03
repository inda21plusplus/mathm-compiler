use parcom::Span;

use crate::{
    parsing::{expr, stmt, Expr, Identifier, Module, Stmt},
    Builtin,
};
use std::{collections::HashMap, mem};

use super::{
    BasicBlock, BasicBlockId, Function, Hir, Instruction, Local, Resolved, Symbol, Variable,
};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct HirGen {
    basic_block_id_counter: BasicBlockId,
    generated_basic_blocks: HashMap<BasicBlockId, BasicBlock>,
    current_locals: Vec<Local>,
    current_basic_block: BasicBlock,
    return_basic_block_id: BasicBlockId,
    curr_stack_height: StackHeight,
    errors: Vec<HirGenError>,
    scope: Box<Scope>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum HirGenError {
    InvalidTopLevelStatement(Span),
    InvalidAssignmentTarget(Span),
    UnkownIdentifier(Span),
    NoBreakDestination(Span),
    NoContinueDestination(Span),
    InvalidVariableInitialization(Span),
    CannotTakeReferenceOf(Span),
}

type StackHeight = isize;

#[derive(Debug, Default, PartialEq, Eq)]
struct Scope {
    parameters: HashMap<String, usize>,
    locals: HashMap<String, usize>,
    symbols: HashMap<String, usize>,
    break_destination: Option<(BasicBlockId, StackHeight)>,
    continue_destination: Option<(BasicBlockId, StackHeight)>,
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
        } else if let Some(&symbol) = self.symbols.get(&ident.name) {
            Some(Resolved::Symbol(ident.span, symbol))
        } else if let Some(ref parent) = self.parent {
            parent.lookup(ident)
        } else {
            None
        }
    }

    fn get_break_destination(self: &Box<Self>) -> Option<(BasicBlockId, StackHeight)> {
        if self.break_destination.is_some() {
            self.break_destination
        } else if let Some(ref parent) = self.parent {
            parent.get_break_destination()
        } else {
            None
        }
    }

    fn get_continue_destination(self: &Box<Self>) -> Option<(BasicBlockId, StackHeight)> {
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
    pub(super) fn generate(mut self, module: Module) -> Result<Hir, Vec<HirGenError>> {
        let mut symbol_counter = 0;
        let mut entypoint_index = None;
        for stmt in &module.stmts {
            match stmt {
                Stmt::Let(stmt::Let { ident, .. })
                | Stmt::Function(stmt::Function { ident, .. }) => {
                    self.scope
                        .symbols
                        .insert(ident.name.clone(), symbol_counter);
                    if ident.name == "main" {
                        entypoint_index = Some(symbol_counter);
                    }
                    symbol_counter += 1;
                }
                _ => {}
            }
        }
        let mut symbols = vec![];
        for stmt in module.stmts {
            match stmt {
                Stmt::Expr(expr) => self.error(HirGenError::InvalidTopLevelStatement(expr.span)),
                Stmt::Assign(ass) => self.error(HirGenError::InvalidTopLevelStatement(ass.span)),
                Stmt::Let(l) => {
                    symbols.push(Symbol::Variable(self.generate_global(l)));
                    assert!(self.generated_basic_blocks.is_empty());
                    assert!(self.current_locals.is_empty());
                    assert!(self.current_basic_block.instructions.is_empty());
                }
                Stmt::Function(f) => {
                    symbols.push(Symbol::Function(self.generate_func(f)));
                    assert!(self.generated_basic_blocks.is_empty());
                    assert!(self.current_locals.is_empty());
                    assert!(self.current_basic_block.instructions.is_empty());
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Hir {
                symbols,
                entypoint_index,
            })
        } else {
            Err(self.errors)
        }
    }

    fn generate_global(&mut self, leet: stmt::Let) -> Variable {
        assert!(self.generated_basic_blocks.is_empty());

        self.scope.push(Default::default());

        let value_span = leet.value.span();

        self.generate_expr(leet.value);
        self.finish_basic_block();

        let body = mem::take(&mut self.generated_basic_blocks);
        if body.len() != 1 {
            self.error(HirGenError::InvalidVariableInitialization(value_span));
        }
        let (_id, body) = body.into_iter().next().unwrap();

        Variable {
            span: leet.span,
            ident: leet.ident,
            type_: leet.type_,
            body,
        }
    }

    fn generate_func(&mut self, func: stmt::Function) -> Function {
        assert!(self.generated_basic_blocks.is_empty());

        self.scope.push(Scope {
            parameters: func
                .params
                .iter()
                .enumerate()
                .map(|(i, (ident, _))| (ident.name.clone(), i))
                .collect(),
            ..Default::default()
        });

        self.return_basic_block_id = self.next_basic_block_id();
        self.generate_block(func.body, self.return_basic_block_id);
        self.current_basic_block.id = self.return_basic_block_id;
        self.instr(Instruction::Return);
        self.finish_basic_block();

        let locals = mem::take(&mut self.current_locals);
        let body = mem::take(&mut self.generated_basic_blocks);

        Function {
            span: func.span,
            ident: func.ident,
            params: func.params,
            ret_type: func.return_type,
            locals,
            body,
        }
    }

    /// Returns true if statement pushed value onto stack
    fn generate_stmt(&mut self, stmt: Stmt) -> bool {
        match stmt {
            Stmt::Expr(stmt::ExprStmt {
                expr, semi: false, ..
            }) => {
                self.generate_expr(expr);
                return true;
            }
            Stmt::Expr(stmt::ExprStmt {
                expr, semi: true, ..
            }) => {
                self.generate_expr(expr);
                self.instr(Instruction::Discard(1));
            }
            Stmt::Assign(assign) => self.generate_assignment(assign),
            Stmt::Let(leet) => self.generate_local(leet),
            _ => todo!(),
        }
        false
    }

    fn generate_assignment(&mut self, assignment: stmt::Assignment) {
        self.generate_expr(assignment.value);
        match assignment.left {
            Expr::Ident(ident) => {
                if let Some(resolved) = self.scope.lookup(&ident) {
                    self.instr(Instruction::Save(resolved));
                } else {
                    self.error(HirGenError::UnkownIdentifier(ident.span));
                    self.instr(Instruction::Invalid { stack_delta: -1 });
                }
            }
            expr => {
                self.error(HirGenError::InvalidAssignmentTarget(expr.span()));
                self.instr(Instruction::Invalid { stack_delta: -1 });
            }
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
        self.instr(Instruction::Save(Resolved::Local(leet.span, index)));
    }

    /// Always pushes one value onto stack
    fn generate_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(expr::Literal::Integer(i)) => self.instr(Instruction::IntegerLiteral(i)),
            Expr::Literal(expr::Literal::Null(n)) => self.instr(Instruction::NullLiteral(n.span)),
            Expr::Literal(expr::Literal::Str(s)) => self.instr(Instruction::StringLiteral(s)),
            Expr::Literal(expr::Literal::Bool(b)) => self.instr(Instruction::BoolLiteral(b)),
            Expr::Ident(ident) => self.generate_ident(ident),
            Expr::BinaryOperation(expr) => self.generate_binary_op_expr(expr),
            Expr::UnaryOperation(expr) => self.generate_unary_op_expr(expr),
            Expr::Call(call) => self.generate_call(call),
            Expr::If(eef) => self.generate_if(eef),
            Expr::Loop(looop) => self.generate_loop(looop),
            Expr::Break(brejk) => self.generate_break(brejk),
            Expr::Continue(continueue) => self.generate_continue(continueue),
            Expr::Return(reeturn) => self.generate_return(reeturn),
            Expr::Block(block) => {
                // TODO: generate_block should perhaps do more so that it actually generates a
                // block fully, and not just part of it. But Because of `if`s and stuff it can't do
                // the same every time.
                let block_id = self.next_basic_block_id();
                let continue_at = self.next_basic_block_id();
                self.instr(Instruction::Jump(block_id));
                self.finish_basic_block();
                self.current_basic_block.id = block_id;
                self.generate_block(block, continue_at);
                self.current_basic_block.id = continue_at;
            }
        }
    }

    /// Always pushes one value onto stack unless continue_at == self.current_basic_block.id
    fn generate_block(&mut self, block: expr::Block, continue_at: BasicBlockId) {
        if block.stmts.is_empty() {
            self.instr(Instruction::NullLiteral(block.span));
            self.instr(Instruction::Jump(continue_at));
            self.finish_basic_block();
            return;
        }

        self.scope.push(Default::default());

        let last_index = block.stmts.len() - 1;
        for (i, stmt) in block.stmts.into_iter().enumerate() {
            let did_push_value = self.generate_stmt(stmt);
            if i < last_index && did_push_value {
                self.instr(Instruction::Discard(1));
            }
            if i == last_index && !did_push_value {
                self.instr(Instruction::NullLiteral(block.span));
            }
        }

        if self.current_basic_block.id == continue_at {
            self.instr(Instruction::Discard(1));
        }
        self.instr(Instruction::Jump(continue_at));

        self.finish_basic_block();

        self.scope.pop();
    }

    fn generate_if(&mut self, eef: expr::If) {
        let condition_span = eef.condition.span();
        self.generate_expr(*eef.condition);
        let then_basic_block_id = self.next_basic_block_id();
        let else_basic_block_id = self.next_basic_block_id();
        self.instr(Instruction::Branch(
            condition_span,
            then_basic_block_id,
            else_basic_block_id,
        ));
        self.finish_basic_block();
        let next_id = self.next_basic_block_id();
        self.current_basic_block.id = then_basic_block_id;
        let stack_height = self.curr_stack_height;
        self.generate_block(eef.then, next_id);
        self.curr_stack_height = stack_height;
        self.current_basic_block.id = else_basic_block_id;
        if let Some(elze) = eef.elze {
            self.generate_block(elze, next_id);
        } else {
            self.instr(Instruction::NullLiteral(eef.span));
            self.instr(Instruction::Jump(next_id));
            self.finish_basic_block();
        }

        self.current_basic_block.id = next_id;
    }

    fn generate_loop(&mut self, looop: expr::Loop) {
        let loop_basic_block_id = self.next_basic_block_id();
        let after_loop_basic_block_id = self.next_basic_block_id();
        self.scope.push(Scope {
            break_destination: Some((after_loop_basic_block_id, self.curr_stack_height)),
            continue_destination: Some((loop_basic_block_id, self.curr_stack_height)),
            ..Default::default()
        });
        self.instr(Instruction::Jump(loop_basic_block_id));
        self.finish_basic_block();
        self.current_basic_block.id = loop_basic_block_id;
        self.generate_block(looop.block, loop_basic_block_id);
        self.current_basic_block.id = after_loop_basic_block_id;
        self.scope.pop();

        // `self.curr_stack_height` will have increased by one due to the loop block we generated.
        // That's not why it will have actually increased though, but subtracting one and then
        // adding one to `self.curr_stack_height` doesn't make that much sense either.
    }

    fn generate_break(&mut self, brejk: expr::Break) {
        if let Some((dest, height_at_dest)) = self.scope.get_break_destination() {
            if self.curr_stack_height > height_at_dest {
                self.instr(Instruction::Discard(
                    (self.curr_stack_height - height_at_dest) as usize,
                ));
            }
            if let Some(expr) = brejk.value {
                self.generate_expr(*expr);
            } else {
                self.instr(Instruction::NullLiteral(brejk.span));
            }
            self.instr(Instruction::Jump(dest));
        } else {
            self.instr(Instruction::Invalid { stack_delta: 1 });
            self.error(HirGenError::NoBreakDestination(brejk.span));
        }
    }

    fn generate_continue(&mut self, continueue: expr::Continue) {
        if let Some((dest, height_at_dest)) = self.scope.get_continue_destination() {
            if self.curr_stack_height > height_at_dest {
                self.instr(Instruction::Discard(
                    (self.curr_stack_height - height_at_dest) as usize,
                ));
            }
            self.instr(Instruction::Jump(dest));
        } else {
            self.instr(Instruction::Invalid { stack_delta: 1 });
            self.error(HirGenError::NoContinueDestination(continueue.0));
        }
    }

    fn generate_return(&mut self, reeturn: expr::Return) {
        if self.curr_stack_height > 0 {
            self.instr(Instruction::Discard(self.curr_stack_height as usize));
        }
        if let Some(expr) = reeturn.value {
            self.generate_expr(*expr);
        } else {
            self.instr(Instruction::NullLiteral(reeturn.span));
        }
        self.instr(Instruction::Jump(self.return_basic_block_id));
    }

    fn generate_ident(&mut self, ident: Identifier) {
        if let Some(builtin) = Builtin::lookup(&ident) {
            self.instr(Instruction::Push(Resolved::Builtin(ident.span, builtin)));
        } else if let Some(resolved) = self.scope.lookup(&ident) {
            self.instr(Instruction::Push(resolved));
        } else {
            self.error(HirGenError::UnkownIdentifier(ident.span));
            self.instr(Instruction::Invalid { stack_delta: 1 });
        }
    }

    fn generate_call(&mut self, call: expr::Call) {
        let params_len = call.params.len();
        for param in call.params.into_iter().rev() {
            self.generate_expr(param);
        }

        self.generate_expr(*call.func);
        self.instr(Instruction::Call(call.span, params_len));
    }

    fn generate_binary_op_expr(&mut self, expr: expr::BinaryOperation) {
        self.generate_expr(*expr.left);
        self.generate_expr(*expr.right);
        self.generate_ident(Identifier {
            span: expr.span,
            name: expr.op.to_string(),
        });
        self.instr(Instruction::Call(expr.span, 2));
    }

    fn generate_unary_op_expr(&mut self, expr: expr::UnaryOperation) {
        if let expr::UnaryOperatorKind::Ref = expr.op {
            self.generate_reference(*expr.expr);
            return;
        }
        self.generate_expr(*expr.expr);
        self.generate_ident(Identifier {
            span: expr.span,
            name: expr.op.to_string(),
        });
        self.instr(Instruction::Call(expr.span, 1));
    }

    fn generate_reference(&mut self, expr: Expr) {
        match expr {
            Expr::Ident(ident) => {
                if let Some(builtin) = Builtin::lookup(&ident) {
                    self.instr(Instruction::PushReference(Resolved::Builtin(
                        ident.span, builtin,
                    )));
                } else if let Some(resolved) = self.scope.lookup(&ident) {
                    self.instr(Instruction::PushReference(resolved));
                } else {
                    self.error(HirGenError::UnkownIdentifier(ident.span));
                    self.instr(Instruction::Invalid { stack_delta: 1 });
                }
            }
            _ => {
                self.error(HirGenError::CannotTakeReferenceOf(expr.span()));
            }
        }
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

    fn instr(&mut self, instr: Instruction) {
        self.curr_stack_height += instr.stack_delta();
        self.current_basic_block.instructions.push(instr);
    }
}

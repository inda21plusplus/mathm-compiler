use std::mem;

use parcom::Span;

use crate::parsing::{self, expr, stmt, type_, Expr, Identifier, Stmt, Type};

#[derive(Debug, PartialEq, Eq)]
pub enum HirGenError {
    InvalidTopLevelStatement(Span),
    InvalidAssignmentTarget(Span),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct HirGen {
    generated_blocks: Vec<Block>,
    current_locals: Vec<Local>,
    current_block: Vec<Instruction>,
    errors: Vec<HirGenError>,
}

impl HirGen {
    fn generate(mut self, module: parsing::Module) -> Result<Hir, Vec<HirGenError>> {
        let mut type_defs = vec![];
        let mut functions = vec![];
        for stmt in module.stmts {
            match stmt {
                Stmt::Expr(expr) => self.error(HirGenError::InvalidTopLevelStatement(expr.span)),
                Stmt::Let(_) => todo!(),
                Stmt::TypeDef(type_def) => type_defs.push(type_def),
                Stmt::Assign(ass) => self.error(HirGenError::InvalidTopLevelStatement(ass.span)),
                Stmt::Function(f) => {
                    functions.push(self.generate_func(f));
                    assert!(self.generated_blocks.is_empty());
                    assert!(self.current_locals.is_empty());
                    assert!(self.current_block.is_empty());
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Hir {
                type_defs,
                functions,
            })
        } else {
            Err(self.errors)
        }
    }

    fn error(&mut self, err: HirGenError) {
        self.errors.push(err);
    }

    pub fn generate_func(&mut self, func: stmt::Function) -> Function {
        for stmt in func.body.stmts.into_iter() {
            self.generate_stmt(stmt);
        }

        self.generated_blocks.push(Block {
            instructions: mem::take(&mut self.current_block),
        });

        let locals = mem::take(&mut self.current_locals);
        let body = mem::take(&mut self.generated_blocks);

        Function {
            span: func.span,
            ident: func.ident,
            params: func.params,
            return_type: func.return_type,
            locals,
            body,
        }
    }

    fn generate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.generate_expr(expr_stmt.expr);
                // assert stack wasnt changed or something like that
            }
            Stmt::Assign(assign) => self.generate_assignment(assign),
            _ => todo!(),
        }
    }

    fn generate_assignment(&mut self, assignment: stmt::Assignment) {
        self.generate_expr(assignment.value);
        match assignment.left {
            Expr::Ident(ident) => self
                .current_block
                .push(Instruction::SaveIdent(assignment.span, ident)),
            expr => self.error(HirGenError::InvalidAssignmentTarget(expr.span())),
        }
    }

    fn generate_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(expr::Literal::Integer(i)) => self
                .current_block
                .push(Instruction::LiteralInteger(i.span, i.value)),
            Expr::Literal(expr::Literal::Null(n)) => {
                self.current_block.push(Instruction::LiteralNull(n.span))
            }
            Expr::Ident(ident) => self.current_block.push(Instruction::Ident(ident)),
            Expr::BinaryOperation(expr) => self.generate_binary_op_expr(expr),
            Expr::UnaryOperation(expr) => self.generate_unary_op_expr(expr),
            Expr::Construction(constr) => self.generate_construction(constr),
            _ => todo!(),
        }
    }

    fn generate_construction(&mut self, mut constr: expr::Construction) {
        let mut counter = 0..;
        let mut params = vec![];
        for param in constr.params.iter_mut() {
            if let Some(key) = &mut param.key {
                params.push(ConstructionParam::Ident(mem::replace(
                    key,
                    Identifier {
                        span: Span::default(),
                        name: "".into(),
                    },
                )));
            } else {
                params.push(ConstructionParam::Int(counter.next().unwrap()));
            }
        }
        for param in constr.params.into_iter().rev() {
            self.generate_expr(param.value);
        }

        if let Some(left) = constr.left {
            self.generate_expr(*left);
            self.current_block
                .push(Instruction::Construction(constr.span, params));
        } else {
            self.current_block
                .push(Instruction::AnonymousConstruction(constr.span, params));
        }
    }

    fn generate_binary_op_expr(&mut self, expr: expr::BinaryOperation) {
        self.generate_expr(*expr.left);
        self.generate_expr(*expr.right);
        self.current_block.push(Instruction::Ident(Identifier {
            span: Span::default(),
            name: expr.op.to_string(),
        }));
        self.current_block.push(Instruction::Construction(
            expr.span,
            vec![ConstructionParam::Int(1), ConstructionParam::Int(0)],
        ));
    }

    fn generate_unary_op_expr(&mut self, expr: expr::UnaryOperation) {
        self.generate_expr(*expr.expr);
        self.current_block.push(Instruction::Ident(Identifier {
            span: Span::default(),
            name: expr.op.to_string(),
        }));
        self.current_block.push(Instruction::Construction(
            expr.span,
            vec![ConstructionParam::Int(0)],
        ));
    }
}

/// High-level IR
#[derive(Debug, Clone)]
pub struct Hir {
    pub type_defs: Vec<stmt::TypeDef>,
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
    pub params: type_::Struct,
    pub return_type: Type,
    pub locals: Vec<Local>,
    pub body: Vec<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    pub span: Span,
    pub ident: Identifier,
    pub type_: Type,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Block {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    LiteralInteger(Span, u128),
    LiteralNull(Span),
    Ident(Identifier),
    Construction(Span, Vec<ConstructionParam>),
    AnonymousConstruction(Span, Vec<ConstructionParam>),
    SaveIdent(Span, Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstructionParam {
    Ident(Identifier),
    Int(usize),
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

use cranelift::codegen::ir::{types::I64, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module};
use std::mem;

pub fn run() {
    let mut ctx = FunctionBuilderContext::new();
    let mut func = Function::new();
    func.signature.returns.push(AbiParam::new(I64));
    let mut builder = FunctionBuilder::new(&mut func, &mut ctx);
    let block = builder.create_block();
    builder.append_block_params_for_function_params(block);
    builder.switch_to_block(block);
    builder.seal_block(block);
    let stack_slot = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
    let value = builder.ins().iconst(I64, 34);
    builder.ins().stack_store(value, stack_slot, 0);
    let val1 = builder.ins().iconst(I64, 35);
    let val2 = builder.ins().stack_load(I64, stack_slot, 0);
    let val3 = builder.ins().iadd(val1, val2);
    builder.ins().return_(&[val3]);
    builder.finalize();

    let mut jit = JITModule::new(JITBuilder::new(default_libcall_names()));
    let mut jit_ctx = jit.make_context();
    let func_id = jit
        .declare_function("cool", Linkage::Export, &func.signature)
        .unwrap();
    jit.define_function(
        func_id,
        &mut jit_ctx,
        &mut cranelift::codegen::binemit::NullTrapSink {},
        &mut cranelift::codegen::binemit::NullStackMapSink {},
    )
    .unwrap();
    let func_ptr = jit.get_finalized_function(func_id);

    let callable: fn() -> u64 = unsafe { mem::transmute(func_ptr) };
    let res = callable();
    println!("{}", res);
}

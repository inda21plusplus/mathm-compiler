use parcom::Input;

use crate::{
    hir::{BasicBlockId, Hir, Instruction, Resolved},
    parsing::{number::IntegerLiteral, Module},
    Builtin,
};

#[test]
fn generate_hir() {
    let input = Input::new(
        "fn add_one(n usize) usize {
             n + 1
         }",
    );
    let module = Module::parse(input).unwrap();

    let hir = Hir::generate(module).unwrap();
    assert_eq!(hir.functions.len(), 1);
    let f = &hir.functions[0];
    assert!(f.body.len() >= 1);
    let block = f.body.get(&BasicBlockId::default()).unwrap();
    assert_eq!(block.instructions.len(), 5, "{:#?}", block.instructions);
    match &block.instructions[0] {
        Instruction::Push(Resolved::Parameter(_, 0)) => (),
        other => panic!("{:#?}", other.clone()),
    }
    match &block.instructions[1] {
        Instruction::IntegerLiteral(IntegerLiteral { value: 1, .. }) => (),
        other => panic!("{:#?}", other.clone()),
    }
    match &block.instructions[2] {
        Instruction::Push(Resolved::Builtin(_, Builtin::Plus)) => (),
        other => panic!("{:#?}", other.clone()),
    }
    match &block.instructions[3] {
        Instruction::Call(_, 2) => (),
        other => panic!("{:#?}", other.clone()),
    }
}

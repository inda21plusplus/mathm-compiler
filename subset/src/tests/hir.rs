use parcom::{Input, Parser};

use crate::{
    hir::{HirGen, Instruction},
    parsing::Stmt,
};

#[test]
fn generate_hir() {
    let mut hg = HirGen::default();
    let input = Input::new(
        "fn one() usize {
             1
         }",
    );
    let (_, f) = Stmt::parser().parse(input).unwrap();
    let f = match f {
        Stmt::Function(f) => f,
        _ => panic!(),
    };

    let f = hg.generate_func(f);
    assert_eq!(f.body.len(), 1);
    assert_eq!(f.body[0].instructions.len(), 1);
    assert!(matches!(
        f.body[0].instructions[0],
        Instruction::LiteralInteger(_, 1)
    ));
}

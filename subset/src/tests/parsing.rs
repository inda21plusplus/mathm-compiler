use parcom::Input;

use crate::parsing::{
    expr::{Block, Construction, ConstructionParameter, Literal},
    stmt::{self, ExprStmt},
    string::StringLiteral,
    type_::Struct,
    Expr, Identifier, Module, Stmt, Type,
};

#[test]
fn hello_world() {
    let input = Input::new(
        r#"
        fn main() () {
            print("Hello, World!");
        }
    "#,
    );

    let module = Module::parse(input).unwrap();

    assert_eq!(module.stmts.len(), 1);
    match &module.stmts[0] {
        Stmt::Function(stmt::Function {
            ident: Identifier { name: main_str, .. },
            params: Struct {
                types: emptyvec0, ..
            },
            return_type: Type::Struct(Struct {
                types: emptyvec1, ..
            }),
            body: Block { stmts, .. },
            ..
        }) if emptyvec0.len() == 0
            && emptyvec1.len() == 0
            && main_str == "main"
            && match &stmts[..] {
                [Stmt::Expr(ExprStmt {
                    expr:
                        Expr::Construction(Construction {
                            left:
                                Some(box Expr::Ident(Identifier {
                                    name: print_str, ..
                                })),
                            params,
                            ..
                        }),
                    semi: true,
                    ..
                })] if print_str == "print"
                    && match &params[..] {
                        [ConstructionParameter {
                            key: None,
                            value:
                                Expr::Literal(Literal::Str(StringLiteral {
                                    value: hello_world_str,
                                    ..
                                })),
                            ..
                        }] if hello_world_str == "Hello, World!" => true,
                        _ => false,
                    } =>
                {
                    true
                }
                _ => false,
            } =>
        {
            ()
        }
        m => panic!("{:#?}", m),
    }
}

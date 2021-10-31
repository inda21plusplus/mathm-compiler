use parcom::{Error, Input, Parser, Span};

use crate::parsing::{
    expr::{Block, Call, Literal},
    number::IntegerLiteral,
    stmt::{self, ExprStmt},
    string::StringLiteral,
    Expr, Identifier, Module, Stmt, Type,
};

#[test]
fn test_let() {
    let input = Input::new("let a usize = 2;");
    let (rest, stmt) = Stmt::parser().parse(input).unwrap();
    assert!(rest.next().is_none());
    match stmt {
        Stmt::Let(stmt::Let {
            ident: Identifier { name, .. },
            type_: Type::Usize(_),
            value:
                Expr::Literal(Literal::Integer(IntegerLiteral {
                    value: 2, base: 10, ..
                })),
            ..
        }) if name == "a" => (),
        other => panic!("{:#?}", other),
    }

    let input = Input::new("let a = 2;");
    let err = Stmt::parser().parse(input).unwrap_err();
    assert_eq!(err, Error::new(Span::new(6..7)));
}

#[test]
fn hello_world() {
    let input = Input::new(
        r#"
        fn main() {
            print("Hello, World!");
        }
    "#,
    );

    let module = Module::parse(input).unwrap();

    assert_eq!(module.stmts.len(), 1);
    match &module.stmts[0] {
        Stmt::Function(stmt::Function {
            ident: Identifier { name: main_str, .. },
            params: emptyvec0,
            return_type: Type::Null(_),
            body: Block { stmts, .. },
            ..
        }) if emptyvec0.len() == 0
            && main_str == "main"
            && match &stmts[..] {
                [Stmt::Expr(ExprStmt {
                    expr:
                        Expr::Call(Call {
                            func:
                                box Expr::Ident(Identifier {
                                    name: print_str, ..
                                }),
                            params,
                            ..
                        }),
                    semi: true,
                    ..
                })] if print_str == "print"
                    && match &params[..] {
                        [Expr::Literal(Literal::Str(StringLiteral {
                            value: hello_world_str,
                            ..
                        }))] if hello_world_str == "Hello, World!" => true,
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

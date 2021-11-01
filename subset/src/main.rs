#![feature(stdin_forwarders, box_patterns)]

mod builtins;
mod error;
mod hir;
pub mod parsing;

#[cfg(test)]
mod tests;

use std::io::{stdin, stdout, Write};

pub use builtins::Builtin;
pub use error::Error;
use parcom::{Input, Parser, Span};

use crate::{
    hir::Hir,
    parsing::{expr::Block, Identifier, Module, Type},
};

fn main() {
    let mut lines = stdin().lines().flat_map(|line| line.ok());
    loop {
        print!("> ");
        let _ = stdout().flush();
        let line = match lines.next() {
            Some(line) => line,
            None => break,
        };

        let input = Input::new(&line);
        match parsing::Stmt::parser().parse(input) {
            Ok((Input { s: "", .. }, output)) => {
                // println!("{:#?}", output);
                let hir = Hir::generate(Module {
                    stmts: vec![parsing::Stmt::Function(parsing::stmt::Function {
                        span: output.span(),
                        ident: Identifier {
                            span: Span::new(0..0),
                            name: "main".into(),
                        },
                        params: vec![],
                        return_type: Type::Void(Span::new(0..0)),
                        body: Block {
                            span: Span::new(0..0),
                            stmts: vec![output],
                        },
                    })],
                });
                println!("{:#?}", hir);
            }
            Ok((Input { location, .. }, _)) => println!("Expected end of input at {}..", location),
            Err(err) => println!("Parsing error at {}", err.at),
        }
    }
}

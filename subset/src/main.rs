#![feature(stdin_forwarders, box_patterns)]

mod builtins;
mod error;
mod hir;
pub mod parsing;

#[cfg(test)]
mod tests;

use std::{
    env, fs,
    io::{stdin, stdout, Read, Write},
};

pub use builtins::Builtin;
pub use error::Error;
use parcom::{Input, Parser, Span};

use crate::{
    hir::Hir,
    parsing::{expr::Block, Identifier, Module, Type},
};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl();
    } else {
        for filename in args.iter().skip(1) {
            let mut file =
                fs::File::open(filename).expect(&format!("Could not open file {}", filename));
            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .expect(&format!("Could not read file {}", filename));
            let input = Input::new(&contents);
            let ast =
                parsing::Module::parse(input).expect(&format!("Could not parse file {}", filename));
            let hir =
                hir::Hir::generate(ast).expect(&format!("Could not generate HIR for {}", filename));
            let mut runner = hir::Runner::new(hir);
            runner.run();
        }
    }
}

fn repl() {
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

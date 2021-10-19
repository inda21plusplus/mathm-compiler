#![feature(stdin_forwarders)]

mod error;
mod get_span;
mod parsing;

#[cfg(test)]
mod tests;

use std::io::{stdin, stdout, Write};

pub use error::Error;
use parcom::{Input, Parser};

fn main() {
    let mut lines = stdin().lines().flat_map(|line| line.ok());
    loop {
        print!("> ");
        let _ = stdout().flush();
        let line = match lines.next() {
            Some(line) => line,
            None => break,
        };

        let input = Input::from_str(&line);
        match parsing::Expr::parser(0).parse(input) {
            Ok((Input { s: "", .. }, output)) => println!("{:#?}", output),
            Ok((Input { location, .. }, _)) => println!("Expected end of input at {}..", location),
            Err(err) => println!("Parsing error at {}", err.at),
        }
    }
}

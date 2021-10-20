use parcom::Parser;

use super::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
}

impl Stmt {
    pub fn parser() -> impl Parser<Output = Self> + Clone {
        Expr::parser(0).map(Self::Expr)
    }
}

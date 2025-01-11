pub mod lexer;
pub mod expr;

use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let filename = env::args().nth(1).expect("Expected file argument");
    let tokens = lexer::run_file(&filename)?;

    println!("{:?}", tokens);

    let ast = expr::parse(tokens);

    println!("AST is {}", ast?.render_tree().unwrap());

    Ok(())
}

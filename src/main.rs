pub mod lexer;

use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let filename = env::args().nth(1).expect("Expected file argument");
    let tokens = lexer::run_file(&filename)?;

    println!("{:?}", tokens);

    Ok(())
}

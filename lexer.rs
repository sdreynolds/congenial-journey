mod lexer;

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let _file_contents = fs::read_to_string(&path)?;
    Ok(())
}

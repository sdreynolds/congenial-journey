use std::{error::Error, fs};

type Offset = usize;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    Bool(Offset,  bool),
    BraceOpen(Offset),
    BraceClose(Offset),

    BracketOpen(Offset),
    BracketClose(Offset),

    Colon(Offset),
    Comma(Offset),
    Null(Offset),
    Equal(Offset),
    Semicolon(Offset),

    Number(f64, Offset),

    Local(Offset),

    EOF(Offset)
}

struct Lexer<'source> {
    file_contents: &'source str,
    pos: Offset,
}

impl <'source> Lexer<'source> {
    fn new(file_contents: &'source str) -> Self {
        Self { file_contents, pos: 0 }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens = Vec::new();

        while self.pos < self.file_contents.len() {
            if self.file_contents.get(self.pos..(self.pos + 4)) == Some("true") {
                tokens.push(Token::Bool(self.pos, true));
                self.pos += 4;
            }
            else if self.file_contents.get(self.pos..(self.pos + 5)) == Some("false") {
                tokens.push(Token::Bool(self.pos, false));
                self.pos += 4;
            }
            else {
                self.pos += 1;
            }
        }
        tokens.push(Token::EOF(self.pos));
        Ok(tokens)
    }
}

pub fn run_file(path: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let file_contents = fs::read_to_string(&path)?;
    let mut lexer = Lexer::new(&file_contents);
    lexer.tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn raw_true() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("true");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], Token::Bool(0, true));
        Ok(())
    }

    #[test]
    pub fn raw_false() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("false");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], Token::Bool(0, false));
        Ok(())
    }
}

use std::{error::Error, fs, iter::Peekable};

extern crate unicode_segmentation;
extern crate string_builder;

use unicode_segmentation::{Graphemes, UnicodeSegmentation};
use string_builder::Builder;

type Offset = usize;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    Bang(Offset),
    BangEqual(Offset),
    Bool(Offset,  bool),
    BraceOpen(Offset),
    BraceClose(Offset),

    BracketOpen(Offset),
    BracketClose(Offset),

    Colon(Offset),
    Comma(Offset),
    Null(Offset),
    Equal(Offset),
    DoubleEqual(Offset),
    Semicolon(Offset),

    Number(f64, Offset),

    Local(Offset),

    GreaterThan(Offset),
    GreaterThanOrEqual(Offset),
    LessThan(Offset),
    LessThanOrEqual(Offset),

    Dot(Offset),
    Plus(Offset),
    Dollar(Offset),

    String(Offset, String),

    EOF(Offset)
}

struct Lexer<'source> {
    file_contents: Peekable<Graphemes<'source>>,
    file_position: (u32, u32),
    pos: Offset,
}

impl <'source> Lexer<'source> {
    fn new(file_contents: &'source str) -> Self {
        Self { file_contents: UnicodeSegmentation::graphemes(file_contents, true).peekable(), pos: 0, file_position: (1, 0) }
    }

    fn is_at_end(&mut self) -> bool {
        self.file_contents.peek().is_none()
    }

    fn advance(&mut self) -> Option<&str> {
        let next_char = self.file_contents.next();
        // @TODO: this assumes 1 byte == 1 advance. This isn't true for utf-8.
        self.pos += 1;
        next_char
    }

    fn consume_comment(&mut self) {
        while let Some(head) = self.advance() {
            if head == "\n" {
                break;
            }
        }
    }

    fn lex_string(&mut self) -> Result<Token, Box<dyn Error>> {
        let mut closed_string = false;
        let mut builder = Builder::default();
        let start_pos = self.pos - 1;
        while let Some(head) = self.advance() {
            if head == "\"" {
                closed_string = true;
                break;
            }
            else {
                // append to new value
                builder.append(head);
            }
        }

        if !closed_string {
            Err("Didn't close string".into())
        } else {
            match builder.string() {
                Ok(s) => Ok(Token::String(start_pos, s)),
                Err(_failure) => Err("Failed to build string".into())
            }
        }
    }

    fn match_pos_and_advance(&mut self, expected: char) -> bool {
        let mut char_buf: [u8; 4] = [0; 4];
        // This implies match_pos_and_advance can only be called with ASCII. It is used for the main grammar
        // not used for parsing identifiers or strings.
        // @TODO: define these as constants so we don't have to do all this "Stuff"
        let expected: &str = expected.encode_utf8(&mut char_buf);

        match self.file_contents.peek() {
            Some(peek_value) => {
                let match_result = *peek_value == expected;
                if match_result {
                    self.advance();
                }
                match_result
            },
            None => false
        }
    }

    fn scan_token(&mut self) -> Result<Option<Token>, Box<dyn Error>> {
        match self.advance() {
            // Ignore Whitespace
            Some(" ") | Some("\r") | Some("\t") => Ok(None),
            Some("\n") => {
                // @TODO: should have a line counter and update that for each \n
                Ok(None)
            },

            Some("\"") => self.lex_string().map(|token| Some(token)),

            Some("[") => Ok(Some(Token::BracketOpen(self.pos))),
            Some("]") => Ok(Some(Token::BracketClose(self.pos))),
            Some("{") => Ok(Some(Token::BraceOpen(self.pos))),
            Some("}") => Ok(Some(Token::BraceClose(self.pos))),
            Some(":") => Ok(Some(Token::Colon(self.pos))),
            Some(",") => Ok(Some(Token::Comma(self.pos))),
            Some("=") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::DoubleEqual(self.pos)))
                } else {
                    Ok(Some(Token::Equal(self.pos)))
                }
            }
            Some(";") => Ok(Some(Token::Semicolon(self.pos))),
            Some("!") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::BangEqual(self.pos)))
                } else {
                    Ok(Some(Token::Bang(self.pos)))
                }
            },
            Some(">") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::GreaterThanOrEqual(self.pos)))
                } else {
                    Ok(Some(Token::GreaterThan(self.pos)))
                }
            },
            Some("<") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::LessThanOrEqual(self.pos)))
                } else {
                    Ok(Some(Token::LessThan(self.pos)))
                }
            },
            Some(".") => Ok(Some(Token::Dot(self.pos))),
            Some("+") => Ok(Some(Token::Plus(self.pos))),
            Some("$") => Ok(Some(Token::Dollar(self.pos))),

            Some("#") => {
                self.consume_comment();
                Ok(None)
            },
            Some("/") => {
                if self.match_pos_and_advance('/') {
                    self.consume_comment();
                    Ok(None)
                } else if self.match_pos_and_advance('*') {
                    // @TODO: NOPE! this doesn't check the end of the line for */
                    self.consume_comment();
                    Ok(None)
                } else {
                    Err("'/' isn't followed by * or /".into())
                }
            },

            _ => Err("Unkown token".into())
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => continue,
                Err(e) => return Err(e)
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
    pub fn raw_string() -> Result<(), Box<dyn Error>> {
        let expected_string = "multiword string is\nhere";
        let mut lexer = Lexer::new("\"multiword string is\nhere\"");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], Token::String(0, String::from(expected_string)));
        Ok(())
    }

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

    #[test]
    pub fn empty_string() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 1);

        assert_eq!(tokens[0], Token::EOF(0));
        Ok(())
    }

    #[test]
    pub fn array_of_booleans() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[true, false, true]");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 6);

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Bool(1, true),
            Token::Bool(7, false),
            Token::Bool(14, true),
            Token::BraceClose(18),
            Token::EOF(19)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn wrong_place_for_comma() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("true,");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 6);

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Bool(1, true),
            Token::Bool(7, false),
            Token::Bool(14, true),
            Token::BraceClose(18),
            Token::EOF(19)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn array_missing_comma() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[true false, true]");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 6);

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Bool(1, true),
            Token::Bool(7, false),
            Token::Bool(14, true),
            Token::BraceClose(18),
            Token::EOF(19)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }


}

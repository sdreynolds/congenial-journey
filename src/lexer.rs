use std::{error::Error, fs, iter::Peekable};

extern crate unicode_segmentation;
extern crate string_builder;

use phf::phf_map;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};
use string_builder::Builder;

type Offset = usize;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Token {
    Bang(Offset),
    BangEqual(Offset),

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
    Minus(Offset),
    Tilde(Offset),

    Number(Offset, f64),

    Local(Offset),

    GreaterThan(Offset),
    GreaterThanOrEqual(Offset),
    LessThan(Offset),
    LessThanOrEqual(Offset),

    Dot(Offset),
    Plus(Offset),
    Dollar(Offset),
    Asterisk(Offset),
    Slash(Offset),
    Modulo(Offset),

    String(Offset, String),
    Identifier(Offset, String),

    Bool(Offset,  bool),
    EOF(Offset)
}

enum Keyword {
    True,
    False,
}

static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
    "true" => Keyword::True,
    "false" => Keyword::False,
};


const RESERVED_CHARS: [char; 20] = [
    '!',
    '=',
    '[',
    ']',
    '{',
    '}',
    ':',
    ',',
    ';',
    '>',
    '<',
    '.',
    '+',
    '$',
    '&',
    '|',

    '\n',
    '\t',
    '\r',
    ' '
];

struct Lexer<'source> {
    file_contents: Peekable<Graphemes<'source>>,
    file_position: (u32, u32),
    pos: Offset
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    if is_digit(c) {
        false
    } else {
        for reserved_char in &RESERVED_CHARS {
            if *reserved_char == c {
                return false;
            }
        }
        true
    }
}

fn is_alpha_numeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

impl <'source> Lexer<'source> {
    fn new(file_contents: &'source str) -> Self {
        Self {
            file_contents: UnicodeSegmentation::graphemes(file_contents, true).peekable(),
            pos: 0,
            file_position: (1, 0)
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.file_contents.peek().is_none()
    }

    fn advance(&mut self) -> Option<&str> {
        let next_char = self.file_contents.next();
        // @TODO: this assumes 1 byte == 1 advance. This isn't true for utf-8.
        self.pos += 1;
        self.file_position.1 += 1;
        next_char
    }

    fn consume_comment(&mut self) {
        while let Some(head) = self.advance() {
            if head == "\n" {
                self.file_position.0 += 1;
                break;
            }
        }
    }

    fn lex_identifier(&mut self, mut builder: Builder) -> Result<Token, Box<dyn Error>> {
        let start = self.pos - 1;
        while let Some(head) = self.file_contents.peek() {
            // Checks the first character and if it isn't a reserved treat it as valid.
            // I am sure this won't work with lots of strings...
            let valid_str = head.chars().nth(0).map(is_alpha_numeric).unwrap_or_default();
            if valid_str {
                self.advance().map(|c| builder.append(c));
            } else {
                break;
            }
        }
        match builder.string() {
            Ok(s) => Ok(Token::Identifier(start, s)),
            Err(_failed_build) => Err("Failed to build a string".into())
        }
    }

    fn lex_number(&mut self, mut builder: Builder) -> Result<Token, Box<dyn Error>> {
        let mut parse_decimal = false;
        let start = self.pos - 1;
        while let Some(head) = self.file_contents.peek() {
            let valid_digit = head.chars().nth(0).map(is_digit).unwrap_or_default();
            if valid_digit {
                builder.append(*head);
                self.advance();
            } else {
                if *head == "." {
                    parse_decimal = true;
                    builder.append(*head);
                    self.advance();
                }
                break;
            }
        }

        if parse_decimal {
            while let Some(head) = self.file_contents.peek() {
                let valid_digit = head.chars().nth(0).map(is_digit).unwrap_or_default();
                if valid_digit {
                    builder.append(*head);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        match builder.string() {
            Ok(s) => {
                match s.parse::<f64>() {
                    Ok(number) => Ok(Token::Number(start, number)),
                    Err(_failed_parse) => Err("Failed to parse number".into())
                }

            },
            Err(_falure) => Err("failed to build number".into())
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
            else if head == "\n" || head == "\r\n" {
                // Multiline strings are invalid.
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
            Some("\n") | Some("\r\n")=> {
                self.file_position.0 += 1;
                Ok(None)
            },

            Some("\"") => self.lex_string().map(|token| Some(token)),

            Some("[") => Ok(Some(Token::BraceOpen(self.pos - 1))),
            Some("]") => Ok(Some(Token::BraceClose(self.pos - 1))),
            Some("{") => Ok(Some(Token::BracketOpen(self.pos - 1))),
            Some("}") => Ok(Some(Token::BracketClose(self.pos - 1))),
            Some(":") => Ok(Some(Token::Colon(self.pos - 1))),
            Some(",") => Ok(Some(Token::Comma(self.pos - 1))),
            Some("=") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::DoubleEqual(self.pos - 1)))
                } else {
                    Ok(Some(Token::Equal(self.pos - 1)))
                }
            }
            Some(";") => Ok(Some(Token::Semicolon(self.pos - 1))),
            Some("!") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::BangEqual(self.pos - 1)))
                } else {
                    Ok(Some(Token::Bang(self.pos - 1)))
                }
            },
            Some(">") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::GreaterThanOrEqual(self.pos - 1)))
                } else {
                    Ok(Some(Token::GreaterThan(self.pos - 1)))
                }
            },
            Some("<") => {
                if self.match_pos_and_advance('=') {
                    Ok(Some(Token::LessThanOrEqual(self.pos - 1)))
                } else {
                    Ok(Some(Token::LessThan(self.pos - 1)))
                }
            },
            Some(".") => Ok(Some(Token::Dot(self.pos - 1))),
            Some("+") => Ok(Some(Token::Plus(self.pos - 1))),
            Some("$") => Ok(Some(Token::Dollar(self.pos - 1))),
            Some("*") => Ok(Some(Token::Asterisk(self.pos - 1))),
            Some("%") => Ok(Some(Token::Modulo(self.pos - 1))),

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
                    Ok(Some(Token::Slash(self.pos - 1)))
                }
            },

            Some(lexme) => {
                let first_char = lexme.chars().nth(0);
                let valid_digit = first_char.map(is_digit).unwrap_or_default();
                let valid_alpha = first_char.map(is_alpha).unwrap_or_default();

                if valid_digit {
                    let mut builder = Builder::default();
                    builder.append(lexme);
                    self.lex_number(builder).map(|token| Some(token))
                } else if valid_alpha {
                    let mut builder = Builder::default();
                    builder.append(lexme);

                    let identifier_token = self.lex_identifier(builder);

                    identifier_token.map(|token| {
                        if let Token::Identifier(offset, name) = token {
                            match KEYWORDS.get(&name) {
                                Some(keyword) => match keyword {
                                    Keyword::True => Some(Token::Bool(offset, true)),
                                    Keyword::False => Some(Token::Bool(offset, false))
                                },
                                _ => Some(Token::Identifier(offset, name))
                            }
                        } else {
                            Some(token)
                        }
                    })

                } else {
                    Err(format!("Unkown token {lexme}").into())
                }
            },
            None => Err("Bad input".into())
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
        let expected_string = "multiword string is here";
        let mut lexer = Lexer::new("\"multiword string is here\"");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], Token::String(0, String::from(expected_string)));
        Ok(())
    }

    #[test]
    pub fn raw_int() -> Result<(), Box<dyn Error>> {
        let expected_int = 64.0;
        let mut lexer = Lexer::new("64");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], Token::Number(0, expected_int));
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
        let mut lexer = Lexer::new("\r\n");
        let tokens = lexer.tokenize()?;
        assert_eq!(tokens.len(), 1);

        assert_eq!(tokens[0], Token::EOF(1));
        Ok(())
    }

    #[test]
    pub fn array_of_strings() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[\"sweet\", \"face\", \"nice\"]");
        let tokens = lexer.tokenize()?;

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::String(1, "sweet".to_string()),
            Token::Comma(8),
            Token::String(10, "face".to_string()),
            Token::Comma(16),
            Token::String(18, "nice".to_string()),
            Token::BraceClose(18 + "\"nice\"".to_string().len()),
            Token::EOF(25)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn array_of_identifiers() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[sweet, face, nice]");
        let tokens = lexer.tokenize()?;

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Identifier(1, "sweet".to_string()),
            Token::Comma(6),
            Token::Identifier(8, "face".to_string()),
            Token::Comma(12),
            Token::Identifier(14, "nice".to_string()),
            Token::BraceClose(18),
            Token::EOF(19)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn array_of_booleans() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[true, false, true]");
        let tokens = lexer.tokenize()?;

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Bool(1, true),
            Token::Comma(5),
            Token::Bool(7, false),
            Token::Comma(12),
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

        let expected_tokens = vec![
            Token::Bool(0, true),
            Token::Comma(4),
            Token::EOF(5)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn array_missing_comma() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("[true false, true]");
        let tokens = lexer.tokenize()?;

        let expected_tokens = vec![
            Token::BraceOpen(0),
            Token::Bool(1, true),
            Token::Bool(6, false),
            Token::Comma(11),
            Token::Bool(13, true),
            Token::BraceClose(17),
            Token::EOF(18)
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    pub fn division() -> Result<(), Box<dyn Error>> {
        let mut lexer = Lexer::new("24 / 7");
        let tokens = lexer.tokenize()?;

        let expected_tokens = vec![
            Token::Number(0, 24.0),
            Token::Slash(3),
            Token::Number(5, 7.0),
            Token::EOF(6)
        ];
        assert_eq!(tokens, expected_tokens);
        Ok(())
    }


}

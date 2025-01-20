use std::error::Error;

use crate::lexer::Token;

#[derive(Debug)]
pub struct AST {
    literal_exprs: Vec<LiteralExpr>,
    binary_exprs: Vec<BinaryExpr>,
    unary_exprs: Vec<UnaryExpr>,

    parse_errors: Vec<ParseError>,

    root_expr_type: ExprType,
    root_expr_id: ExprId,
}

impl AST {
    fn get_literal_expr(&self, index: ExprId) -> Option<&LiteralExpr> {
        self.literal_exprs.get(index)
    }
    fn get_binary_expr(&self, index: ExprId) -> Option<&BinaryExpr> {
        self.binary_exprs.get(index)
    }
    fn get_unary_expr(&self, index: ExprId) -> Option<&UnaryExpr> {
        self.unary_exprs.get(index)
    }

    pub fn render_tree(&self) -> Option<String> {
        match self.root_expr_type {
            ExprType::Literal => self.literal_exprs.get(self.root_expr_id).and_then(|n| n.tree_render(self)),
            ExprType::Binary => self.binary_exprs.get(self.root_expr_id).and_then(|n| n.tree_render(self)),
            ExprType::Unary => self.unary_exprs.get(self.root_expr_id).and_then(|n| n.tree_render(self)),
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
struct ParseError {
    pos: usize,
    message: String,
}

trait Expr {
    fn tree_render(&self, tree: &AST) -> Option<String>;
}

type ExprId = usize;

#[derive(Debug)]
#[derive(Clone)]
struct BinaryExpr {
    left: ExprId,
    left_type: ExprType,
    right: ExprId,
    right_type: ExprType,
    operator: Token
}

#[derive(Debug)]
#[derive(Clone)]
struct UnaryExpr {
    operator: Token,
    right: ExprId,
    right_type: ExprType,
}

impl Expr for UnaryExpr {
    fn tree_render(&self, tree: &AST) -> Option<String> {
        let operator_str = match self.operator {
            Token::Bang(_) => Some("!"),
            _ => None
        };

        let right_str = match self.right_type {
            ExprType::Literal => tree.get_literal_expr(self.right).and_then(|r| r.tree_render(tree)),
            ExprType::Binary => tree.get_binary_expr(self.right).and_then(|r| r.tree_render(tree)),
            ExprType::Unary => tree.get_unary_expr(self.right).and_then(|r| r.tree_render(tree)),
        };

        if let (Some(op), Some(right)) = (operator_str, right_str) {
            Some(format!("({op} {right})"))
        } else {
            None
        }
    }
}


impl Expr for LiteralExpr {
    fn tree_render(&self, _tree: &AST) -> Option<String> {
        match &self.value {
            Token::Number(_offset, v) => Some(v.to_string()),
            Token::String(_offset, v) => Some(format!("\"{v}\"")),
            Token::Bool(_offset, v) => Some(v.to_string()),
            Token::Null(_) => Some("null".into()),
            _ => None
        }
    }
}


impl Expr for BinaryExpr {
    fn tree_render(&self, tree: &AST) -> Option<String> {
        let operator_str = match self.operator {
            Token::Dot(_) => Some("."),
            Token::Plus(_) => Some("+"),
            Token::DoubleEqual(_) => Some("=="),
            Token::BangEqual(_) => Some("!="),
            Token::Slash(_) => Some("/"),
            Token::Asterisk(_) => Some("*"),
            Token::Modulo(_) => Some("%"),
            Token::Minus(_) => Some("-"),
            Token::ShiftLeft(_) => Some("<<"),
            Token::ShiftRight(_) => Some(">>"),
            Token::GreaterThan(_) => Some(">"),
            Token::LessThan(_) => Some("<"),
            Token::GreaterThanOrEqual(_) => Some(">="),
            Token::LessThanOrEqual(_) => Some("<="),
            Token::Ampersand(_) => Some("&"),
            Token::Caret(_) => Some("^"),
            Token::Bar(_) => Some("|"),
            Token::And(_) => Some("&&"),
            Token::Or(_) => Some("||"),

            _ => None
        };

        let left_str = match self.left_type {
            ExprType::Literal => tree.get_literal_expr(self.left).and_then(|l| l.tree_render(tree)),
            ExprType::Binary => tree.get_binary_expr(self.left).and_then(|l| l.tree_render(tree)),
            ExprType::Unary => tree.get_unary_expr(self.left).and_then(|l| l.tree_render(tree)),
        };

        let right_str = match self.right_type {
            ExprType::Literal => tree.get_literal_expr(self.right).and_then(|r| r.tree_render(tree)),
            ExprType::Binary => tree.get_binary_expr(self.right).and_then(|r| r.tree_render(tree)),
            ExprType::Unary => tree.get_unary_expr(self.right).and_then(|r| r.tree_render(tree)),
        };


        if let (Some(op), Some(left), Some(right)) = (operator_str, left_str, right_str) {
            let binary_representation = format!("({op} {left} {right})");
            Some(binary_representation)
        } else {
            None
        }
    }
}

// Use ExprType to figure out what vector each one is in
// u32 is used for the location in the vector
#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
enum ExprType {
    Literal,
    Binary,
    Unary,
}

#[derive(Debug)]
#[derive(Clone)]
struct LiteralExpr {
    value: Token
}


struct Parser {
    current: usize,
    tokens: Vec<Token>,

    literal_exprs: Vec<LiteralExpr>,
    binary_exprs: Vec<BinaryExpr>,
    unary_exprs: Vec<UnaryExpr>,

    parse_errors: Vec<ParseError>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser{
            current: 0,
            tokens,
            literal_exprs: Vec::new(),
            binary_exprs: Vec::new(),
            unary_exprs: Vec::new(),
            parse_errors: Vec::new(),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            // Valid Token streams have at least one element that is EOF. Having None is weird
            Some(Token::EOF(_)) | None => true,
            _ => false
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn boolean_or(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.boolean_and() {
            self.peek().and_then(|token| {
                match token {
                    Token::Or(offset) => Some(Token::Or(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.boolean_or() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn boolean_and(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.bitwise_or() {
            self.peek().and_then(|token| {
                match token {
                    Token::And(offset) => Some(Token::And(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.boolean_and() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn bitwise_or(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.bitwise_xor() {
            self.peek().and_then(|token| {
                match token {
                    Token::Bar(offset) => Some(Token::Bar(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.bitwise_or() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn bitwise_xor(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.bitwise_and() {
            self.peek().and_then(|token| {
                match token {
                    Token::Caret(offset) => Some(Token::Caret(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.bitwise_xor() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn bitwise_and(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.equals() {
            self.peek().and_then(|token| {
                match token {
                    Token::Ampersand(offset) => Some(Token::Ampersand(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.bitwise_and() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn equals(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.comparison() {
            self.peek().and_then(|token| {
                match token {
                    Token::BangEqual(offset) => Some(Token::BangEqual(*offset)),
                    Token::DoubleEqual(offset) => Some(Token::DoubleEqual(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.equals() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn comparison(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.shift() {
            self.peek().and_then(|token| {
                match token {
                    Token::GreaterThan(offset) => Some(Token::GreaterThan(*offset)),
                    Token::LessThan(offset) => Some(Token::LessThan(*offset)),
                    Token::GreaterThanOrEqual(offset) => Some(Token::GreaterThanOrEqual(*offset)),
                    Token::LessThanOrEqual(offset) => Some(Token::LessThanOrEqual(*offset)),

                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.comparison() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() -1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn shift(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.term() {
            self.peek().and_then(|token| {
                match token {
                    Token::ShiftRight(offset) => Some(Token::ShiftRight(*offset)),
                    Token::ShiftLeft(offset) => Some(Token::ShiftLeft(*offset)),
                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.shift() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() - 1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn term(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.factor() {
            self.peek().and_then(|token| {
                match token {
                    Token::Plus(offset) => Some(Token::Plus(*offset)),
                    Token::Minus(offset) => Some(Token::Minus(*offset)),
                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.term() {
                    self.binary_exprs.push(
                        BinaryExpr{left, left_type, right, right_type, operator}
                    );
                    Some((self.binary_exprs.len() - 1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn factor(&mut self) -> Option<(usize, ExprType)> {
        if let Some((left, left_type)) = self.unary() {
            self.peek().and_then(|token| {
                match token {
                    Token::Slash(offset) => Some(Token::Slash(*offset)),
                    Token::Asterisk(offset) => Some(Token::Asterisk(*offset)),
                    Token::Modulo(offset) => Some(Token::Modulo(*offset)),
                    _ => None
                }
            }).and_then(|operator| {
                self.current += 1;
                if let Some((right, right_type)) = self.factor() {
                    self.binary_exprs.push(
                        BinaryExpr { left, left_type , right , right_type, operator}
                    );
                    Some((self.binary_exprs.len() - 1, ExprType::Binary))
                } else {
                    None
                }
            }).or_else(|| Some((left, left_type)))
        } else {
            None
        }
    }

    fn unary(&mut self) -> Option<(usize, ExprType)> {
        match self.peek() {
            // Create the operator if possible
            Some(Token::Bang(offset)) => Some(Token::Bang(*offset)),
            Some(Token::Minus(offset)) => Some(Token::Minus(*offset)),
            Some(Token::Plus(offset)) => Some(Token::Plus(*offset)),
            Some(Token::Tilde(offset)) => Some(Token::Tilde(*offset)),
            _ => None
        }.and_then(|operator| {
            // consume the token
            self.current += 1;
            // recurse to find the matching right side
            if let Some((right, right_type)) = self.unary() {
                self.unary_exprs.push(UnaryExpr{
                    operator,
                    right,
                    right_type,
                });

                Some((self.unary_exprs.len() - 1, ExprType::Unary))
            } else {
                None
            }
        })
        // recurse to lower priority rule
        .or_else(|| self.primary())
    }

    fn primary(&mut self) -> Option<(usize, ExprType)> {
        let maybe_literal = match self.peek() {
            Some(Token::Bool(offset, v)) => {
                Some(LiteralExpr{value: Token::Bool(*offset, *v)})
            },
            Some(Token::String(offset, v)) => {
                Some(LiteralExpr{value: Token::String(*offset, v.clone())})
            },
            Some(Token::Number(offset, v)) => {
                Some(LiteralExpr{value: Token::Number(*offset, *v)})
            }
            Some(Token::Null(offset)) => {
                Some(LiteralExpr{value: Token::Null(*offset)})
            }
            _ => None
        };

        if maybe_literal.is_some() {
            self.advance();
            self.literal_exprs.push(maybe_literal.unwrap());
            Some((self.literal_exprs.len() - 1, ExprType::Literal))
        } else {
            None
        }
    }

    fn parse(&mut self) -> Option<AST> {
        let mut root_expr_id = None;
        let mut root_expr_type = None;
        while !self.is_at_end() {
            if let Some((found_expr_id, found_expr_type)) = self.boolean_or() {
                root_expr_type = Some(found_expr_type);
                root_expr_id = Some(found_expr_id);
            }
        }

        let valid_parse = root_expr_id
            .and(root_expr_type)
            .and(self.peek())
            .is_some_and(|token| {
                if let Token::EOF(_) = token {
                    true
                } else {
                    false
                }
            });


        if valid_parse {
            Some(AST {
                literal_exprs: self.literal_exprs.clone(),
                binary_exprs: self.binary_exprs.clone(),
                unary_exprs: self.unary_exprs.clone(),
                parse_errors: self.parse_errors.clone(),
                root_expr_id: root_expr_id.expect("Root expr id should be check by if statement"),
                root_expr_type: root_expr_type.expect("Root type should be checked above by if statement"),
            })
        } else {
            None
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<AST, Box<dyn Error>> {

    let mut parser = Parser::new(tokens);

    parser.parse().ok_or("Couldn't parse".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn print_double_equals() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::String(0, "this is awesome".to_string()),
                          Token::DoubleEqual(10),
                          Token::Number(14, 58.0),
                          Token::EOF(123)
        );
        let ast = parse(tokens)?;

        assert_eq!("(== \"this is awesome\" 58)", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn print_nested_double_equals() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::String(0, "this is awesome".to_string()),
            Token::DoubleEqual(10),
            Token::String(0, "this is awesome".to_string()),
            Token::DoubleEqual(1283),
            Token::Number(14, 58.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;

        assert_eq!("(== \"this is awesome\" (== \"this is awesome\" 58))",
                   ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn parse_single_boolean() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Bool(0, true),
                          Token::EOF(123));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "true".to_string());
        Ok(())
    }

    #[test]
    pub fn parse_single_null() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Null(0),
                          Token::EOF(123));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "null".to_string());
        Ok(())
    }

    #[test]
    pub fn parse_not_boolean() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Bang(0), Token::Bool(1, true),
                          Token::EOF(123));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "(! true)".to_string());
        Ok(())
    }

    #[test]
    pub fn parse_not_not_boolean() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Bang(0), Token::Bang(0), Token::Bool(2, true),
                          Token::EOF(123));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "(! (! true))".to_string());
        Ok(())
    }

    #[test]
    pub fn parse_and_print_double_equals() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::String(0, "This is awesome".to_string()),
            Token::DoubleEqual(14),
            Token::Number(16, 58.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(== \"This is awesome\" 58)", ast.render_tree().unwrap());
        Ok(())
    }
    #[test]
    pub fn parse_and_print_division() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 100.0),
            Token::Slash(14),
            Token::Number(16, 58.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(/ 100 58)", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn parse_and_print_multiplication() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 100.0),
            Token::Asterisk(14),
            Token::Number(16, 58.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(* 100 58)", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn parse_and_print_modulo() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 100.0),
            Token::Modulo(14),
            Token::Number(16, 58.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(% 100 58)", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn multiple_multiplications() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 100.0),
            Token::Slash(14),
            Token::Number(20, 40000.23),
            Token::Asterisk(18),
            Token::Number(89, 67832.5478),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(/ 100 (* 40000.23 67832.5478))", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn modulo_and_addition() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::Plus(28),
            Token::Number(123, 45.0),
            Token::Modulo(0),
            Token::Number(82, 76.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(+ 200 (% 45 76))", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn multiplication_and_subtraction() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::Minus(28),
            Token::Number(123, 45.0),
            Token::Asterisk(0),
            Token::Number(82, 76.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(- 200 (* 45 76))", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn shift_left() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::ShiftLeft(28),
            Token::Number(123, 45.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(<< 200 45)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn shift_right() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::ShiftRight(28),
            Token::Number(123, 45.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(>> 200 45)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn multiple_compare() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::GreaterThanOrEqual(2),
            Token::Number(0, 450.0),
            Token::LessThan(0),
            Token::Number(0, 780.7),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(>= 200 (< 450 780.7))", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn bitwise_and() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::Ampersand(2),
            Token::Number(0, 450.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(& 200 450)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn bitwise_xor() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::Caret(2),
            Token::Number(0, 450.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(^ 200 450)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn bitwise_or() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::Bar(2),
            Token::Number(0, 450.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(| 200 450)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn boolean_and() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(
            Token::Number(0, 200.0),
            Token::And(2),
            Token::Number(0, 450.0),
            Token::EOF(123)
        );
        let ast = parse(tokens)?;
        assert_eq!("(&& 200 450)", ast.render_tree().unwrap());

        Ok(())
    }

    #[test]
    pub fn booleans() -> Result<(), Box<dyn Error>> {
        let tokens = vec![
            Token::Number(0, 24.0),
            Token::GreaterThan(3),
            Token::Number(5, 0.0),
            Token::And(7),
            Token::Number(10, 14.0),
            Token::LessThan(13),
            Token::Number(15, 12.0),
            Token::Or(18),
            Token::Number(21, 78.0),
            Token::BangEqual(24),
            Token::Number(27, 96.0),
            Token::EOF(29)
        ];
        let ast = parse(tokens)?;
        assert_eq!("(|| (&& (> 24 0) (< 14 12)) (!= 78 96))", ast.render_tree().unwrap());

        Ok(())
    }
}

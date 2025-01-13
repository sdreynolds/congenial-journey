use std::error::Error;

use crate::lexer::Token;

#[derive(Debug)]
pub struct AST {
    literal_exprs: Vec<LiteralExpr>,
    binary_exprs: Vec<BinaryExpr>,
    unary_exprs: Vec<UnaryExpr>,

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
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser{
            current: 0,
            tokens,
            literal_exprs: Vec::new(),
            binary_exprs: Vec::new(),
            unary_exprs: Vec::new(),
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

    fn unary(&mut self) -> Option<usize> {

        let found_offset = match self.peek() {
            Some(Token::Bang(_)) => true,
            _ => false
        };

        if found_offset {
            self.current += 1;
        } else {
            return None;
        }

        let parsed = {
            if let Some(Token::Bang(offset)) = self.previous() {
                let operator = Token::Bang(*offset);
                if let Some(right) = self.unary() {
                    self.unary_exprs.push(UnaryExpr{
                        operator,
                        right,
                        right_type: ExprType::Unary,
                    });

                    Some(self.unary_exprs.len() - 1)
                } else if let Some(right) = self.primary() {
                    self.unary_exprs.push(UnaryExpr{
                        operator,
                        right,
                        right_type: ExprType::Literal,
                    });
                    Some(self.unary_exprs.len() - 1)
                } else {
                    None
                }
            } else {
                None
            }
        };

        parsed
    }

    fn primary(&mut self) -> Option<usize> {
        let maybe_literal = match self.peek() {
            Some(Token::Bool(offset, v)) => {
                Some(LiteralExpr{value: Token::Bool(*offset, *v)})
            },
            Some(Token::String(offset, v)) => {
                Some(LiteralExpr{value: Token::String(*offset, v.clone())})
            },
            _ => None
        };

        if maybe_literal.is_some() {
            self.advance();
            self.literal_exprs.push(maybe_literal.unwrap());
            Some(self.literal_exprs.len() - 1)
        } else {
            None
        }
    }

    fn parse(&mut self) -> Option<AST> {
        if let Some(root) = self.unary() {
            Some(AST {
                literal_exprs: self.literal_exprs.clone(),
                binary_exprs: self.binary_exprs.clone(),
                unary_exprs: self.unary_exprs.clone(),
                root_expr_id: root,
                root_expr_type: ExprType::Unary,
            })
        }
        else if let Some(root) = self.primary() {
            Some(AST {
            literal_exprs: self.literal_exprs.clone(),
            binary_exprs: self.binary_exprs.clone(),
            unary_exprs: self.unary_exprs.clone(),
            root_expr_id: root,
            root_expr_type: ExprType::Literal,
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
        let ast = AST{
            literal_exprs: vec!(
                LiteralExpr{value: Token::String(0, "this is awesome".to_string())},
                LiteralExpr{value: Token::Number(14, 58.0)},
            ),
            binary_exprs: vec!(
                BinaryExpr{
                    left: 0,
                    left_type: ExprType::Literal,
                    right: 1,
                    right_type: ExprType::Literal,
                    operator: Token::DoubleEqual(10)
                }
            ),
            unary_exprs: vec!(),
            root_expr_id: 0,
            root_expr_type: ExprType::Binary,
        };
        assert_eq!("(== \"this is awesome\" 58)", ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn print_nested_double_equals() -> Result<(), Box<dyn Error>> {
        let ast = AST{
            literal_exprs: vec!(
                LiteralExpr{value: Token::String(0, "this is awesome".to_string())},
                LiteralExpr{value: Token::Number(14, 58.0)},
            ),
            binary_exprs: vec!(
                BinaryExpr{
                    left: 0,
                    left_type: ExprType::Literal,
                    right: 1,
                    right_type: ExprType::Binary,
                    operator: Token::DoubleEqual(10)
                },
                BinaryExpr{
                    left: 0,
                    left_type: ExprType::Literal,
                    right: 1,
                    right_type: ExprType::Literal,
                    operator: Token::DoubleEqual(40),
                }
            ),
            unary_exprs: vec!(),
            root_expr_id: 0,
            root_expr_type: ExprType::Binary,
        };
        assert_eq!("(== \"this is awesome\" (== \"this is awesome\" 58))",
                   ast.render_tree().unwrap());
        Ok(())
    }

    #[test]
    pub fn parse_single_boolean() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Bool(0, true));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "true".to_string());
        Ok(())
    }

    #[test]
    pub fn parse_not_boolean() -> Result<(), Box<dyn Error>> {
        let tokens = vec!(Token::Bang(0), Token::Bool(1, true));
        let tree = parse(tokens)?;
        assert_eq!(tree.render_tree().unwrap(), "(! true)".to_string());
        Ok(())
    }
}

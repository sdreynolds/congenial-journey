use std::error::Error;

use crate::lexer::Token;

#[derive(Debug)]
pub struct AST {
    literal_exprs: Vec<LiteralExpr>,
    binary_exprs: Vec<BinaryExpr>,
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

    pub fn render_tree(&self) -> Option<String> {
        match self.root_expr_type {
            ExprType::Literal => self.literal_exprs.get(self.root_expr_id).and_then(|n| n.tree_render(self)),
            ExprType::Binary => self.binary_exprs.get(self.root_expr_id).and_then(|n| n.tree_render(self)),
        }
    }
}

trait Expr {
    fn tree_render(&self, tree: &AST) -> Option<String>;
}

type ExprId = usize;

#[derive(Debug)]
struct BinaryExpr {
    left: ExprId,
    left_type: ExprType,
    right: ExprId,
    right_type: ExprType,
    operator: Token
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
            ExprType::Binary => tree.get_binary_expr(self.left).and_then(|l| l.tree_render(tree))
        };

        let right_str = match self.right_type {
            ExprType::Literal => tree.get_literal_expr(self.right).and_then(|r| r.tree_render(tree)),
            ExprType::Binary => tree.get_binary_expr(self.right).and_then(|r| r.tree_render(tree))
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
enum ExprType {
    Literal,
    Binary
}

#[derive(Debug)]
struct LiteralExpr {
    value: Token
}

pub fn parse(_tokens: Vec<Token>) -> Result<AST, Box<dyn Error>> {

    let mut literal_exprs: Vec<LiteralExpr> = Vec::new();
    let binary_exprs: Vec<BinaryExpr> = Vec::new();

    literal_exprs.push(LiteralExpr{value: Token::Bool(0, false)});

    Ok(AST {
        literal_exprs,
        binary_exprs,
        root_expr_id: 0,
        root_expr_type: ExprType::Literal,
    })
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
}

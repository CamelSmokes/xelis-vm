use crate::Type;

use super::{context::Context, Expression, Operator, Parser, ParserError};

#[derive(Debug, PartialEq, Clone)]
/// Not all operators which are needed to be ordered by the shunting yard
/// algorithm are present in the `Operator` enum. This enum wraps
/// `Operator` and holds the others.
pub enum YardOp {
    Standard(Operator),
    Subscript,
    Path,
    LogicalNot,
    Cast(Type),
}

impl YardOp {
    pub fn parameter_count(&self) -> u64 {
        match self {
            YardOp::Standard(_) => 2,
            YardOp::Subscript => 2,
            YardOp::Path => 2,
            YardOp::Cast(_) => 1,
            YardOp::LogicalNot => 1,
        }
    }
    fn precedence(&self) -> u64 {
        match self {
            YardOp::Path => 15,
            YardOp::Subscript => 12,
            YardOp::LogicalNot => 11,
            YardOp::Cast(_) => 10,
            YardOp::Standard(s) => s.precedence(),
        }
    }
}
#[derive(Debug, PartialEq)]
/// The shunting yard algorithm requires Left parenthesis to be inserted into
/// the operator stack. This wrapper allows that.
enum PossiblyParenOpen<T> {
    ParenOpen,
    Token(T),
}

#[derive(Debug, Default)]

pub struct ShuntingYard {
    parenthesis_depth: u64,
    output_stack: Vec<Expression>,
    operator_stack: Vec<PossiblyParenOpen<YardOp>>,
}
impl<'a> ShuntingYard {
    pub fn new() -> ShuntingYard {
        Self::default()
    }
    pub fn insert_op(
        &mut self,
        op: YardOp,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<(), ParserError<'a>> {
        while let Some(PossiblyParenOpen::Token(op2)) = self.operator_stack.last() {
            let op2 = op2.clone();
            // TODO: Handle Associativity
            if op2.precedence() >= op.precedence() {
                self.operator_stack.pop().unwrap();
                self.push_op_to_output(op2, parser, context)?
            } else {
                break;
            }
        }
        self.operator_stack.push(PossiblyParenOpen::Token(op));
        Ok(())
    }

    /// The output stack contains expressions.
    /// To push an operator to the output stack, it must be converted to an
    /// expression.
    fn push_op_to_output(
        &mut self,
        operator: YardOp,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<(), ParserError<'a>> {
        let mut params: Vec<Expression> = Vec::new();
        let param_count = operator.parameter_count();

        for _ in 0..param_count {
            let param = self.pop_expression()?;
            params.push(param);
        }
        // TODO: Handle unwrapping cleaner
        let param_1 = params.pop().unwrap();
        let param_2 = params.pop();
        // construct operator
        let expr = match operator {
            YardOp::Standard(op) => {
                Expression::Operator(op, Box::new(param_1), Box::new(param_2.unwrap()))
            },
            YardOp::Subscript => {
                // let index = param_1;
                // let array = param_2.unwrap();
                // println!("AA {:?}, {:?}", array, index);

                // if !parser
                //     .get_type_from_expression(None, &array, context)?
                //     .is_array()
                // {
                //     return Err(ParserError::InvalidArrayCall);
                // }

                // // Index must be of type u64
                // let index_type = parser.get_type_from_expression(None, &index, context)?;
                // if *index_type != Type::U64 {
                //     return Err(ParserError::InvalidArrayCallIndexType(
                //         index_type.into_owned(),
                //     ));
                // }

                // Expression::ArrayCall(Box::new(array), Box::new(index))
                todo!();
            },
            YardOp::Path => Expression::Path(Box::new(param_1), Box::new(param_2.unwrap())),
            YardOp::LogicalNot => {
                let expr_type = parser.get_type_from_expression(None, &param_1, context)?;
                if *expr_type != Type::Bool {
                    return Err(ParserError::InvalidValueType(
                        expr_type.into_owned(),
                        Type::Bool,
                    ));
                }
                Expression::IsNot(Box::new(param_1))
            },
            YardOp::Cast(right_type) => {
                let left_type = parser
                    .get_type_from_expression(None, &param_1, context)?
                    .into_owned();

                if !left_type.is_castable_to(&right_type) {
                    return Err(ParserError::CastError(left_type, right_type));
                }
                Expression::Cast(Box::new(param_1), right_type)
            },
        };
        self.output_stack.push(expr);
        Ok(())
    }

    /// Pops the top value on the output stack if it is an expression
    pub fn pop_expression(&mut self) -> Result<Expression, ParserError<'a>> {
        match self.output_stack.pop() {
            Some(expr) => Ok(expr),
            None => {
                todo!()
            },
        }
    }
    /// Return a reference to the operator at the top of the operator stack
    pub fn peek_operator(&self) -> Option<&YardOp> {
        match self.operator_stack.last() {
            Some(PossiblyParenOpen::Token(op)) => Some(op),
            Some(PossiblyParenOpen::ParenOpen) => None,
            None => None,
        }
    }
    pub fn insert_expr(&mut self, expr: Expression) {
        self.output_stack.push(expr);
    }
    pub fn open_paren(&mut self) {
        self.parenthesis_depth += 1;
        self.operator_stack.push(PossiblyParenOpen::ParenOpen)
    }
    pub fn close_paren(
        &mut self,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<(), ParserError<'a>> {
        let current_depth = self.parenthesis_depth;
        if current_depth == 0 {
            return Err(ParserError::ReturnAlreadyInElse);
        }
        self.parenthesis_depth -= 1;
        while PossiblyParenOpen::ParenOpen != *self.operator_stack.last().expect("TODO") {
            {
                if let Some(PossiblyParenOpen::Token(op)) = self.operator_stack.pop() {
                    self.push_op_to_output(op, parser, context)?;
                } else {
                    unreachable!("TODO: ERROR IMPOSSIBLE")
                };
            }
        }

        assert_eq!(
            self.operator_stack.pop(),
            Some(PossiblyParenOpen::ParenOpen)
        );
        Ok(())
    }
    pub fn get_paren_depth(&self) -> u64 {
        self.parenthesis_depth
    }

    pub fn finish(
        &mut self,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<Expression, ParserError<'a>> {
        let mut operators: Vec<YardOp> = Vec::new();
        for op in self.operator_stack.drain(..) {
            match op {
                PossiblyParenOpen::Token(op) => {
                    operators.push(op);
                },
                PossiblyParenOpen::ParenOpen => {
                    todo!("ERROR: Found open paren in operator stack")
                },
            }
        }
        operators.reverse();
        for op in operators {
            self.push_op_to_output(op, parser, context)?;
        }
        // TODO: make real error
        assert!(self.output_stack.len() == 1);
        self.pop_expression()
    }
}

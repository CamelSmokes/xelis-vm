//

use crate::Type;

use super::{context::Context, Expression, Operator, Parser, ParserError, Token};

#[derive(Debug, PartialEq, Clone)]
/// Not all operators which are needed to be ordered by the shunting yard
/// algorithm are present in the `Operator` enum. This enum wraps
/// `Operator` and holds the others.
pub enum YardOp {
    Standard(Operator),
    LogicalNot,
    /// As far as the shunting yard algorithm part of the parser is concerned,
    /// Ternary operators are postfix operators.
    Ternary(Expression, Expression),
    Cast(Type),
}

impl YardOp {
    fn precedence(&self) -> u64 {
        // TODO make precedences sensible
        match self {
            YardOp::Ternary(..) => 1,
            YardOp::LogicalNot => 11,
            YardOp::Cast(_) => 10,
            YardOp::Standard(s) => match s {
                Operator::Multiply | Operator::Divide | Operator::Rem => 9,
                Operator::Plus | Operator::Minus => 8,
                Operator::BitwiseLeft | Operator::BitwiseRight => 7,
                Operator::BitwiseAnd => 6,
                Operator::BitwiseXor => 5,
                Operator::BitwiseOr => 4,
                Operator::GreaterThan
                | Operator::LessThan
                | Operator::GreaterOrEqual
                | Operator::LessOrEqual
                | Operator::Equals
                | Operator::NotEquals => 3,
                Operator::And => 2,
                Operator::Or => 1,
                Operator::Assign(_) => 0,
            },
        }
    }
    fn op_type(&self) -> OpType {
        match self {
            YardOp::Standard(_) => OpType::Infix,
            YardOp::LogicalNot => OpType::Prefix,
            YardOp::Cast(..) | YardOp::Ternary(..) => OpType::Postfix,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum OpType {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Debug, PartialEq, Clone)]
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
    expecting_op: bool,
}

impl<'a> ShuntingYard {
    pub fn new() -> ShuntingYard {
        Self::default()
    }
    pub fn insert_op(
        &mut self,
        op: YardOp,
        token: &Token<'a>,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<(), ParserError<'a>> {
        match op.op_type() {
            OpType::Prefix => {
                if self.expecting_op {
                    return Err(ParserError::UnexpectedToken(token.clone()));
                }
                self.expecting_op = false;
            }
            OpType::Infix => {
                if !self.expecting_op {
                    return Err(ParserError::UnexpectedToken(token.clone()));
                }
                self.expecting_op = false;
            }
            OpType::Postfix => {
                if !self.expecting_op {
                    return Err(ParserError::UnexpectedToken(token.clone()));
                }
                self.expecting_op = true;
            }
        }

        while let Some(PossiblyParenOpen::Token(op2)) = self.operator_stack.last() {
            let op2 = op2.clone();
            if op2.precedence() >= op.precedence() {
                // Unwrap will never panic as the `while let` loop above checks that the last item in
                // `operator_stack` exists.
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
    /// expression, consuming a quantity of expression already in the output.
    fn push_op_to_output(
        &mut self,
        operator: YardOp,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<(), ParserError<'a>> {
        let param_1 = self.pop_expression()?;
        // construct operator
        let expr = match operator {
            YardOp::Standard(op) => {
                let right = param_1;
                let left = self.pop_expression()?;
                let left_type = parser
                    .get_type_from_expression(None, &left, context)?
                    .into_owned();
                let right_type = parser
                    .get_type_from_expression(None, &right, context)?
                    .into_owned();
                match op {
                    Operator::And | Operator::Or => {
                        if left_type != Type::Bool {
                            return Err(ParserError::InvalidOperationNotSameType(
                                left_type,
                                Type::Bool,
                            ));
                        }
                        if right_type != Type::Bool {
                            return Err(ParserError::InvalidOperationNotSameType(
                                Type::Bool,
                                right_type,
                            ));
                        }
                    }
                    Operator::GreaterThan
                    | Operator::LessThan
                    | Operator::GreaterOrEqual
                    | Operator::LessOrEqual
                    | Operator::Minus
                    | Operator::Multiply
                    | Operator::Divide
                    | Operator::Rem
                    | Operator::BitwiseXor
                    | Operator::BitwiseAnd
                    | Operator::BitwiseOr
                    | Operator::BitwiseLeft
                    | Operator::BitwiseRight => {
                        if !left_type.is_number() || !right_type.is_number() {
                            return Err(ParserError::ExpectedNumberType);
                        }
                        if left_type != right_type {
                            return Err(ParserError::InvalidOperationNotSameType(
                                left_type, right_type,
                            ));
                        }
                    }
                    Operator::Plus => match (&left_type, &right_type) {
                        (Type::String, Type::String) => (),
                        (Type::String, b) if b.is_number() => (),
                        (a, Type::String) if a.is_number() => (),
                        (a, b) if a.is_number() && b.is_number() => (),
                        _ => return Err(ParserError::ExpectedNumberType),
                    },
                    Operator::Equals | Operator::NotEquals | Operator::Assign(_) => {
                        if left_type != right_type {
                            return Err(ParserError::InvalidOperationNotSameType(
                                left_type, right_type,
                            ));
                        }
                    }
                }

                Expression::Operator(op, Box::new(left), Box::new(right))
            }
            YardOp::LogicalNot => {
                let expr_type = parser.get_type_from_expression(None, &param_1, context)?;
                if *expr_type != Type::Bool {
                    return Err(ParserError::InvalidValueType(
                        expr_type.into_owned(),
                        Type::Bool,
                    ));
                }
                Expression::IsNot(Box::new(param_1))
            }
            YardOp::Ternary(valid_expr, else_expr) => {
                if *parser.get_type_from_expression(None, &param_1, context)? != Type::Bool {
                    return Err(ParserError::InvalidCondition(Type::Bool, param_1.clone()));
                }

                Expression::Ternary(Box::new(param_1), Box::new(valid_expr), Box::new(else_expr))
            }
            YardOp::Cast(right_type) => {
                let left_type = parser
                    .get_type_from_expression(None, &param_1, context)?
                    .into_owned();

                if !right_type.is_primitive() {
                    return Err(ParserError::CastPrimitiveError(left_type, right_type));
                }
                if !left_type.is_castable_to(&right_type) {
                    return Err(ParserError::CastError(left_type, right_type));
                }
                Expression::Cast(Box::new(param_1), right_type)
            }
        };
        self.output_stack.push(expr);
        Ok(())
    }

    /// Pops the top value on the output stack if it is an expression
    pub fn pop_expression(&mut self) -> Result<Expression, ParserError<'a>> {
        self.output_stack.pop().ok_or(ParserError::EmptyOutputStack)
    }
    /// Returns a reference to the the last expression if it exists
    pub fn peek_expression(&self) -> Option<&Expression> {
        self.output_stack.last()
    }
    /// Returns the state of whether the yard is expecting an operator to be inserted next
    pub fn expecting_op(&self) -> bool {
        self.expecting_op
    }
    /// Toggles the state of whether the yard is expecting an operator to be inserted next.
    /// Used when parsing complicated operator-like things such as `.` paths and array subscripts.
    /// Those things don't fit into the shunting yard model very well, so are parsed elsewhere
    /// then inserted into the yard.
    pub fn toggle_expect(&mut self) {
        self.expecting_op = !self.expecting_op;
    }

    pub fn insert_expr(&mut self, expr: Expression) {
        if self.expecting_op {
            unreachable!("Expected operator, found expression")
        }
        self.expecting_op = true;
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
        if self.parenthesis_depth == 0 {
            return Err(ParserError::UnmatchedParenthesis);
        }
        self.parenthesis_depth -= 1;
        loop {
            let Some(top) = self.operator_stack.pop() else {
                return Err(ParserError::UnmatchedParenthesis);
            };
            match top {
                PossiblyParenOpen::ParenOpen => break,
                PossiblyParenOpen::Token(t) => self.push_op_to_output(t, parser, context)?,
            }
        }

        Ok(())
    }

    pub fn get_paren_depth(&self) -> u64 {
        self.parenthesis_depth
    }

    pub fn finish(
        mut self,
        parser: &Parser<'a>,
        context: &Context,
    ) -> Result<Expression, ParserError<'a>> {
        let mut operators: Vec<YardOp> = Vec::new();
        for op in self.operator_stack.drain(..) {
            match op {
                PossiblyParenOpen::Token(op) => {
                    operators.push(op);
                }
                PossiblyParenOpen::ParenOpen => {
                    return Err(ParserError::UnmatchedParenthesis);
                }
            }
        }
        operators.reverse();
        for op in operators {
            self.push_op_to_output(op, parser, context)?;
        }
        // TODO: make real error
        assert!(self.output_stack.len() <= 1);
        if self.output_stack.is_empty() {
            return Err(ParserError::EmptyValue);
        }
        self.pop_expression()
    }
}

#[cfg(test)]
mod tests {
    use std::{borrow::Cow, collections::VecDeque};

    use crate::{
        ast::{Expression, Operator, Signature, Token},
        parser::{context::Context, IdMapper},
        EnvironmentBuilder, Parser, Type, Value,
    };

    #[track_caller]
    fn parse_tokens(tokens: &[Token]) -> Expression {
        parse_tokens_full(tokens, None, None)
    }
    #[track_caller]
    fn parse_tokens_full(
        tokens: &[Token],
        expected: Option<Type>,
        variables: Option<Vec<(&str, Type)>>,
    ) -> Expression {
        let env = EnvironmentBuilder::default();
        let tokens: VecDeque<Token> = tokens.iter().cloned().collect();
        let mut p = Parser::new(tokens, &env);
        let mut mapper = IdMapper::new();
        let mut context = Context::new();
        context.begin_scope();
        if let Some(variables) = variables {
            for (name, t) in variables {
                let id = mapper.register(Cow::Borrowed(name)).unwrap();
                context.register_variable(id, t).unwrap();
            }
        }
        p.read_expr(expected.as_ref(), &mut context, &mut mapper)
            .unwrap()
    }

    #[test]
    fn test_empty_array() {
        // []
        let tokens = [Token::BracketOpen, Token::BracketClose];
        let expr = parse_tokens(&tokens);
        assert_eq!(expr, Expression::ArrayConstructor(Vec::new()))
    }

    #[test]
    fn test_array_constructor() {
        // [1,7]
        let tokens = [
            Token::BracketOpen,
            Token::U64Value(1),
            Token::Comma,
            Token::U64Value(7),
            Token::Comma,
            Token::BracketClose,
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::ArrayConstructor(vec![
                Expression::Value(Value::U64(1)),
                Expression::Value(Value::U64(7))
            ])
        )
    }

    #[test]
    // Construct and index an array in the same expression
    fn test_array_construct_index() {
        // [1,7][1]
        let tokens = [
            Token::BracketOpen,
            Token::U64Value(1),
            Token::Comma,
            Token::U64Value(7),
            Token::Comma,
            Token::BracketClose,
            Token::BracketOpen,
            Token::U64Value(1),
            Token::BracketClose,
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::ArrayCall(
                Box::new(Expression::ArrayConstructor(vec![
                    Expression::Value(Value::U64(1)),
                    Expression::Value(Value::U64(7))
                ])),
                Box::new(Expression::Value(Value::U64(1)))
            )
        )
    }

    #[test]
    fn test_logical_not() {
        // !(0 == 2)
        let tokens = [
            Token::IsNot,
            Token::ParenthesisOpen,
            Token::U64Value(0),
            Token::OperatorEquals,
            Token::U64Value(2),
            Token::ParenthesisClose,
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::IsNot(Box::new(Expression::Operator(
                Operator::Equals,
                Box::new(Expression::Value(Value::U64(0))),
                Box::new(Expression::Value(Value::U64(2)))
            )))
        );
    }

    #[test]
    fn test_precedence() {
        // 3 + 7 * 13
        let tokens = [
            Token::U64Value(3),
            Token::OperatorPlus,
            Token::U64Value(7),
            Token::OperatorMultiply,
            Token::U64Value(13),
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::Operator(
                Operator::Plus,
                Box::new(Expression::Value(Value::U64(3))),
                Box::new(Expression::Operator(
                    Operator::Multiply,
                    Box::new(Expression::Value(Value::U64(7))),
                    Box::new(Expression::Value(Value::U64(13))),
                ))
            )
        )
    }

    #[test]
    fn test_precedence_parens() {
        // (8 + 3 * 7) / 2
        let tokens = [
            Token::ParenthesisOpen,
            Token::U64Value(8),
            Token::OperatorPlus,
            Token::U64Value(3),
            Token::OperatorMultiply,
            Token::U64Value(7),
            Token::ParenthesisClose,
            Token::OperatorDivide,
            Token::U64Value(2),
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::Operator(
                Operator::Divide,
                Box::new(Expression::Operator(
                    Operator::Plus,
                    Box::new(Expression::Value(Value::U64(8))),
                    Box::new(Expression::Operator(
                        Operator::Multiply,
                        Box::new(Expression::Value(Value::U64(3))),
                        Box::new(Expression::Value(Value::U64(7)))
                    ))
                )),
                Box::new(Expression::Value(Value::U64(2)))
            )
        );
    }

    #[test]
    fn test_ternary() {
        // true ? 7 : 13
        let tokens = [
            Token::True,
            Token::OperatorTernary,
            Token::U64Value(7),
            Token::Colon,
            Token::U64Value(13),
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::Ternary(
                Box::new(Expression::Value(Value::Boolean(true))),
                Box::new(Expression::Value(Value::U64(7))),
                Box::new(Expression::Value(Value::U64(13)))
            )
        )
    }

    #[test]
    fn test_ternary_precedence() {
        // 1 > 0 ? 7 : 13 + 2
        let tokens = [
            Token::U64Value(1),
            Token::OperatorGreaterThan,
            Token::U64Value(0),
            Token::OperatorTernary,
            Token::U64Value(7),
            Token::Colon,
            Token::U64Value(13),
            Token::OperatorPlus,
            Token::U64Value(2),
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::Ternary(
                Box::new(Expression::Operator(
                    Operator::GreaterThan,
                    Box::new(Expression::Value(Value::U64(1))),
                    Box::new(Expression::Value(Value::U64(0)))
                )),
                Box::new(Expression::Value(Value::U64(7))),
                Box::new(Expression::Operator(
                    Operator::Plus,
                    Box::new(Expression::Value(Value::U64(13))),
                    Box::new(Expression::Value(Value::U64(2)))
                ))
            )
        )
    }

    #[test]
    fn test_ternary_subexpression() {
        // (true || false) ? (7*7) : (13*13)
        let tokens = [
            Token::ParenthesisOpen,
            Token::True,
            Token::OperatorOr,
            Token::False,
            Token::ParenthesisClose,
            Token::OperatorTernary,
            Token::ParenthesisOpen,
            Token::U64Value(7),
            Token::OperatorMultiply,
            Token::U64Value(7),
            Token::ParenthesisClose,
            Token::Colon,
            Token::ParenthesisOpen,
            Token::U64Value(13),
            Token::OperatorMultiply,
            Token::U64Value(13),
            Token::ParenthesisClose,
        ];
        let expr = parse_tokens(&tokens);
        assert_eq!(
            expr,
            Expression::Ternary(
                Box::new(Expression::Operator(
                    Operator::Or,
                    Box::new(Expression::Value(Value::Boolean(true))),
                    Box::new(Expression::Value(Value::Boolean(false)))
                )),
                Box::new(Expression::Operator(
                    Operator::Multiply,
                    Box::new(Expression::Value(Value::U64(7))),
                    Box::new(Expression::Value(Value::U64(7)))
                )),
                Box::new(Expression::Operator(
                    Operator::Multiply,
                    Box::new(Expression::Value(Value::U64(13))),
                    Box::new(Expression::Value(Value::U64(13)))
                ))
            )
        )
    }

    #[test]
    fn test_method_call() {
        // "hello".len()
        const STRING_METHOD_NAME: &str = "len";
        let tokens = [
            Token::StringValue("Hello".into()),
            Token::Dot,
            Token::Identifier(STRING_METHOD_NAME),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
        ];
        let expr = parse_tokens(&tokens);
        // Resolve ID for `len` manually
        let env = EnvironmentBuilder::default();
        let sig = Signature::new(
            STRING_METHOD_NAME.to_string(),
            Some(Type::String),
            Vec::new(),
        );
        let id = env.get_functions_mapper().get(&sig).unwrap();
        assert_eq!(
            expr,
            Expression::FunctionCall(
                Some(Box::new(Expression::Value(Value::String(
                    "Hello".to_string()
                )))),
                id,
                Vec::new()
            )
        )
    }
    #[test]
    fn test_cast_u64() {
        // 128 as u8 + 0
        let tokens = [
            Token::Identifier("a"),
            Token::As,
            Token::U8,
            Token::OperatorPlus,
            Token::U64Value(0),
        ];
        let expr = parse_tokens_full(&tokens, Some(Type::U8), Some(vec![("a", Type::U64)]));
        assert_eq!(
            expr,
            Expression::Operator(
                Operator::Plus,
                Box::new(Expression::Cast(
                    Box::new(Expression::Variable(0)),
                    Type::U8
                )),
                Box::new(Expression::Value(Value::U8(0)))
            )
        )
    }

    #[test]
    #[should_panic]
    fn test_empty_expression() {
        // ( )
        let tokens = [Token::ParenthesisOpen, Token::ParenthesisClose];
        let expr = parse_tokens(&tokens);
        dbg!(expr);
    }
}

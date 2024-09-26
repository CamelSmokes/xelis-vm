mod context;
mod error;
mod mapper;
mod program;
mod scope;
mod shunting_yard;
mod struct_manager;

use shunting_yard::{ShuntingYard, YardOp};
pub(crate) use struct_manager::{StructBuilder, StructManager};

pub use error::ParserError;
pub use program::Program;

use self::context::Context;

pub use mapper::{FunctionMapper, IdMapper, Mapper};

use crate::{
    ast::*,
    types::{HasKey, Type},
    values::Value,
    EnvironmentBuilder, Function, IdentifierType,
};
use std::{
    borrow::Cow,
    collections::{HashSet, VecDeque},
};

pub struct Parser<'a> {
    // Tokens to process
    tokens: VecDeque<Token<'a>>,
    // All constants declared
    constants: HashSet<DeclarationStatement>,
    // All functions registered by the program
    functions: Vec<FunctionType>,
    // Functions mapper
    // It will contains all the functions declared in the program
    // with the matching id <-> function signature
    functions_mapper: FunctionMapper<'a>,
    // Struct manager
    struct_manager: StructManager<'a>,
    // Environment contains all the library linked to the program
    environment: &'a EnvironmentBuilder<'a>,
    // TODO: Path to use to import files
    // _path: Option<&'a str>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token<'a>>, environment: &'a EnvironmentBuilder) -> Self {
        let functions_mapper = FunctionMapper::with_parent(environment.get_functions_mapper());

        Parser {
            tokens,
            constants: HashSet::new(),
            functions: Vec::new(),
            functions_mapper,
            struct_manager: StructManager::with_parent(environment.get_struct_manager()),
            environment,
        }
    }

    // Consume the next token
    #[inline(always)]
    fn advance(&mut self) -> Result<Token<'a>, ParserError<'a>> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Consume the next token without error
    #[inline(always)]
    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.pop_front()
    }

    // Peek the next token without consuming it
    #[inline(always)]
    fn peek(&self) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    #[inline(always)]
    fn next_identifier(&mut self) -> Result<&'a str, ParserError<'a>> {
        match self.advance()? {
            Token::Identifier(id) => Ok(id),
            token => Err(ParserError::ExpectedIdentifierToken(token)),
        }
    }

    // Check if the next token is a specific token
    #[inline(always)]
    fn peek_is(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t == token).is_some()
    }

    // Check if the next token is not a specific token
    #[inline(always)]
    fn peek_is_not(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t != token).is_some()
    }

    // Check if the next token is an identifier
    #[inline(always)]
    fn peek_is_identifier(&self) -> bool {
        self.peek()
            .ok()
            .filter(|t| match t {
                Token::Identifier(_) => true,
                _ => false,
            })
            .is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token<'a>) -> Result<(), ParserError<'a>> {
        let token = self.advance()?;
        if token != expected {
            return Err(ParserError::InvalidToken(token, expected));
        }
        Ok(())
    }

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - u8
     * - u16
     * - u64
     * - u128
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError<'a>> {
        let token = self.advance()?;
        let mut _type = match Type::from_token(&token, &self.struct_manager) {
            Some(v) => v,
            None => return Err(ParserError::TypeNotFound(token)),
        };

        // support multi dimensional arrays
        loop {
            let token = self.advance()?;
            if token != Token::BracketOpen {
                // Push back
                // This allow us to economize one read per iteration on array type
                // by simply pushing back the token that we don't need
                self.tokens.push_front(token);
                break;
            }

            self.expect_token(Token::BracketClose)?;
            _type = Type::Array(Box::new(_type));
        }

        Ok(_type)
    }

    // get the type of an expression
    fn get_type_from_expression<'b>(
        &'b self,
        on_type: Option<&Type>,
        expression: &'b Expression,
        context: &'b Context,
    ) -> Result<Cow<'b, Type>, ParserError<'a>> {
        match self.get_type_from_expression_internal(on_type, expression, context)? {
            Some(v) => Ok(v),
            None => Err(ParserError::EmptyValue),
        }
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression_internal<'b>(
        &'b self,
        on_type: Option<&Type>,
        expression: &'b Expression,
        context: &'b Context,
    ) -> Result<Option<Cow<'b, Type>>, ParserError<'a>> {
        let _type: Cow<'b, Type> = match expression {
            Expression::ArrayConstructor(ref values) => match values.first() {
                Some(v) => Cow::Owned(Type::Array(Box::new(
                    self.get_type_from_expression(on_type, v, context)?
                        .into_owned(),
                ))),
                None => return Err(ParserError::EmptyArrayConstructor), // cannot determine type from empty array
            },
            Expression::Variable(ref var_name) => match on_type {
                Some(t) => {
                    if let Type::Struct(struct_name) = t {
                        let structure = self.struct_manager.get(struct_name)?;
                        let index = *var_name as usize;
                        if let Some(value) = structure.fields.get(index) {
                            Cow::Borrowed(value)
                        } else {
                            return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()));
                        }
                    } else {
                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()));
                    }
                }
                None => {
                    if context.has_variable(var_name) {
                        Cow::Borrowed(context.get_type_of_variable(var_name)?)
                    } else {
                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()));
                    }
                }
            },
            Expression::FunctionCall(path, name, _) => {
                let f = self.get_function(*name)?;
                match f.return_type() {
                    Some(ref v) => match v {
                        Type::T => match on_type {
                            Some(t) => Cow::Owned(t.get_inner_type().clone()),
                            None => match path {
                                Some(p) => Cow::Owned(
                                    self.get_type_from_expression(on_type, p, context)?
                                        .get_inner_type()
                                        .clone(),
                                ),
                                None => return Err(ParserError::InvalidTypeT),
                            },
                        },
                        Type::Optional(inner) => match inner.as_ref() {
                            Type::T => match on_type {
                                Some(t) => {
                                    Cow::Owned(Type::Optional(Box::new(t.get_inner_type().clone())))
                                }
                                None => match path {
                                    Some(p) => Cow::Owned(Type::Optional(Box::new(
                                        self.get_type_from_expression(on_type, p, context)?
                                            .get_inner_type()
                                            .clone(),
                                    ))),
                                    None => return Err(ParserError::InvalidTypeT),
                                },
                            },
                            _ => Cow::Borrowed(v),
                        },
                        _ => Cow::Borrowed(v),
                    },
                    None => return Err(ParserError::FunctionNoReturnType),
                }
            }
            // we have to clone everything due to this
            Expression::Value(ref val) => match Type::from_value(val, &self.struct_manager) {
                Some(v) => Cow::Owned(v),
                None => return Ok(None),
            },
            Expression::ArrayCall(path, _) => {
                match self
                    .get_type_from_expression(on_type, path, context)?
                    .into_owned()
                {
                    Type::Array(_type) => Cow::Owned(*_type),
                    _ => return Err(ParserError::InvalidArrayCall),
                }
            }
            Expression::StructConstructor(name, _) => {
                if !self.struct_manager.has(name) {
                    return Err(ParserError::StructNotFound(name.clone()));
                }

                Cow::Owned(Type::Struct(name.clone()))
            }
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(on_type, left, context)?;
                self.get_type_from_expression(Some(&var_type), right, context)?
            }
            // Compatibility checks are done when constructing the expression
            Expression::Operator(op, left, right) => match op {
                // Condition operators
                Operator::Or
                | Operator::Equals
                | Operator::NotEquals
                | Operator::GreaterOrEqual
                | Operator::GreaterThan
                | Operator::LessOrEqual
                | Operator::LessThan
                | Operator::And => Cow::Owned(Type::Bool),
                // Assign operators
                Operator::Assign(_) => return Err(ParserError::AssignReturnNothing),
                // String compatible operators
                Operator::Plus | Operator::Minus => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if *left_type == Type::String || *right_type == Type::String {
                        Cow::Owned(Type::String)
                    } else {
                        left_type
                    }
                }
                // Number only operators
                Operator::Multiply
                | Operator::Divide
                | Operator::BitwiseXor
                | Operator::BitwiseAnd
                | Operator::BitwiseOr
                | Operator::BitwiseLeft
                | Operator::BitwiseRight
                | Operator::Rem => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if !left_type.is_number() || !right_type.is_number() || left_type != right_type
                    {
                        return Err(ParserError::InvalidOperationNotSameType(
                            left_type.into_owned(),
                            right_type.into_owned(),
                        ));
                    }
                    left_type
                }
            },
            Expression::IsNot(_) => Cow::Owned(Type::Bool),
            Expression::Ternary(_, expr, _) => {
                self.get_type_from_expression(on_type, expr, context)?
            }
            Expression::Cast(_, _type) => Cow::Borrowed(_type),
        };

        Ok(Some(_type))
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(
        &mut self,
        path: Option<Expression>,
        on_type: Option<&Type>,
        name: &str,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Type> = Vec::new();

        // read parameters for function call
        while self.peek_is_not(Token::ParenthesisClose) {
            let expr = self.read_expression(context, mapper)?;
            // We are forced to clone the type because we can't borrow it from the expression
            // I prefer to do this than doing an iteration below
            types.push(
                self.get_type_from_expression(None, &expr, context)?
                    .into_owned(),
            );
            parameters.push(expr);

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        let id = self.functions_mapper.get_compatible(Signature::new(
            name.to_owned(),
            on_type.cloned(),
            types,
        ))?;

        // Entry are only callable by external
        let f = self.get_function(id)?;
        if f.is_entry() {
            return Err(ParserError::FunctionNotFound);
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(path.map(Box::new), id, parameters))
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(
        &mut self,
        struct_name: IdentifierType,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        self.expect_token(Token::BraceOpen)?;
        let structure = self.struct_manager.get(&struct_name)?.clone();
        let mut fields = Vec::with_capacity(structure.fields.len());
        for t in structure.fields.iter() {
            let field_name = self.next_identifier()?;
            let field_value = match self.advance()? {
                Token::Comma => {
                    if let Ok(id) = mapper.get(field_name) {
                        Expression::Variable(id)
                    } else {
                        return Err(ParserError::UnexpectedVariable(field_name.to_owned()));
                    }
                }
                Token::Colon => {
                    let value = self.read_expr(Some(t), context, mapper)?;
                    if self.peek_is(Token::Comma) {
                        self.advance()?;
                    }
                    value
                }
                token => return Err(ParserError::UnexpectedToken(token)),
            };

            let field_type = self.get_type_from_expression(None, &field_value, context)?;
            if !t.is_compatible_with(&field_type) {
                return Err(ParserError::InvalidValueType(
                    field_type.into_owned(),
                    t.clone(),
                ));
            }

            fields.push(field_value);
        }

        self.expect_token(Token::BraceClose)?;
        Ok(Expression::StructConstructor(struct_name, fields))
    }

    fn read_array_constructor(
        &mut self,
        expected_type: Option<&Type>,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        // require at least one value in a array constructor
        let mut expressions: Vec<Expression> = Vec::new();
        let mut array_type: Option<Type> = None;
        while *self.peek()? != Token::BracketClose {
            let expr =
                self.read_expr(expected_type.map(|t| t.get_inner_type()), context, mapper)?;
            match &array_type {
                // array values must have the same type
                Some(t) => {
                    let _type = self.get_type_from_expression(None, &expr, context)?;
                    if *_type != *t {
                        return Err(ParserError::InvalidTypeInArray(
                            _type.into_owned(),
                            t.clone(),
                        ));
                    }
                }
                None => {
                    // first value determines the type of array
                    array_type = Some(
                        self.get_type_from_expression(None, &expr, context)?
                            .into_owned(),
                    );
                }
            };
            expressions.push(expr);

            if *self.peek()? == Token::Comma {
                self.expect_token(Token::Comma)?;
            }
        }

        self.expect_token(Token::BracketClose)?;
        Ok(Expression::ArrayConstructor(expressions))
    }

    fn read_array_index(
        &mut self,
        on_type: Option<&Type>,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        let index = self.read_expr(Some(&Type::U64), context, mapper)?;
        // Index must be of type u64
        let index_type = self.get_type_from_expression(on_type, &index, context)?;
        if *index_type != Type::U64 {
            return Err(ParserError::InvalidArrayCallIndexType(
                index_type.into_owned(),
            ));
        }
        // TODO validate index is u64
        self.expect_token(Token::BracketClose)?;
        Ok(index)
    }

    // Read an expression with default parameters
    fn read_expression(
        &mut self,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, context, mapper)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(
        &mut self,
        expected_type: Option<&Type>,
        context: &mut Context,
        mapper: &mut IdMapper,
    ) -> Result<Expression, ParserError<'a>> {
        println!("START");
        if self.peek_is(Token::ParenthesisClose) {
            return Err(ParserError::UnmatchedParenthesis);
        }
        let mut yard = ShuntingYard::new();
        while let Ok(token) = self.peek() {
            match token {
                Token::BraceClose | Token::BracketClose | Token::BraceOpen => break,
                Token::ParenthesisClose if yard.get_paren_depth() == 0 => break,
                _ => (),
            }

            // The series of tokens `a + b c + d` should be parsed as different expressions
            // `a + b` and `c + d`. The parser keeps track this by recording whether it is
            // expecting an operator.
            // By default, the yard does not expect an op.
            // If an infix operator is successfully inserted to the yard, then don't expect op.
            // If an expression is successfully inserted to the yard, then expect op.
            //
            // If a non operator is found when the parser expects one, the expression
            // is finished parsing.
            if yard.expecting_op() && !token.is_operator() {
                // In `(a+b) + c`: after parsing the `b` token, the parser expects the next
                // token to be an operator. However, the next token is a closing paren.
                // We prevent this closing paren from finishing the expression.
                if *token != Token::ParenthesisClose {
                    break;
                }
            }
            let token = self.advance()?;
            println!("{:?}", token);
            match token {
                Token::BracketOpen => {
                    if yard.expecting_op() {
                        // Array index
                        let array_index = self.read_array_index(None, context, mapper)?;
                        let array = yard.pop_expression()?;
                        if !self
                            .get_type_from_expression(None, &array, context)?
                            .is_array()
                        {
                            return Err(ParserError::InvalidArrayCall);
                        }
                        // Although it acts like an operator, the `[` token is not internally an operator.
                        // Manually toggle expecting op so that the following insert is valid.
                        yard.toggle_expect();
                        yard.insert_expr(Expression::ArrayCall(
                            Box::new(array),
                            Box::new(array_index),
                        ));
                    } else {
                        // Array constructor
                        let array_expr = self.read_array_constructor(None, context, mapper)?;
                        yard.insert_expr(array_expr);
                    };
                }
                Token::ParenthesisOpen => {
                    yard.open_paren();
                }
                Token::ParenthesisClose => {
                    yard.close_paren(self, context)?;
                }
                Token::Identifier(id) => {
                    if self.peek_is(Token::ParenthesisOpen) {
                        // function call
                        yard.insert_expr(self.read_function_call(
                            yard.peek_expression().cloned(),
                            None,
                            id,
                            context,
                            mapper,
                        )?);
                    } else {
                        // variable
                        if let Ok(id) = mapper.get(id) {
                            yard.insert_expr(Expression::Variable(id));
                        } else if let Ok(id) = self.struct_manager.get_mapping(id) {
                            yard.insert_expr(self.read_struct_constructor(id, context, mapper)?);
                        } else {
                            return Err(ParserError::UnexpectedVariable(id.to_owned()));
                        }
                    }
                }
                Token::U64Value(value) => yard.insert_expr(Expression::Value(match expected_type {
                    Some(t) => Value::U64(value).checked_cast_to_primitive_type(t)?,
                    None => Value::U64(value)
                })),
                Token::U128Value(value) => yard.insert_expr(Expression::Value(Value::U128(value))),
                Token::StringValue(value) => {
                    yard.insert_expr(Expression::Value(Value::String(value.into_owned())))
                }
                Token::True => yard.insert_expr(Expression::Value(Value::Boolean(true))),
                Token::False => yard.insert_expr(Expression::Value(Value::Boolean(false))),
                Token::Null => yard.insert_expr(Expression::Value(Value::Null)),
                Token::Dot => {
                    if !yard.expecting_op() {
                        return Err(ParserError::UnexpectedToken(token));
                    }
                    let Ok(value) = yard.pop_expression() else {
                        return Err(ParserError::UnexpectedToken(Token::Dot));
                    };
                    let _type = self
                        .get_type_from_expression(None, &value, context)?
                        .into_owned();
                    let ident = self.next_identifier()?;
                    if self.peek_is(Token::ParenthesisOpen) {
                        // Method call
                        yard.toggle_expect();
                        let call_expr = self.read_function_call(
                            Some(value),
                            Some(&_type),
                            ident,
                            context,
                            mapper,
                        )?;
                        yard.insert_expr(call_expr);
                    } else {
                        // Struct member path
                        let Type::Struct(struct_name) = _type else {
                            return Err(ParserError::UnexpectedType(_type.clone()));
                        };
                        let builder = self.struct_manager.get(&struct_name)?;
                        yard.toggle_expect();
                        match builder.mapper.get(ident).ok() {
                            Some(v) => yard.insert_expr(Expression::Path(
                                Box::new(value),
                                Box::new(Expression::Variable(v)),
                            )),
                            None => return Err(ParserError::UnexpectedVariable(ident.to_owned())),
                        }
                    }
                }
                Token::IsNot => yard.insert_op(YardOp::LogicalNot, &token, self, context)?,
                Token::OperatorTernary => {
                    let valid_expr = self.read_expr(expected_type, context, mapper)?;
                    let first_type = self
                        .get_type_from_expression(None, &valid_expr, context)?
                        .into_owned();
                    self.expect_token(Token::Colon)?;
                    let else_expr = self.read_expr(expected_type, context, mapper)?;
                    let else_type = self.get_type_from_expression(None, &else_expr, context)?;

                    if first_type != *else_type {
                        // both expr should have the SAME type.
                        return Err(ParserError::InvalidValueType(
                            else_type.into_owned(),
                            first_type,
                        ));
                    }
                    yard.insert_op(
                        YardOp::Ternary(valid_expr, else_expr),
                        &token,
                        self,
                        context,
                    )?;
                }
                Token::As => {
                    yard.insert_op(YardOp::Cast(self.read_type()?), &token, self, context)?;
                }
                token => {
                    let op = match Operator::value_of(&token) {
                        Some(op) => op,
                        // Unknown token at end of expression (let, return, etc.): stop parsing expression.
                        None => break,
                    };
                    yard.insert_op(YardOp::Standard(op), &token, self, context)?;
                }
            };
        }
        yard.finish(self, context)
    }

    /**
     * {
     *     ...
     * }
     */
    fn read_body(
        &mut self,
        context: &mut Context,
        return_type: &Option<Type>,
        mapper: &mut IdMapper<'a>,
    ) -> Result<Vec<Statement>, ParserError<'a>> {
        context.begin_scope();
        let statements = self.read_statements(context, return_type, mapper)?;
        context.end_scope();
        Ok(statements)
    }

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must be alphanumeric characters
     * - Must provide a value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(
        &mut self,
        context: &mut Context,
        mapper: &mut IdMapper<'a>,
        is_const: bool,
    ) -> Result<DeclarationStatement, ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;

        // Variable name must be unique
        // Shadowing is not allowed atm
        if mapper.has_variable(name) {
            return Err(ParserError::VariableNameAlreadyUsed(name.to_owned()));
        }

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(ParserError::ConstantNameNotUppercase(name.to_owned()));
        }

        // Variable name must start with a alphabetic character
        if !name.starts_with(char::is_alphabetic) {
            return Err(ParserError::VariableMustStartWithAlphabetic(
                name.to_owned(),
            ));
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if self.peek_is(Token::OperatorAssign) {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(Some(&value_type), context, mapper)?;

            let expr_type = match self.get_type_from_expression_internal(None, &expr, context) {
                Ok(opt_type) => match opt_type {
                    Some(v) => v,
                    None => {
                        if value_type.is_optional() {
                            Cow::Owned(value_type.clone())
                        } else {
                            return Err(ParserError::NoValueType);
                        }
                    }
                },
                Err(e) => match e {
                    // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => {
                        Cow::Owned(value_type.clone())
                    }
                    _ => return Err(e),
                },
            };

            if !expr_type.is_compatible_with(&value_type) {
                return Err(ParserError::InvalidValueType(
                    expr_type.into_owned(),
                    value_type,
                ));
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        let id = mapper.register(Cow::Borrowed(name))?;
        context.register_variable(id, value_type.clone())?;

        Ok(DeclarationStatement {
            id,
            value_type,
            value,
        })
    }

    fn read_loop_body(
        &mut self,
        context: &mut Context,
        return_type: &Option<Type>,
        mapper: &mut IdMapper<'a>,
    ) -> Result<Vec<Statement>, ParserError<'a>> {
        let old_value = context.is_in_a_loop(); // support loop in loop
        context.set_in_a_loop(true);
        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, return_type, mapper)?;
        context.set_in_a_loop(old_value);

        Ok(statements)
    }

    // Read a single statement
    fn read_statement(
        &mut self,
        context: &mut Context,
        return_type: &Option<Type>,
        mapper: &mut IdMapper<'a>,
    ) -> Result<Option<Statement>, ParserError<'a>> {
        if let Some(token) = self.next() {
            let statement: Statement = match token {
                Token::BraceClose => return Ok(None),
                Token::For => {
                    // Example: for i: u64 = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context, mapper, false)?;
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type =
                        self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(
                            condition_type.into_owned(),
                            condition,
                        ));
                    }

                    let increment = self.read_expression(context, mapper)?;
                    match &increment {
                        // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {}
                        _ => return Err(ParserError::InvalidForExpression(increment)),
                    };

                    let statements = self.read_loop_body(context, return_type, mapper)?;
                    context.end_scope();

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => {
                    // Example: foreach a in array {}
                    context.begin_scope();
                    let variable = self.next_identifier()?;
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context, mapper)?;
                    let expr_type = self.get_type_from_expression(None, &expr, context)?;

                    // verify that we can iter on it
                    if !expr_type.is_array() {
                        return Err(ParserError::ExpectedArrayType);
                    }

                    let id = mapper.register(Cow::Borrowed(variable))?;
                    context.register_variable(id, expr_type.get_inner_type().clone())?;
                    let statements = self.read_loop_body(context, return_type, mapper)?;
                    context.end_scope();

                    Statement::ForEach(id, expr, statements)
                }
                Token::While => {
                    // Example: while i < 10 {}
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type =
                        self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(
                            condition_type.into_owned(),
                            condition,
                        ));
                    }

                    let statements = self.read_loop_body(context, return_type, mapper)?;

                    Statement::While(condition, statements)
                }
                Token::If => {
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type =
                        self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(
                            condition_type.into_owned(),
                            condition,
                        ));
                    }

                    self.expect_token(Token::BraceOpen)?;
                    let body = self.read_body(context, return_type, mapper)?;
                    let else_statement = if self.peek_is(Token::Else) {
                        self.advance()?;
                        Some(if self.peek_is(Token::If) {
                            let statement = self.read_statement(context, return_type, mapper)?;
                            vec![statement.ok_or(ParserError::UnexpectedToken(Token::If))?]
                        } else {
                            self.expect_token(Token::BraceOpen)?;
                            self.read_body(context, return_type, mapper)?
                        })
                    } else {
                        None
                    };

                    Statement::If(condition, body, else_statement)
                }
                Token::BraceOpen => {
                    Statement::Scope(self.read_body(context, return_type, mapper)?)
                }
                Token::Let => Statement::Variable(self.read_variable(context, mapper, false)?),
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(Some(return_type), context, mapper)?;
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;
                        if !expr_type.is_compatible_with(return_type) {
                            return Err(ParserError::InvalidValueType(
                                expr_type.into_owned(),
                                return_type.clone(),
                            ));
                        }
                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Continue));
                    }

                    // we can't have anything after a continue
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                }
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Break));
                    }

                    // we can't have anything after a break
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                }
                token => {
                    self.tokens.push_front(token);
                    Statement::Expression(self.read_expression(context, mapper)?)
                }
            };
            Ok(Some(statement))
        } else {
            Ok(None)
        }
    }
    // Read all statements in a block
    // return type is used to verify that the last statement is a return with a valid value type
    // consume_brace is used to know if we should consume the open brace
    fn read_statements(
        &mut self,
        context: &mut Context,
        return_type: &Option<Type>,
        mapper: &mut IdMapper<'a>,
    ) -> Result<Vec<Statement>, ParserError<'a>> {
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(statement) = self.read_statement(context, return_type, mapper)? {
            statements.push(statement);
        }

        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(
        &mut self,
        mapper: &mut IdMapper<'a>,
    ) -> Result<Vec<Parameter>, ParserError<'a>> {
        let mut parameters: Vec<Parameter> = Vec::new();
        while self.peek_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            parameters.push(Parameter::new(
                mapper.register(Cow::Borrowed(name))?,
                value_type,
            ));

            if self.peek_is_not(Token::Comma) {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    // Verify that the last statement is a return
    // We don't check the last statement directly has it would allow
    // to have dead code after a return
    fn ends_with_return(statements: &Vec<Statement>) -> Result<bool, ParserError<'a>> {
        let mut ok = false;
        if let Some(statement) = statements.last() {
            match statement {
                Statement::If(_, statements, else_statements) => {
                    // if its the last statement
                    ok = Self::ends_with_return(statements)?;
                    // if it ends with a return, else must also end with a return
                    if let Some(statements) = else_statements.as_ref().filter(|_| ok) {
                        ok = Self::ends_with_return(statements)?;
                    } else {
                        ok = false;
                    }
                }
                Statement::Return(Some(_)) => {
                    ok = true;
                }
                _ => {}
            }
        }

        Ok(ok)
    }

    /**
     * Examples:
     * - entry foo() { ... }
     * - func foo() { ... }
     * - func foo(): u64 { ... }
     * - func foo(a: u64, b: u64) { ... }
     * - func (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a u64 value
     */
    fn read_function(
        &mut self,
        entry: bool,
        context: &mut Context,
        constants_mapper: &IdMapper<'a>,
    ) -> Result<(), ParserError<'a>> {
        context.begin_scope();

        // we need to clone the constants mapper to use it as our own local mapper
        let mut mapper = constants_mapper.clone();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;

            // verify that the type is a struct
            if let Type::Struct(struct_name) = &for_type {
                // only types that are declared by the same program
                if !self.struct_manager.has(struct_name) {
                    return Err(ParserError::StructNotFound(struct_name.clone()));
                }
            } else {
                return Err(ParserError::InvalidFunctionType(for_type));
            }

            let id = mapper.register(Cow::Borrowed(instance_name))?;
            context.register_variable(id, for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(id), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(ParserError::ExpectedIdentifierToken(token));
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters(&mut mapper)?;
        self.expect_token(Token::ParenthesisClose)?;

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(ParserError::EntryFunctionCannotHaveForType);
            }

            Some(Type::U64)
        } else if self.peek_is(Token::Colon) {
            // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let types: Vec<Type> = parameters.iter().map(|p| p.get_type().clone()).collect();
        let id = self.functions_mapper.register(Signature::new(
            name.to_owned(),
            for_type.clone(),
            types,
        ))?;
        if self.has_function(id) {
            return Err(ParserError::FunctionSignatureAlreadyExist);
        }

        let has_return_type = return_type.is_some();

        for param in parameters.iter() {
            context.register_variable(param.get_name().clone(), param.get_type().clone())?;
        }

        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, &return_type, &mut mapper)?;

        context.end_scope();

        // verify that the function ends with a return
        if has_return_type && !Self::ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound);
        }

        let function = match entry {
            true => FunctionType::Entry(EntryFunction::new(
                parameters,
                statements,
                mapper.count() as u16,
            )),
            false => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                parameters,
                statements,
                return_type,
                mapper.count() as u16,
            )),
        };

        // push function before reading statements to allow recursive calls
        self.functions.push(function);

        Ok(())
    }

    // Read a type with the following syntax:
    // import "filename.xel";
    // or with an alias:
    // import "filename.xel" as alias;
    fn read_import(&mut self) -> Result<(), ParserError<'a>> {
        let path = self.advance()?;

        let Token::StringValue(path) = path else {
            return Err(ParserError::InvalidImport);
        };

        // We don't allow absolute path or path that contains ".."
        if path.starts_with("/") || path.contains("..") {
            return Err(ParserError::InvalidImportPath(path.into_owned()));
        }

        // If its a local import, we will import its content directly
        let is_local = path.ends_with(".xel");
        if !is_local {
            return Err(ParserError::NotImplemented);
        }

        // let content = fs::read_to_string(path.as_ref()).map_err(|e| ParserError::InvalidImportPath(e.to_string()))?;
        // let mut tokens = Lexer::new(&content).get().map_err(|e| ParserError::ImportLexerError(path.into_owned(), e))?;

        // if *self.peek()? == Token::As {
        //     self.expect_token(Token::As)?;
        //     todo!()
        // } else {
        //     // Append all the tokens parsed at the beginning
        //     std::mem::swap(&mut self.tokens, &mut tokens);
        //     self.tokens.append(&mut tokens);
        // }

        Ok(())
    }

    // check if a function with the same signature exists
    fn has_function(&self, id: u16) -> bool {
        self.get_function(id).is_ok()
    }

    // get a function using its identifier
    fn get_function<'b>(&'b self, id: u16) -> Result<Function<'b>, ParserError<'a>> {
        // the id is the index of the function in the functions array
        let index = id as usize;
        let len = self.environment.get_functions().len();
        if index < len {
            Ok(Function::Native(&self.environment.get_functions()[index]))
        } else {
            match self.functions.get(index - len) {
                Some(func) => Ok(func.as_function()),
                None => Err(ParserError::FunctionNotFound),
            }
        }
    }

    /**
     * Example: Message { message_id: u64, message: string }
     * Rules:
     * - Structure name should start with a uppercase character
     * - only alphanumeric chars in name
     */
    fn read_struct(&mut self) -> Result<(&'a str, StructBuilder<'a>), ParserError<'a>> {
        let name = self.next_identifier()?;
        let mut chars = name.chars();
        if !chars.all(|c| c.is_ascii_alphanumeric()) {
            return Err(ParserError::InvalidStructureName(name.to_owned()));
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(ParserError::InvalidStructureName(name.to_owned()));
                }
            }
            None => return Err(ParserError::EmptyStructName),
        };

        let mut mapper: Mapper<'a, Cow<str>> = IdMapper::new();
        self.expect_token(Token::BraceOpen)?;
        let params = self.read_parameters(&mut mapper)?;
        let mut fields: Vec<Type> = Vec::with_capacity(params.len());
        for (i, param) in params.into_iter().enumerate() {
            let (id, value_type) = param.consume();
            if i != id as usize {
                return Err(ParserError::InvalidStructFieldOrder);
            }

            fields.push(value_type);
        }

        self.expect_token(Token::BraceClose)?;

        Ok((name, StructBuilder { fields, mapper }))
    }

    // Parse the tokens and return a Program
    // The function mapper is also returned for external calls
    pub fn parse(mut self) -> Result<(Program, FunctionMapper<'a>), ParserError<'a>> {
        let mut context: Context = Context::new();
        let mut constants_mapper: Mapper<'a, Cow<str>> = IdMapper::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => {
                    self.read_import()?;
                    continue;
                }
                Token::Const => {
                    let var = self.read_variable(&mut context, &mut constants_mapper, true)?;
                    let id = var.id;
                    if !self.constants.insert(var) {
                        return Err(ParserError::VariableIdAlreadyUsed(id));
                    }
                }
                Token::Function => self.read_function(false, &mut context, &constants_mapper)?,
                Token::Entry => self.read_function(true, &mut context, &constants_mapper)?,
                Token::Struct => {
                    let (name, builder) = self.read_struct()?;
                    self.struct_manager.add(Cow::Borrowed(name), builder)?;
                }
                token => return Err(ParserError::UnexpectedToken(token)),
            };
        }

        let program = Program::with(
            self.constants,
            self.struct_manager.finalize(),
            self.functions,
        );
        Ok((program, self.functions_mapper))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn test_parser(tokens: Vec<Token>) -> Program {
        let env = EnvironmentBuilder::default();
        test_parser_with_env(tokens, &env)
    }

    #[track_caller]
    fn test_parser_with_env(tokens: Vec<Token>, env: &EnvironmentBuilder) -> Program {
        let parser = Parser::new(VecDeque::from(tokens), env);
        let (program, _) = parser.parse().unwrap();
        program
    }

    #[track_caller]
    fn test_parser_statement(tokens: Vec<Token>, variables: Vec<(&str, Type)>) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &None, env)
    }

    #[track_caller]
    fn test_parser_statement_with(
        tokens: Vec<Token>,
        variables: Vec<(&str, Type)>,
        return_type: &Option<Type>,
        env: EnvironmentBuilder,
    ) -> Vec<Statement> {
        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut mapper = IdMapper::new();
        let mut context = Context::new();
        context.begin_scope();
        for (name, t) in variables {
            let id = mapper.register(Cow::Borrowed(name)).unwrap();
            context.register_variable(id, t).unwrap();
        }

        parser
            .read_statements(&mut context, return_type, &mut mapper)
            .unwrap()
    }

    fn test_parser_statement_with_return_type(
        tokens: Vec<Token>,
        variables: Vec<(&str, Type)>,
        return_type: Type,
    ) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &Some(return_type), env)
    }

    #[test]
    fn test_function() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose,
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_call_in_expr() {
        /*
        function foo(): bool {
            let array: u64[] = [1, 2, 3];
            return 0 > array.len()
        }
        */
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::Colon,
            Token::Bool,
            Token::BraceOpen,
            Token::Let,
            Token::Identifier("array"),
            Token::Colon,
            Token::U64,
            Token::BracketOpen,
            Token::BracketClose,
            Token::OperatorAssign,
            Token::BracketOpen,
            Token::U64Value(1),
            Token::Comma,
            Token::U64Value(2),
            Token::Comma,
            Token::U64Value(3),
            Token::BracketClose,
            Token::Return,
            Token::U64Value(0),
            Token::OperatorGreaterThan,
            Token::Identifier("array"),
            Token::Dot,
            Token::Identifier("len"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceClose,
        ];

        let program = test_parser(tokens);
        let statements = vec![
            Statement::Variable(DeclarationStatement {
                id: 0,
                value_type: Type::Array(Box::new(Type::U64)),
                value: Expression::ArrayConstructor(vec![
                    Expression::Value(Value::U64(1)),
                    Expression::Value(Value::U64(2)),
                    Expression::Value(Value::U64(3)),
                ]),
            }),
            Statement::Return(Some(Expression::Operator(
                Operator::GreaterThan,
                Box::new(Expression::Value(Value::U32(0))),
                Box::new(Expression::FunctionCall(
                    Some(Box::new(Expression::Variable(0))),
                    0,
                    Vec::new(),
                )),
            ))),
        ];

        assert_eq!(
            *program.functions().get(0).unwrap(),
            FunctionType::Declared(DeclaredFunction::new(
                None,
                None,
                Vec::new(),
                statements,
                Some(Type::Bool),
                1
            ))
        );
    }

    #[test]
    fn test_entry_function() {
        let tokens = vec![
            Token::Entry,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::U64Value(0),
            Token::BraceClose,
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_on_type() {
        // func (f Foo) bar() {}
        let tokens = vec![
            Token::Function,
            Token::ParenthesisOpen,
            Token::Identifier("f"),
            Token::Identifier("Foo"),
            Token::ParenthesisClose,
            Token::Identifier("bar"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_structure("Foo", Vec::new());
        let program = test_parser_with_env(tokens, &env);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_parameters() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::U64,
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::U64,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose,
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_return_type() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::Colon,
            Token::U64,
            Token::BraceOpen,
            Token::Return,
            Token::U64Value(0),
            Token::BraceClose,
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_foreach() {
        // foreach a in array {}
        let tokens = vec![
            Token::ForEach,
            Token::Identifier("a"),
            Token::In,
            Token::Identifier("array"),
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements =
            test_parser_statement(tokens, vec![("array", Type::Array(Box::new(Type::U64)))]);

        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_for() {
        // for i: u64 = 0; i < 10; i += 1 {}
        let tokens = vec![
            Token::For,
            Token::Identifier("i"),
            Token::Colon,
            Token::U64,
            Token::OperatorAssign,
            Token::U64Value(0),
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::Identifier("i"),
            Token::OperatorPlusAssign,
            Token::U64Value(1),
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_while() {
        // while i < 10 {}
        let tokens = vec![
            Token::While,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if() {
        // if i < 10 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else() {
        // if i < 10 {} else {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else_if() {
        // if i < 10 {} else if i < 20 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(20),
            Token::BraceOpen,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_return() {
        // if i < 10 { nothing } return 0
        let mut tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
        ];

        let statements = test_parser_statement(tokens.clone(), vec![("i", Type::U64)]);
        assert_eq!(
            statements,
            vec![
                Statement::If(
                    Expression::Operator(
                        Operator::LessThan,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Value(Value::U64(10)))
                    ),
                    Vec::new(),
                    None
                ),
                Statement::Return(None)
            ]
        );

        tokens.push(Token::U64Value(0));

        let statements =
            test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(
            statements,
            vec![
                Statement::If(
                    Expression::Operator(
                        Operator::LessThan,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Value(Value::U64(10)))
                    ),
                    Vec::new(),
                    None
                ),
                Statement::Return(Some(Expression::Value(Value::U64(0))))
            ]
        );
    }

    #[test]
    fn test_if_else_if_else_return() {
        // if i < 10 {} else if i < 20 {} else {} return 0
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(20),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
            Token::U64Value(0),
        ];

        let statements =
            test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(
            statements,
            vec![
                Statement::If(
                    Expression::Operator(
                        Operator::LessThan,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Value(Value::U64(10)))
                    ),
                    Vec::new(),
                    Some(vec![Statement::If(
                        Expression::Operator(
                            Operator::LessThan,
                            Box::new(Expression::Variable(0)),
                            Box::new(Expression::Value(Value::U64(20)))
                        ),
                        Vec::new(),
                        Some(Vec::new())
                    )])
                ),
                Statement::Return(Some(Expression::Value(Value::U64(0))))
            ]
        );
    }

    #[test]
    fn test_ends_with_return() {
        const RETURN: Statement = Statement::Return(Some(Expression::Value(Value::U64(0))));
        let statements = vec![RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![
            RETURN,
            Statement::Expression(Expression::Value(Value::U64(0))),
        ];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if ... return
        let statements = vec![
            Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), None),
            RETURN,
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![Statement::If(
            Expression::Value(Value::Boolean(true)),
            Vec::new(),
            None,
        )];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if else
        let statements = vec![Statement::If(
            Expression::Value(Value::Boolean(true)),
            Vec::new(),
            Some(Vec::new()),
        )];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else return
        let statements = vec![Statement::If(
            Expression::Value(Value::Boolean(true)),
            vec![RETURN],
            Some(vec![RETURN]),
        )];
        assert!(Parser::ends_with_return(&statements).unwrap());

        // if return else if return else no return
        let statements = vec![Statement::If(
            Expression::Value(Value::Boolean(true)),
            vec![RETURN],
            Some(vec![Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                None,
            )]),
        )];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else if return else return
        let statements = vec![Statement::If(
            Expression::Value(Value::Boolean(true)),
            vec![RETURN],
            Some(vec![Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                Some(vec![RETURN]),
            )]),
        )];
        assert!(Parser::ends_with_return(&statements).unwrap());
    }

    #[test]
    fn test_variable() {
        // let hello: string = "hello";
        let tokens = vec![
            Token::Let,
            Token::Identifier("hello"),
            Token::Colon,
            Token::String,
            Token::OperatorAssign,
            Token::StringValue(Cow::Borrowed("world")),
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_ternary() {
        // i < 10 ? 1 : 0
        let tokens = vec![
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::U64Value(10),
            Token::OperatorTernary,
            Token::U64Value(1),
            Token::Colon,
            Token::U64Value(0),
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_struct() {
        // Message { message_id: u64, message: string }
        let tokens = vec![
            Token::Struct,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("aaa_message_id"),
            Token::Colon,
            Token::U64,
            Token::Comma,
            Token::Identifier("aaa_message"),
            Token::Colon,
            Token::String,
            Token::BraceClose,
        ];

        let program = test_parser(tokens.clone());
        assert_eq!(program.structures().len(), 1);

        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_structure(
            "Message",
            vec![("message_id", Type::U64), ("message", Type::String)],
        );

        // Create a struct instance
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("message_id"),
            Token::Colon,
            Token::U64Value(0),
            Token::Comma,
            Token::Identifier("message"),
            Token::Colon,
            Token::StringValue(Cow::Borrowed("hello")),
            Token::BraceClose,
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
    }
}

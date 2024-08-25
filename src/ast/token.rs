use std::borrow::Cow;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'a> {
    // Variable / function names
    Identifier(&'a str),
    // Values
    U64Value(u64),
    U128Value(u128),
    StringValue(Cow<'a, str>),
    Null,
    True,
    False,

    // Types supported
    U8,
    U16,
    U32,
    U64,
    U128,
    Bool,
    String,
    Optional(Box<Token<'a>>),

    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,

    Const,
    Let,
    Entry,
    Function,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Return,
    If,
    Else,
    For,
    ForEach,
    While,
    Break,
    Continue,
    In,
    IsNot, // !

    Newline,

    ParenthesisOpen,
    ParenthesisClose,
    Struct,

    OperatorAssign,
    OperatorEquals,
    OperatorNotEquals,
    OperatorAnd,
    OperatorOr,
    OperatorGreaterThan,
    OperatorLessThan,
    OperatorGreaterOrEqual,
    OperatorLessOrEqual,

    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorModulo,
    OperatorBitwiseXor,
    OperatorBitwiseOr,
    OperatorBitwiseAnd,
    OperatorBitwiseLeft,
    OperatorBitwiseRight,

    OperatorPlusAssign,
    OperatorMinusAssign,
    OperatorMultiplyAssign,
    OperatorDivideAssign,
    OperatorModuloAssign,

    OperatorBitwiseXorAssign,
    OperatorBitwiseOrAssign,
    OperatorBitwiseAndAssign,
    OperatorBitwiseLeftAssign,
    OperatorBitwiseRightAssign,

    OperatorTernary,

    Import,
    From,
    As,
}

impl Token<'_> {
    pub fn value_of(s: &str) -> Option<Token> {
        use Token::*;
        Some(match s {
            "{" => BraceOpen,
            "}" => BraceClose,
            "[" => BracketOpen,
            "]" => BracketClose,

            "(" => ParenthesisOpen,
            ")" => ParenthesisClose,
            "struct" => Struct,

            "=" => OperatorAssign,
            "==" => OperatorEquals,
            "!=" => OperatorNotEquals,
            "&&" => OperatorAnd,
            "||" => OperatorOr,
            ">" => OperatorGreaterThan,
            "<" => OperatorLessThan,
            ">=" => OperatorGreaterOrEqual,
            "<=" => OperatorLessOrEqual,

            "+" => OperatorPlus,
            "-" => OperatorMinus,
            "*" => OperatorMultiply,
            "/" => OperatorDivide,
            "%" => OperatorModulo,
            "^" => OperatorBitwiseXor,
            "&" => OperatorBitwiseAnd,
            "|" => OperatorBitwiseOr,
            "<<" => OperatorBitwiseLeft,
            ">>" => OperatorBitwiseRight,

            "+=" => OperatorPlusAssign,
            "-=" => OperatorMinusAssign,
            "*=" => OperatorMultiplyAssign,
            "/=" => OperatorDivideAssign,
            "%=" => OperatorModuloAssign,

            "^=" => OperatorBitwiseXorAssign,
            "<<=" => OperatorBitwiseLeftAssign,
            ">>=" => OperatorBitwiseRightAssign,
            "&=" => OperatorBitwiseAndAssign,
            "|=" => OperatorBitwiseOrAssign,

            "?" => OperatorTernary,

            "." => Dot,
            "," => Comma,
            ":" => Colon,

            "u8" => U8,
            "u16" => U16,
            "u32" => U32,
            "u64" => U64,
            "u128" => U128,
            "bool" => Bool,
            "string" => String,

            "let" => Let,

            "const" => Const,
            "entry" => Entry,
            "func" => Function,

            "return" => Return,
            "if" => If,
            "else" => Else,
            "for" => For,
            "foreach" => ForEach,
            "while" => While,
            "break" => Break,
            "continue" => Continue,
            "in" => In,
            "!" => IsNot,

            "null" => Null,
            "true" => True,
            "false" => False,

            "import" => Import,
            "from" => From,
            "as" => As,
            _ => return None,
        })
    }

    pub fn should_stop(&self) -> bool {
        use Token::*;
        match self {
            ParenthesisClose | BraceOpen | BraceClose | BracketClose => true,
            _ => false,
        }
    }
    pub fn is_operator(&self) -> bool {
        use Token::*;
        match self {
            OperatorEquals
            | OperatorNotEquals
            | OperatorAnd
            | OperatorOr
            | OperatorGreaterThan
            | OperatorLessThan
            | OperatorGreaterOrEqual
            | OperatorLessOrEqual
            | OperatorPlus
            | OperatorMinus
            | OperatorMultiply
            | OperatorDivide
            | OperatorModulo
            | OperatorBitwiseXor
            | OperatorBitwiseOr
            | OperatorBitwiseLeft
            | OperatorBitwiseRight
            | OperatorBitwiseAnd
            | OperatorPlusAssign
            | OperatorMinusAssign
            | OperatorMultiplyAssign
            | OperatorDivideAssign
            | OperatorModuloAssign
            | OperatorBitwiseXorAssign
            | OperatorBitwiseOrAssign
            | OperatorBitwiseAndAssign
            | OperatorBitwiseLeftAssign
            | OperatorBitwiseRightAssign
            | OperatorAssign
            | BracketOpen
            | Dot
            | OperatorTernary
            | As => true,
            _ => false,
        }
    }

    pub fn is_type(&self) -> bool {
        use Token::*;
        match self {
            U8 | U16 | U64 | U128 | Bool | String | Identifier(_) | Optional(_) => true,
            _ => false,
        }
    }
}

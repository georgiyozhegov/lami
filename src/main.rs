use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;
use std::vec;

fn main() {
    let source = std::fs::read_to_string("source.lami").unwrap();
    // include standard library
    let source = std::fs::read_to_string("sl.lami").unwrap() + "\n" + &source;
    let tokenized = Lexer::new(&source).lex();
    let program = Parser::new(tokenized.into_iter()).parse();
    let interpreter = Interpreter::new(program.into_iter());
    let main = interpreter.run();
    println!("{main:#?}");
}

struct Interpreter {
    program: vec::IntoIter<Let>,
    global: Context,
}

impl Interpreter {
    pub fn new(program: impl Iterator<Item = Let>) -> Self {
        let program: Vec<_> = program.collect();
        Self {
            program: program.into_iter(),
            global: Context::new(),
        }
    }

    fn insert_sl(&mut self) {
        self.global.insert(
            ":+".into(),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left + right),
                    _ => panic!("Cannot apply \":+\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":+\" to a non-number argument"),
            })),
        );

        self.global.insert(
            ":-".into(),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left - right),
                    _ => panic!("Cannot apply \":-\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":-\" to a non-number argument"),
            })),
        );

        self.global.insert(
            ":*".into(),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number((left * right) as i128),
                    _ => panic!("Cannot apply \":*\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":*\" to a non-number argument"),
            })),
        );

        self.global.insert(
            ":<".into(),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number((left < right) as i128),
                    _ => panic!("Cannot apply \":<\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":<\" to a non-number argument"),
            })),
        );

        self.global.insert(
            ":=".into(),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number((left == right) as i128),
                    _ => panic!("Cannot apply \":=\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":=\" to a non-number argument"),
            })),
        );

        self.global.insert("_".into(), Value::Number(0));
    }

    fn run(mut self) -> Value {
        self.insert_sl();
        for let_ in self.program {
            let value = Self::evaluate(let_.value, &mut self.global);
            self.global.insert(let_.identifier, value);
        }
        let main = self
            .global
            .get("main")
            .expect("Missing the \"main\" function")
            .clone();
        main
    }

    fn evaluate(value: Expression, context: &mut Context) -> Value {
        match value {
            Expression::Lambda { parameter, body } => Value::Closure {
                parameter,
                body: *body,
                context: context.clone(),
            },
            Expression::Application { left, right } => {
                Self::evaluate_application(*left, *right, context)
            }
            Expression::If {
                condition,
                then,
                otherwise,
            } => Self::evaluate_if(*condition, *then, *otherwise, context),
            Expression::Parenthesized(inner) => Self::evaluate(*inner, context),
            Expression::Identifier(name) => match context.get(&name) {
                Some(value) => value.clone(),
                _ => panic!("Name {name:?} is not found"),
            },
            Expression::Number(raw_value) => match raw_value.parse::<i128>() {
                Ok(value) => Value::Number(value),
                _ => panic!("Failed to parse a number literal: {raw_value:?}"),
            },
        }
    }

    fn evaluate_application(left: Expression, right: Expression, context: &mut Context) -> Value {
        let left = Self::evaluate(left, context);
        let argument = Self::evaluate(right, context);
        match left {
            Value::Closure {
                parameter,
                body,
                context: mut local,
            } => {
                local.insert(parameter, argument);
                Self::evaluate(body, &mut local)
            }
            Value::Intrinsic(function) => function(argument),
            _ => panic!("Cannot apply a non-function: {left:?}"),
        }
    }

    fn evaluate_if(
        condition: Expression,
        then: Expression,
        otherwise: Expression,
        context: &mut Context,
    ) -> Value {
        match Self::evaluate(condition, context) {
            Value::Number(1) => Self::evaluate(then, context),
            Value::Number(0) => Self::evaluate(otherwise, context),
            value => panic!("Invalid boolean value: {value:?}"),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(i128),
    Closure {
        parameter: String,
        body: Expression,
        context: Context,
    },
    Intrinsic(Rc<dyn Fn(Value) -> Value>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Number(value) => write!(f, "Number({value})"),
            Self::Closure {
                parameter,
                body,
                context,
            } => f
                .debug_struct("Closure")
                .field("parameter", parameter)
                .field("body", body)
                .field("context", context)
                .finish(),
            Self::Intrinsic(_) => write!(f, "Intrinsic"),
        }
    }
}

pub type Context = HashMap<String, Value>;

struct Parser {
    tokenized: Peekable<vec::IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokenized: impl Iterator<Item = Token>) -> Self {
        let tokenized: Vec<_> = tokenized.collect();
        Self {
            tokenized: tokenized.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Vec<Let> {
        std::iter::from_fn(|| self.tokenized.peek().is_some().then(|| self.parse_let())).collect()
    }

    fn parse_let(&mut self) -> Let {
        self.expect(Token::Let);
        let identifier = self.parse_identifier();
        self.expect(Token::Equal);
        let value = self.parse_expression();
        Let { identifier, value }
    }

    fn parse_expression(&mut self) -> Expression {
        match self.tokenized.peek() {
            Some(Token::Lambda) => self.parse_lambda(),
            Some(Token::If) => self.parse_if(),
            Some(_) => self.parse_application(),
            _ => panic!("Hit the end of file when parsing an expression"),
        }
    }

    fn parse_lambda(&mut self) -> Expression {
        self.expect(Token::Lambda);
        let parameter = self.parse_identifier();
        self.expect(Token::Arrow);
        let body = self.parse_expression();
        Expression::Lambda {
            parameter,
            body: Box::new(body),
        }
    }

    fn parse_if(&mut self) -> Expression {
        self.expect(Token::If);
        let condition = self.parse_expression();
        self.expect(Token::Then);
        let then = self.parse_expression();
        self.expect(Token::Else);
        let otherwise = self.parse_expression();
        Expression::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }

    fn parse_application(&mut self) -> Expression {
        let mut left = self.parse_atom();
        while matches!(
            self.tokenized.peek(),
            Some(Token::Word(_, WordType::Identifier | WordType::Number) | Token::OpenRound)
        ) {
            let right = self.parse_atom();
            left = Expression::Application {
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        left
    }

    fn parse_atom(&mut self) -> Expression {
        match self.tokenized.peek() {
            Some(Token::OpenRound) => self.parse_parenthesized(),
            Some(Token::Word(_, WordType::Identifier)) => {
                let name = self.parse_identifier();
                Expression::Identifier(name)
            }
            Some(Token::Word(_, WordType::Number)) => {
                let value = self.parse_number();
                Expression::Number(value)
            }
            _ => panic!("Hit the end of file when parsing an atom"),
        }
    }

    fn parse_parenthesized(&mut self) -> Expression {
        self.expect(Token::OpenRound);
        let inner = self.parse_expression();
        self.expect(Token::CloseRound);
        Expression::Parenthesized(Box::new(inner))
    }

    fn parse_identifier(&mut self) -> String {
        match self.tokenized.next() {
            Some(Token::Word(name, WordType::Identifier)) => name,
            got => panic!("Expected identifier, got {got:?}"),
        }
    }

    fn parse_number(&mut self) -> String {
        match self.tokenized.next() {
            Some(Token::Word(value, WordType::Number)) => value,
            got => panic!("Expected number literal, got {got:?}"),
        }
    }

    fn expect(&mut self, token: Token) {
        let next = self.tokenized.next();
        if next.as_ref() != Some(&token) {
            panic!("Expected {token:?}, got {next:?}");
        }
    }
}

#[derive(Debug)]
pub struct Let {
    identifier: String,
    value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Lambda {
        parameter: String,
        body: Box<Expression>,
    },
    Application {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },
    Parenthesized(Box<Expression>),
    Identifier(String),
    Number(String),
}
struct Lexer<'s> {
    source: &'s str,
    cursor: usize,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self { source, cursor: 0 }
    }

    fn lex(mut self) -> Vec<Token> {
        std::iter::from_fn(|| self.token()).collect()
    }

    fn token(&mut self) -> Option<Token> {
        while matches!(self.peek(), Some(' ' | '\t' | '\n')) {
            self.eat();
        }
        let token = match self.peek()? {
            '(' => {
                self.eat();
                Token::OpenRound
            }
            ')' => {
                self.eat();
                Token::CloseRound
            }
            '=' => {
                self.eat();
                Token::Equal
            }
            '\\' => {
                self.eat();
                Token::Lambda
            }
            ch if ch == '-' && self.ahead(1) == Some('>') => {
                self.eat(); // '-'
                self.eat(); // '>'
                Token::Arrow
            }
            '_' | 'a'..='z' | 'A'..='Z' => {
                let lexeme = self.lexeme(|ch| matches!(ch, '_' | 'a'..='z' | 'A'..='Z'));
                match lexeme {
                    "let" => Token::Let,
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    _ => Token::Word(lexeme.to_owned(), WordType::Identifier),
                }
            }
            ':' => {
                self.eat(); // ':'
                let lexeme = self.lexeme(|ch| !matches!(ch, ':' | ' ' | '\t' | '\n'));
                Token::Word(":".to_owned() + lexeme, WordType::Identifier)
            }
            '0'..='9' => {
                let lexeme = self.lexeme(|ch| matches!(ch, '0'..='9'));
                Token::Word(lexeme.to_owned(), WordType::Number)
            }
            ch => panic!("Unknown character: {ch:?}"),
        };
        Some(token)
    }

    fn ahead(&self, of: usize) -> Option<char> {
        self.source[self.cursor..].chars().nth(of)
    }

    fn peek(&self) -> Option<char> {
        self.ahead(0)
    }

    fn eat(&mut self) -> Option<char> {
        self.peek().inspect(|ch| {
            self.cursor += ch.len_utf8();
        })
    }

    fn lexeme(&mut self, continues: fn(char) -> bool) -> &'s str {
        let start = self.cursor;
        while self.peek().is_some_and(continues) {
            self.eat();
        }
        &self.source[start..self.cursor]
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Word(String, WordType),
    Let,
    If,
    Then,
    Else,
    Equal,
    Lambda,
    OpenRound,
    CloseRound,
    Arrow,
}

#[derive(Debug, PartialEq)]
pub enum WordType {
    Identifier,
    Number,
}

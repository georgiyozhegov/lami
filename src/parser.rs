use std::iter::Peekable;
use std::vec;

use crate::lexer::{Interner, Symbol, Token, WordType};

pub struct Parser<'s> {
    tokenized: Peekable<vec::IntoIter<Token>>,
    interner: Interner<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(tokenized: impl Iterator<Item = Token>, interner: Interner<'s>) -> Self {
        let tokenized: Vec<_> = tokenized.collect();
        Self {
            tokenized: tokenized.into_iter().peekable(),
            interner,
        }
    }

    pub fn parse(mut self) -> (Vec<Let>, Interner<'s>) {
        (
            std::iter::from_fn(|| self.tokenized.peek().is_some().then(|| self.parse_let()))
                .collect(),
            self.interner,
        )
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

    fn parse_identifier(&mut self) -> Symbol {
        match self.tokenized.next() {
            Some(Token::Word(name, WordType::Identifier)) => name,
            got => panic!("Expected identifier, got {got:?}"),
        }
    }

    fn parse_number(&mut self) -> i128 {
        match self.tokenized.next() {
            Some(Token::Word(value, WordType::Number)) => {
                let value = self.interner.resolve(value);
                value
                    .parse()
                    .unwrap_or_else(|_| panic!("Failed to parse an integer: {value:?}"))
            }
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
    pub identifier: Symbol,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Lambda {
        parameter: Symbol,
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
    Identifier(Symbol),
    Number(i128),
}

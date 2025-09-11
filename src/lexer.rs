use std::collections::HashMap;

pub struct Lexer<'s> {
    source: &'s str,
    cursor: usize,
    interner: Interner<'s>,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            cursor: 0,
            interner: Interner::new(),
        }
    }

    pub fn lex(mut self) -> (Vec<Token>, Interner<'s>) {
        (std::iter::from_fn(|| self.token()).collect(), self.interner)
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
            '#' => {
                self.eat(); // '#'
                self.lexeme(|ch| ch != '\n');
                self.token()?
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
                    _ => Token::Word(self.interner.intern(lexeme), WordType::Identifier),
                }
            }
            ':' => {
                let lexeme = self.lexeme(|ch| !matches!(ch, ' ' | '\t' | '\n'));
                Token::Word(self.interner.intern(lexeme), WordType::Identifier)
            }
            '-' | '0'..='9' => {
                let lexeme = self.lexeme(|ch| matches!(ch, '-' | '0'..='9'));
                Token::Word(self.interner.intern(lexeme), WordType::Number)
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
    Word(Symbol, WordType),
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

pub struct Interner<'s> {
    map: HashMap<&'s str, Symbol>,
    vec: Vec<&'s str>,
}

impl<'s> Interner<'s> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn intern(&mut self, string: &'s str) -> Symbol {
        if let Some(symbol) = self.map.get(string) {
            return *symbol;
        }
        let symbol = Symbol(self.vec.len());
        self.map.insert(string, symbol);
        self.vec.push(string);
        symbol
    }

    pub fn resolve(&self, symbol: Symbol) -> &'s str {
        self.vec[symbol.0]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

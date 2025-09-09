pub struct Lexer<'s> {
    source: &'s str,
    cursor: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        Self { source, cursor: 0 }
    }

    pub fn lex(mut self) -> Vec<Token> {
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
                    _ => Token::Word(lexeme.to_owned(), WordType::Identifier),
                }
            }
            ':' => {
                self.eat(); // ':'
                let lexeme = self.lexeme(|ch| !matches!(ch, ':' | ' ' | '\t' | '\n'));
                Token::Word(":".to_owned() + lexeme, WordType::Identifier)
            }
            '-' | '0'..='9' => {
                let lexeme = self.lexeme(|ch| matches!(ch, '-' | '0'..='9'));
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

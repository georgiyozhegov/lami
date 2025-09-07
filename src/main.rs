fn main() {
    let source = std::fs::read_to_string("source.lami").unwrap();
    let tokenized = Lexer::new(&source).lex();
    for token in tokenized {
        println!("{token:?}");
    }
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
        std::iter::from_fn(|| dbg!(self.token())).collect()
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
                    _ => Token::Word(lexeme.to_owned(), WordType::Identifier),
                }
            }
            '-' | '0'..='9' => {
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

#[derive(Debug)]
pub enum Token {
    Word(String, WordType),
    Let,
    Equal,
    Lambda,
    OpenRound,
    CloseRound,
    Arrow,
}

#[derive(Debug)]
pub enum WordType {
    Identifier,
    Number,
}

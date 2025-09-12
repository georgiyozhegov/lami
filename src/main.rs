mod lexer;
mod parser;

use lexer::{Interner, Lexer, Symbol};
use parser::{Expression, Let, Parser};

use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

fn main() {
    let source = std::fs::read_to_string("source.lami").unwrap();
    // include standard library
    let source = std::fs::read_to_string("sl.lami").unwrap() + "\n" + &source;
    let (tokenized, interner) = Lexer::new(&source).lex();
    let (program, interner) = Parser::new(tokenized.into_iter(), interner).parse();
    let interpreter = Interpreter::new(program.into_iter(), interner);
    let main = interpreter.run();
    println!("{main:#?}");
}

struct Interpreter<'s> {
    program: vec::IntoIter<Let>,
    global: Context,
    interner: Interner<'s>,
}

impl<'s> Interpreter<'s> {
    pub fn new(program: impl Iterator<Item = Let>, interner: Interner<'s>) -> Self {
        let program: Vec<_> = program.collect();
        Self {
            program: program.into_iter(),
            global: Context::new(),
            interner,
        }
    }

    fn insert_sl(&mut self) {
        self.global.insert(
            self.interner.intern(":+"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left + right),
                    _ => panic!("Cannot apply \":+\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":+\" to a non-number argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":-"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left - right),
                    _ => panic!("Cannot apply \":-\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":-\" to a non-number argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":*"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left * right),
                    _ => panic!("Cannot apply \":*\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":*\" to a non-number argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":/"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number(left / right),
                    _ => panic!("Cannot apply \":/\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":/\" to a non-number argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":<"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number((left < right) as i128),
                    _ => panic!("Cannot apply \":<\" to a non-number argument"),
                })),
                _ => panic!("Cannot apply \":<\" to a non-number argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":="),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left) => Value::Intrinsic(Rc::new(move |right| match right {
                    Value::Number(right) => Value::Number((left == right) as i128),
                    Value::Nil => Value::Number(0), // number != nil
                    _ => panic!("Cannot apply \":=\" to a non-number or non-nil argument"),
                })),
                Value::Nil => Value::Intrinsic(Rc::new(move |right| {
                    Value::Number(matches!(right, Value::Nil) as i128)
                })),
                // closure != nil
                Value::Closure { .. } => Value::Intrinsic(Rc::new(move |_| Value::Number(0))),
                _ => panic!("Cannot apply \":=\" to a non-number or non-nil argument"),
            })),
        );

        self.global.insert(
            self.interner.intern(":!"),
            Value::Intrinsic(Rc::new(|value| match value {
                Value::Number(value @ (0 | 1)) => Value::Number((value == 0) as i128),
                _ => panic!("Cannot apply \":!\" to a non-boolean argument"),
            })),
        );

        self.global.insert(
            self.interner.intern("or"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left @ (0 | 1)) => {
                    Value::Intrinsic(Rc::new(move |right| match right {
                        Value::Number(right @ (0 | 1)) => {
                            Value::Number((left == 1 || right == 1) as i128)
                        }
                        _ => panic!("Cannot apply \"or\" to a non-boolean argument"),
                    }))
                }
                _ => panic!("Cannot apply \"or\" to a non-boolean argument"),
            })),
        );

        self.global.insert(
            self.interner.intern("and"),
            Value::Intrinsic(Rc::new(|left| match left {
                Value::Number(left @ (0 | 1)) => {
                    Value::Intrinsic(Rc::new(move |right| match right {
                        Value::Number(right @ (0 | 1)) => {
                            Value::Number((left == 1 && right == 1) as i128)
                        }
                        _ => panic!("Cannot apply \"and\" to a non-boolean argument"),
                    }))
                }
                _ => panic!("Cannot apply \"and\" to a non-boolean argument"),
            })),
        );

        self.global.insert(
            self.interner.intern("debug"),
            Value::Intrinsic(Rc::new(|value| dbg!(value))),
        );

        self.global.insert(self.interner.intern("nil"), Value::Nil);
    }

    fn run(mut self) -> Value {
        self.insert_sl();
        for let_ in self.program {
            let value = Self::evaluate(
                let_.value,
                &mut HashMap::new(),
                &self.global,
                &self.interner,
            );
            self.global.insert(let_.identifier, value);
        }
        let main = self
            .global
            .get(&self.interner.intern("main"))
            .expect("Missing the \"main\" function")
            .clone();
        main
    }

    fn evaluate(
        value: Expression,
        context: &mut Context,
        global: &Context,
        interner: &Interner<'s>,
    ) -> Value {
        match value {
            Expression::Lambda { parameter, body } => Value::Closure {
                parameter,
                body: *body,
                context: context.clone(),
            },
            Expression::Application { left, right } => {
                Self::evaluate_application(*left, *right, context, global, interner)
            }
            Expression::If {
                condition,
                then,
                otherwise,
            } => Self::evaluate_if(*condition, *then, *otherwise, context, global, interner),
            Expression::Parenthesized(inner) => Self::evaluate(*inner, context, global, interner),
            Expression::Identifier(name) => match context.get(&name).or(global.get(&name)) {
                Some(value) => value.clone(),
                _ => panic!("Name {:?} is not found", interner.resolve(name)),
            },
            Expression::Number(value) => Value::Number(value),
        }
    }

    fn evaluate_application(
        left: Expression,
        right: Expression,
        context: &mut Context,
        global: &Context,
        interner: &Interner<'s>,
    ) -> Value {
        let left = Self::evaluate(left, context, global, interner);
        let argument = Self::evaluate(right, context, global, interner);
        match left {
            Value::Closure {
                parameter,
                body,
                context: mut local,
            } => {
                local.insert(parameter, argument);
                Self::evaluate(body, &mut local, global, interner)
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
        global: &Context,
        interner: &Interner<'s>,
    ) -> Value {
        match Self::evaluate(condition, context, global, interner) {
            Value::Number(1) => Self::evaluate(then, context, global, interner),
            Value::Number(0) => Self::evaluate(otherwise, context, global, interner),
            value => panic!("Invalid boolean value: {value:?}"),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(i128),
    Closure {
        parameter: Symbol,
        body: Expression,
        context: Context,
    },
    Intrinsic(Rc<dyn Fn(Value) -> Value>),
    Nil,
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
            Self::Nil => write!(f, "Nil"),
        }
    }
}

pub type Context = HashMap<Symbol, Value>;


#[derive(Clone, Debug, PartialEq)]
pub struct Pos {
    pub src_name: String,
    pub line: i32,
    pub col: i32,
}

#[derive(Debug)]
pub enum Error {
    Parse(Pos, Option<String>, String),
    ParseRecoverable(Pos, Option<String>, String),
}
impl Pretty for Error {
    fn pretty(self: Self) -> String {
        match self {
            Error::Parse(pos, expected, msg) => {
                let mut s = format!("Parse error. {} at `{}:{}:{}`.", msg, pos.src_name, pos.line, pos.col);
                if let Some(expected) = expected {
                    s.push_str(&format!(" Expected {}.", expected));
                }
                s
            }
            Error::ParseRecoverable(pos, expected, msg) => Error::Parse(pos, expected, msg).pretty()
        }
    }
}

pub trait Pretty {
    fn pretty(self: Self) -> String;
}

#[derive(Clone, Debug)]
pub enum Syntax {
    Lambda(Pos, String, Box<Syntax>),
    Ident(Pos, String),
    Call(Pos, Box<Syntax>, Box<Syntax>),
    Int(Pos, i64),
    LetForce(Pos, String, Box<Syntax>, Box<Syntax>),
    Object(Pos, Option<String>, Vec<(String, String, Syntax)>),
    Access(Pos, Box<Syntax>, String),
    Update(Pos, Box<Syntax>, String, String, Box<Syntax>),
    Operator(Pos, String, Box<Syntax>, Box<Syntax>),
    String(Pos, String),
    Float(Pos, f64),
    Module(Pos, String, Vec<(String, String, Syntax)>),
}
impl Syntax {
    pub fn pos(&self) -> &Pos {
        match self {
            Syntax::Lambda(pos, _, _) => pos,
            Syntax::Ident(pos, _) => pos,
            Syntax::Call(pos, _, _) => pos,
            Syntax::Int(pos, _) => pos,
            Syntax::LetForce(pos, _, _, _) => pos,
            Syntax::Object(pos, _, _) => pos,
            Syntax::Access(pos, _, _) => pos,
            Syntax::Update(pos, _, _, _, _) => pos,
            Syntax::Operator(pos, _, _, _) => pos,
            Syntax::String(pos, _) => pos,
            Syntax::Float(pos, _) => pos,
            Syntax::Module(pos, _, _) => pos,
        }
    }
}
impl Pretty for Syntax {
    fn pretty(self: Self) -> String {
        match self {
            Syntax::Lambda(_, ident, body) => ident + "-> " + &body.pretty(),
            Syntax::Ident(_, ident) => ident,
            Syntax::Call(_, foo, bar) => "(".to_owned() + &foo.pretty() + ")(" + &bar.pretty() + ")",
            Syntax::Int(_, i) => format!("{}", i),
            Syntax::LetForce(_, ident, val, scope) => "let force ".to_owned() + &ident + " = " + &val.pretty() + " in " + &scope.pretty(),
            Syntax::Object(_, Some(name), _) => name,
            Syntax::Object(_, None, methods) => "{".to_owned() + &methods.iter().map(|(this, method, def)| this.to_owned() + "." + &method + ": " + &def.clone().pretty()).collect::<Vec<String>>().join(", ") + "}",
            Syntax::Access(_, ob, method) => ob.pretty() + "." + &method,
            Syntax::Update(_, ob, this, method, def) => "(".to_owned() + &ob.pretty() + " <- " + &this + "." + &method + ": " + &def.pretty() + ")",
            Syntax::Operator(_, op, left, right) => left.pretty() + " " + &op + " " + &right.pretty(),
            Syntax::String(_, s) => "\"".to_owned() + &s + "\"",
            Syntax::Float(_, f) => format!("{}", f),
            Syntax::Module(_, name, _) => name,
        }
    }
}
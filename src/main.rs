use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
struct Pos {
    src_name: String,
    line: i32,
    col: i32,
}

#[derive(Debug)]
enum Error {
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

#[derive(Clone, Debug)]
struct ParseData<'p> {
    src_iter: Chars<'p>,
    pos: Pos,
    expected: Option<String>,
}

type ParseResult<T> = Result<T, Error>;

type Parser<'p, 'a, T> = &'a dyn Fn(&mut ParseData<'p>) -> ParseResult<T>;

trait Pretty {
    fn pretty(self: Self) -> String;
}

fn satisfy<'p>(data: &mut ParseData<'p>, pred: &dyn Fn(char) -> bool) -> ParseResult<char> {
    match data.src_iter.next() {
        Some(c) if c == '\n' && pred(c) => {
            data.pos = Pos {
                src_name: data.pos.src_name.clone(),
                line: data.pos.line + 1,
                col: 0,
            };
            Ok(c)
        }
        Some(c) if pred(c) => {
            data.pos = Pos {
                src_name: data.pos.src_name.clone(),
                line: data.pos.line,
                col: data.pos.col + 1,
            };
            Ok(c)
        }
        Some(c) => Err(Error::ParseRecoverable(
            data.pos.clone(),
            data.expected.clone(),
            format!("Unexpected `{}`", c),
        )),
        None => Err(Error::ParseRecoverable(
            data.pos.clone(),
            data.expected.clone(),
            "Unexpected end of input".to_owned(),
        )),
    }
}

fn commit<'p, 'a, T>(data: &mut ParseData<'p>, parser: Parser<'p, 'a, T>) -> ParseResult<T> {
    match parser(data) {
        Ok(res) => Ok(res),
        Err(Error::ParseRecoverable(pos, expected, msg)) => Err(Error::Parse(pos, expected, msg)),
        Err(e) => Err(e),
    }
}

fn the_char<'p>(data: &mut ParseData<'p>, c: char) -> ParseResult<char> {
    satisfy(data, &|c2| c == c2)
}

fn one_of<'p, 'a, T>(data: &mut ParseData<'p>, parsers: &[Parser<'p, 'a, T>]) -> ParseResult<T> {
    match parsers {
        [] => panic!(),
        [parser] => parser(data),
        [parser, rest @ ..] => {
            let mut new_data = data.clone();
            match parser(&mut new_data) {
                // data.src_iter getting cloned is how I'm implementing backtracking lol
                Ok(res) => {
                    data.pos = new_data.pos;
                    data.src_iter = new_data.src_iter;
                    Ok(res)
                }
                Err(e @ Error::Parse(_, _, _)) => Err(e),
                Err(_err) => one_of(data, rest), // note: no tail call because of .clone above
            }
        }
    }
}

fn possible<'p, 'a, T>(
    data: &mut ParseData<'p>,
    parser: Parser<'p, 'a, T>,
) -> ParseResult<Option<T>> {
    let mut new_data = data.clone();
    match parser(&mut new_data) {
        Ok(res) => {
            data.pos = new_data.pos;
            data.src_iter = new_data.src_iter;
            Ok(Some(res))
        }
        Err(e @ Error::Parse(_, _, _)) => Err(e),
        Err(_err) => Ok(None),
    }
}

fn many0<'p, 'a, T>(data: &mut ParseData<'p>, parser: Parser<'p, 'a, T>) -> ParseResult<Vec<T>> {
    let mut new_data = data.clone();
    match parser(&mut new_data) {
        Ok(res) => {
            let mut res = vec![res];
            let rest = many0(&mut new_data, parser)?;
            res.extend(rest);
            data.pos = new_data.pos;
            data.src_iter = new_data.src_iter;
            Ok(res)
        }
        Err(e @ Error::Parse(_, _, _)) => Err(e),
        Err(_err) => Ok(vec![]),
    }
}

fn many<'p, 'a, T>(data: &mut ParseData<'p>, parser: Parser<'p, 'a, T>) -> ParseResult<Vec<T>> {
    let first = parser(data)?;
    let mut res = vec![first];
    let rest = many0(data, parser)?;
    res.extend(rest);
    Ok(res)
}

fn exact<'p>(data: &mut ParseData<'p>, s: &str) -> ParseResult<String> {
    for c in s.chars() {
        the_char(data, c)?;
    }
    Ok(s.to_string())
}

fn sep_by<'p, 'a, T, U>(
    data: &mut ParseData<'p>,
    by: Parser<'p, 'a, U>,
    parser: Parser<'p, 'a, T>,
) -> ParseResult<Vec<T>> {
    let first = parser(data)?;
    let mut res = vec![first];
    let rest = many0(data, &|d| {
        by(d)?;
        parser(d)
    })?;
    res.extend(rest);
    Ok(res)
}

fn sep_by0<'p, 'a, T, U>(
    data: &mut ParseData<'p>,
    by: Parser<'p, 'a, U>,
    parser: Parser<'p, 'a, T>,
) -> ParseResult<Vec<T>> {
    one_of(data, &[&move |d| sep_by(d, by, parser), &|_d| Ok(vec![])])
}

fn comment<'p>(data: &mut ParseData<'p>) -> ParseResult<char> {
    exact(data, "//")?;
    many0(data, &|d| satisfy(d, &|c| c != '\n'))?;
    possible(data, &|d| the_char(d, '\n'))?;
    Ok('\n')
}

fn whitespace0<'p>(data: &mut ParseData<'p>) -> ParseResult<Vec<char>> {
    many0(data, &|d| {
        one_of(
            d,
            &[
                &|d2| the_char(d2, ' '),
                &|d2| the_char(d2, '\n'),
                &|d2| the_char(d2, '\t'),
                &comment,
            ],
        )
    })
}

fn whitespace<'p>(data: &mut ParseData<'p>) -> ParseResult<Vec<char>> {
    many(data, &|d| {
        one_of(
            d,
            &[
                &|d2| the_char(d2, ' '),
                &|d2| the_char(d2, '\n'),
                &|d2| the_char(d2, '\t'),
                &comment,
            ],
        )
    })
}

fn ident_string<'p>(data: &mut ParseData<'p>) -> ParseResult<String> {
    let first = satisfy(data, &|c| c.is_alphabetic())?;
    let rest = many0(data, &|d| satisfy(d, &|c| c.is_alphanumeric() || c == '_'))?;
    Ok(format!("{}{}", first, rest.iter().collect::<String>()))
}

fn pattern_string<'p>(data: &mut ParseData<'p>) -> ParseResult<String> {
    one_of(
        data,
        &[
            &|d| ident_string(d), 
            &|d| {
                the_char(d, '_')?;
                let mb_rest = possible(d, &ident_string)?;
                match mb_rest {
                    Some(rest) => Ok(format!("_{}", rest)),
                    None => Ok("_".to_owned()),
                }
            }
        ],
    )
}

fn escaped<'p>(data: &mut ParseData<'p>) -> ParseResult<char> {
    the_char(data, '\\')?;
    let c = commit(data, &|d| satisfy(d, &|_c| true))?;
    match c {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        'r' => Ok('\r'),
        _ => Ok(c),
    }
}

#[derive(Clone, Debug)]
enum Syntax {
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
    fn pos(&self) -> &Pos {
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

fn parse_ident_or_lambda<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    let ident = ident_string(data)?;
    whitespace0(data)?;
    let mb_arrow = possible(data, &|d| exact(d, "->"))?;
    match mb_arrow {
        Some(_) => {
            let body = commit(data, &parse_term)?;
            Ok(Syntax::Lambda(pos, ident, Box::new(body)))
        }
        None => Ok(Syntax::Ident(pos, ident)),
    }
}

fn parse_constant_lambda<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    the_char(data, '_')?;
    let x = possible(data, &ident_string)?;
    let param = "_".to_owned() + &x.unwrap_or("".to_string());
    whitespace0(data)?;
    commit(data, &|d| exact(d, "->"))?;
    let body = commit(data, &parse_term)?;
    Ok(Syntax::Lambda(pos, param, Box::new(body)))
}

fn parse_num<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    let mb_neg = possible(data, &|d| the_char(d, '-'))?;
    let whole = many(data, &|d| satisfy(d, &|c| c.is_digit(10)))?;
    let mb_dec = possible(data, &|d| {
        the_char(d, '.')?;
        commit(d, &|d2| many(d2, &|d3| satisfy(d3, &|c| c.is_digit(10))))
    })?;
    match (mb_neg, mb_dec) {
        (Some(_), Some(dec)) => {
            let s = format!(
                "{}.{}",
                whole.iter().collect::<String>(),
                dec.iter().collect::<String>()
            );
            let f = s.parse::<f64>().unwrap();
            Ok(Syntax::Float(pos, -f))
        }
        (Some(_), None) => {
            let i = whole.iter().collect::<String>().parse::<i64>().unwrap();
            Ok(Syntax::Int(pos, -i))
        }
        (None, Some(dec)) => {
            let s = format!(
                "{}.{}",
                whole.iter().collect::<String>(),
                dec.iter().collect::<String>()
            );
            let f = s.parse::<f64>().unwrap();
            Ok(Syntax::Float(pos, f))
        }
        (None, None) => {
            let i = whole.iter().collect::<String>().parse::<i64>().unwrap();
            Ok(Syntax::Int(pos, i))
        }
    }
}

fn parse_string<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    the_char(data, '"')?;
    let s = many0(data, &|d| {
        one_of(d, &[
            &escaped,
            &|d2| satisfy(d2, &|c| c != '"')
        ])
    })?;
    commit(data, &|d| the_char(d, '"'))?;
    Ok(Syntax::String(pos, s.iter().collect::<String>()))
}

enum LetType {
    Force,
    Basic,
    Back,
}

fn parse_let<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    exact(data, "let")?;
    commit(data, &whitespace)?;
    let pat = commit(data, &pattern_string)?;
    whitespace0(data)?;
    let (ident, let_type, params) = match pat.as_str() {
        "force" => {
            let ident = commit(data, &ident_string)?;
            let mb_params = possible(data, &parse_params)?;
            let params = mb_params.unwrap_or(vec![]);
            whitespace0(data)?;
            commit(data, &|d| the_char(d, '='))?;
            Ok((ident, LetType::Force, params))
        }
        _ => {
            let mb_params = possible(data, &parse_params)?;
            match mb_params {
                Some(params) => {
                    whitespace0(data)?;
                    commit(data, &|d| the_char(d, '='))?;
                    Ok((pat, LetType::Basic, params))
                }
                None => {
                    let op = commit(data, &|d| one_of(d, &[&|d2| exact(d2, "="), &|d2| exact(d2, "<-")]))?;
                    match op.as_str() {
                        "=" => Ok((pat, LetType::Basic, vec![])),
                        "<-" => Ok((pat, LetType::Back, vec![])),
                        _ => unreachable!(),
                    }
                }
            }
        }
    }?;
    let val = commit(data, &parse_term)?;
    commit(data, &|d| exact(d, "in"))?;
    commit(data, &whitespace)?;
    let scope = commit(data, &parse_term)?;
    // I hope the optimizer knows it can take ownership of each string, instead of cloning them all just to free them at the end
    let val2 = params.iter().fold(val, |acc, p| {
        Syntax::Lambda(pos.clone(), p.to_owned(), Box::new(acc))
    });
    match let_type {
        LetType::Force => Ok(Syntax::LetForce(
            pos,
            ident,
            Box::new(val2),
            Box::new(scope),
        )),
        LetType::Basic => Ok(Syntax::Call(
            pos.clone(),
            Box::new(Syntax::Lambda(pos, ident, Box::new(scope))),
            Box::new(val2),
        )),
        LetType::Back => Ok(Syntax::Call(
            pos.clone(),
            Box::new(val2),
            Box::new(Syntax::Lambda(pos, ident, Box::new(scope))),
        )),
    }
}

fn parse_methods<'p>(data: &mut ParseData<'p>) -> ParseResult<Vec<(String, String, Syntax)>> {
    sep_by0(data, &|d| the_char(d, ','), &|d| {
        whitespace0(d)?;
        let w = ident_string(d)?;
        let mb_dot = possible(d, &|d2| exact(d2, "."))?;
        let (this, method) = match mb_dot {
            Some(_) => {
                let method = commit(d, &ident_string)?;
                Ok((w, method))
            }
            None => Ok(("_".to_string(), w)),
        }?;
        whitespace0(d)?;
        let mb_params = possible(d, &parse_params)?;
        commit(d, &|d2| the_char(d2, ':'))?;
        let def_pos = d.pos.clone();
        let def = commit(d, &parse_term)?;
        whitespace0(d)?;
        let def2 = match mb_params {
            Some(params) => params.iter().fold(def, |acc, p| {
                Syntax::Lambda(def_pos.clone(), p.to_owned(), Box::new(acc))
            }),
            None => def,
        };
        Ok((this, method, def2))
    })
}

fn parse_object<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let pos = data.pos.clone();
    the_char(data, '{')?;
    let methods = parse_methods(data)?;
    commit(data, &|d| the_char(d, '}'))?;
    Ok(Syntax::Object(pos, None, methods))
}

fn parse_parens<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    the_char(data, '(')?;
    let t = commit(data, &|d| parse_term(d))?;
    commit(data, &|d: &mut ParseData<'_>| the_char(d, ')'))?;
    Ok(t)
}

fn parse_term_no_prefix<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    whitespace0(data)?;
    let t = one_of(
        data,
        &[
            &parse_parens,
            &parse_object,
            &parse_constant_lambda,
            &parse_string,
            &parse_num,
            &parse_let,
            &parse_ident_or_lambda,
        ],
    )?;
    whitespace0(data)?;
    Ok(t)
}

#[derive(Debug)]
enum Postfix {
    Call(Pos, Syntax),
    Access(Pos, String),
    Update(Pos, String, String, Syntax),
    Operator(Pos, String, Syntax),
    Monoid(Pos, Vec<Syntax>),
    Apostrophe(Pos, Syntax),
}

fn parse_term<'p>(data: &mut ParseData<'p>) -> ParseResult<Syntax> {
    let t = parse_term_no_prefix(data)?;
    let args = many0(data, &|d| {
        one_of(
            d,
            &[
                &|d2| {
                    let pos = d2.pos.clone();
                    let arg = one_of(d2, &[&parse_parens, &parse_object])?;
                    Ok(Postfix::Call(pos, arg))
                },
                &|d2| {
                    let pos = d2.pos.clone();
                    the_char(d2, '.')?;
                    let ident = commit(d2, &ident_string)?;
                    whitespace0(d2)?;
                    Ok(Postfix::Access(pos, ident))
                },
                &|d2| {
                    let pos = d2.pos.clone();
                    exact(d2, "<-")?;
                    whitespace0(d2)?;
                    let w = commit(d2, &ident_string)?;
                    let mb_dot = possible(d2, &|d3| the_char(d3, '.'))?;
                    let (this, method) = match mb_dot {
                        Some(_) => {
                            let method = commit(d2, &ident_string)?;
                            Ok((w, method))
                        }
                        None => Ok(("_".to_string(), w)),
                    }?;
                    whitespace0(d2)?;
                    commit(d2, &|d3| the_char(d3, ':'))?;
                    let body = commit(d2, &parse_term)?;
                    Ok(Postfix::Update(pos, this, method, body))
                },
                &|d2| {
                    // todo: pratt parsing; proper order of operations/infix levels
                    // alternatively, wuffs(?)-style required parentheses
                    let pos = d2.pos.clone();
                    let op = one_of(
                        d2,
                        &[
                            &|d3| exact(d3, "*"),
                            &|d3| exact(d3, "-"),
                            &|d3| exact(d3, "+"),
                            &|d3| exact(d3, "/"),
                            &|d3| exact(d3, "%"),
                            &|d3| exact(d3, "<"),
                            &|d3| exact(d3, ">"),
                            &|d3| exact(d3, "=="),
                            &|d3| exact(d3, "<="),
                            &|d3| exact(d3, ">="),
                            &|d3| exact(d3, "!="),
                        ],
                    )?;
                    let rhs = commit(d2, &parse_term)?;
                    Ok(Postfix::Operator(pos, op, rhs))
                },
                &|d2| {
                    let pos = d2.pos.clone();
                    the_char(d2, '[')?;
                    let terms = sep_by0(d2, &|d3| the_char(d3, ','), &parse_term)?;
                    commit(d2, &|d3| the_char(d3, ']'))?;
                    Ok(Postfix::Monoid(pos, terms))
                },
                &|d2| {
                    let pos = d2.pos.clone();
                    the_char(d2, '\'')?;
                    let arg = commit(d2, &parse_term)?;
                    Ok(Postfix::Apostrophe(pos, arg))
                },
            ],
        )
    })?;
    let out = match args.as_slice() {
        [] => t,
        _ => args.iter().fold(t, |acc, a| match a {
            Postfix::Call(pos, arg) => Syntax::Call(pos.clone(), Box::new(acc), Box::new(arg.clone())),
            Postfix::Access(pos, ident) => Syntax::Access(pos.clone(), Box::new(acc), ident.to_string()),
            Postfix::Update(pos, this, method, body) => Syntax::Update(
                pos.clone(),
                Box::new(acc),
                this.to_string(),
                method.to_string(),
                Box::new(body.clone()),
            ),
            Postfix::Operator(pos, op, rhs) => {
                Syntax::Operator(pos.clone(), op.to_string(), Box::new(acc), Box::new(rhs.clone()))
            }
            Postfix::Monoid(pos, terms) => terms.iter().fold(
                Syntax::Access(acc.pos().clone(), Box::new(acc.clone()), "Empty".to_string()),
                |so_far, term| {
                    Syntax::Call(
                        pos.clone(),
                        Box::new(Syntax::Call(
                            pos.clone(),
                            Box::new(Syntax::Access(
                                acc.pos().clone(),
                                Box::new(
                                    acc.clone(), // ouch! small ASTs are recommended for monoids
                                ),
                                "Has".to_string(),
                            )),
                            Box::new(term.clone()),
                        )),
                        Box::new(so_far),
                    )
                },
            ),
            Postfix::Apostrophe(pos, arg) => {
                Syntax::Call(pos.clone(), Box::new(acc), Box::new(arg.clone()))
            }
        }),
    };
    whitespace0(data)?;
    Ok(out)
}

fn parse_params<'p>(data: &mut ParseData<'p>) -> ParseResult<Vec<String>> {
    many(data, &|d| {
        the_char(d, '(')?;
        whitespace0(d)?;
        let param = commit(d, &pattern_string)?;
        whitespace0(d)?;
        commit(d, &|d2| the_char(d2, ')'))?;
        Ok(param)
    })
}

fn main() {
    let mut data = ParseData {
        pos: Pos {
            src_name: "input".to_owned(),
            line: 1,
            col: 1,
        },
        expected: None,
        src_iter: "let x = {this.y: 8, z: \"\"} in print(x.z)".chars(),
    };
    let res = parse_term(&mut data);
    match res {
        Ok(x) => 
            match data.src_iter.next() {
                None => println!("{}", x.pretty()),
                Some(c) => println!("{}", Error::Parse(data.pos, None, format!("Unexpected {}", c)).pretty())
            }
        Err(e) => println!("{}", e.pretty()),
    }
}

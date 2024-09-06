use std::str::Chars;

#[derive(Clone, Copy)]
struct SrcPtr(i32);

#[derive(Clone, Copy)]
struct SrcNamePtr(i32);

#[derive(Clone, Copy)]
struct Pos(SrcNamePtr, i32, i32);

enum Error {
    Parse(Pos, Option<String>, String),
}

#[derive(Clone)]
struct ParseData<'p> {
    src_iter: Chars<'p>,
    pos: Pos,
    expected: Option<String>,
}

type ParseResult<T> = Result<T, Error>;

type Parser<'p, 'a, T> = &'a dyn Fn(&mut ParseData<'p>) -> ParseResult<T>;

fn satisfy<'p>(data: &mut ParseData<'p>, pred: &dyn Fn(char) -> bool) -> ParseResult<char> {
    let Pos(src_name, line, col) = data.pos;
    match data.src_iter.next() {
        Some(c) if c == '\n' && pred(c) => {
            data.pos = Pos(src_name, line + 1, 0);
            Ok(c)
        }
        Some(c) if pred(c) => {
            data.pos = Pos(src_name, line, col + 1);
            Ok(c)
        }
        Some(c) => Err(Error::Parse(
            data.pos,
            data.expected.clone(),
            format!("Unexpected `{}`", c),
        )),
        None => Err(Error::Parse(
            data.pos,
            data.expected.clone(),
            "Unexpected end of input".to_owned(),
        )),
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
            match parser(&mut data.clone()) {
                // data.src_iter getting cloned is how I'm implementing backtracking lol
                Ok(res) => Ok(res),
                Err(_err) => one_of(data, rest), // note: no tail call because of .clone above
            }
        }
    }
}

fn possible<'p, 'a, T>(
    data: &mut ParseData<'p>,
    parser: Parser<'p, 'a, T>,
) -> ParseResult<Option<T>> {
    match parser(data) {
        Ok(res) => Ok(Some(res)),
        Err(_err) => Ok(None),
    }
}

fn many0<'p, 'a, T>(data: &mut ParseData<'p>, parser: Parser<'p, 'a, T>) -> ParseResult<Vec<T>> {
    let mut res = Vec::new();
    loop {
        match parser(data) {
            Ok(res2) => res.push(res2),
            Err(_err) => break,
        }
    }
    Ok(res)
}

fn many<'p, 'a, T>(data: &mut ParseData<'p>, parser: Parser<'p, 'a, T>) -> ParseResult<Vec<T>> {
    let first = parser(data)?;
    let mut res = vec![first];
    let rest = many0(data, parser)?;
    res.extend(rest);
    Ok(res)
}

fn exact<'p>(data: &mut ParseData<'p>, s: &str) -> ParseResult<()> {
    for c in s.chars() {
        the_char(data, c)?;
    }
    Ok(())
}

fn sep_by<'p, 'a, T>(
    data: &mut ParseData<'p>,
    by: Parser<'p, 'a, ()>,
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

fn sep_by0<'p, 'a, T>(
    data: &mut ParseData<'p>,
    by: Parser<'p, 'a, ()>,
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
        &[&|d| ident_string(d), &|d| {
            the_char(d, '_')?;
            let mb_rest = possible(d, &ident_string)?;
            match mb_rest {
                Some(rest) => Ok(format!("_{}", rest)),
                None => Ok("_".to_owned()),
            }
        }],
    )
}

fn escaped<'p>(data: &mut ParseData<'p>) -> ParseResult<char> {
    the_char(data, '\\')?;
    let c = satisfy(data, &|_c| true)?;
    match c {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        'r' => Ok('\r'),
        _ => Ok(c),
    }
}

#[derive(Clone)]
enum Syntax<'p> {
    Lambda(Pos, String, &'p Syntax<'p>),
    Ident(Pos, String),
    Call(Pos, &'p Syntax<'p>, &'p Syntax<'p>),
    Int(Pos, i64),
    LetForce(Pos, String, &'p Syntax<'p>, &'p Syntax<'p>),
    Object(Pos, Option<String>, Vec<(String, String, Syntax<'p>)>),
    Access(Pos, &'p Syntax<'p>, String),
    Update(Pos, &'p Syntax<'p>, String, String, &'p Syntax<'p>),
    Operator(Pos, String, &'p Syntax<'p>, &'p Syntax<'p>),
    String(Pos, String),
    Float(Pos, f64),
    Module(Pos, String, Vec<(String, String, Syntax<'p>)>),
}

fn main() {
    println!("Hello, world!");
}

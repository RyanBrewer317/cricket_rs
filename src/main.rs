mod common;

use common::*;
mod parser;
use parser::*;
mod translate;
use translate::*;

fn go() -> Result<Term, Error> {
    let mut data = ParseData {
        pos: Pos {
            src_name: "input".to_owned(),
            line: 1,
            col: 1,
        },
        expected: None,
        src_iter: "let x = {this.y: 8, z: \"\"} in x.z".chars(),
    };
    let syntax = parse_term(&mut data)?;
    if let Some(c) = data.src_iter.next() {
        return Err(Error::Parse(data.pos, None, format!("Unexpected {}", c)));
    }
    let term = translate(&"input".to_owned(), 0, &mut vec![], syntax);
    Ok(term)
}

fn main() {
    let res = go();
    match res {
        Ok(x) => println!("{}", x.pretty()),
        Err(e) => println!("{}", e.pretty()),
    }
}

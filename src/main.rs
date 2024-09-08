mod common;
use common::*;
mod parser;
use parser::*;

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

mod common;

use std::{collections::HashMap, fs::read_to_string, io::{self, stdout, BufRead, Write}};

use common::*;
mod parser;
use parser::*;
mod translate;
use translate::*;

fn process_import(mod_path_segments: &Vec<String>) -> Result<(String, String, Syntax), Error> {
    let mut mod_path_segments_copy = mod_path_segments.clone();
    let mod_name = mod_path_segments_copy.pop().unwrap();
    let mod_path = mod_path_segments.join("/");
    let pos = Pos{src_name: mod_path.clone(), line: 1, col: 1};
    let res = read_to_string(mod_path.to_owned() + ".ct");
    let src = match res {
        Ok(s) => s,
        Err(_) => panic!("couldn't read file `{}`.", mod_path)
    };
    let mut parser_data = ParseData {
        pos: pos.clone(),
        expected: None,
        src_iter: src.chars(),
    };
    let (decls, imports, params) = parse_file(&mut parser_data)?;
    if let Some(c) = parser_data.src_iter.next() {
        return Err(Error::Parse(parser_data.pos, None, format!("Unexpected `{}`", c)))
    }
    let mods = imports.into_iter().map(|path| process_import(&[mod_path_segments_copy.clone(), path].concat())).collect::<Result<Vec<_>, _>>()?;
    let mut mod_methods_hash = builtins(pos.clone());
    mod_methods_hash.extend(decls);
    let mod_methods = mod_methods_hash.into_iter().collect::<Vec<_>>();
    let ob = Syntax::Module(pos.clone(), mod_name.clone(), [mods, mod_methods.into_iter().map(|(a,b)| ("$".to_owned()+&mod_name,a,b)).collect()].concat());
    let ob2 = params.into_iter().fold(ob, |acc, param| Syntax::Lambda(pos.clone(), param, Box::new(acc)));
    Ok(("$".to_owned()+&mod_name, mod_name, ob2))
}

fn remove_suffix(sub: String, s: String) -> String {
    s.strip_suffix(&sub).unwrap_or(&s).to_owned()
}

fn builtins(pos: Pos) -> HashMap<String, Syntax> {
    HashMap::from([
        ("console".to_owned(), Syntax::Object(pos.clone(), Some("console".to_string()), vec![
            ("".to_string(), "write".to_string(), Syntax::Ident(pos.clone(), "#console_write".to_string())),
            ("".to_string(), "read".to_string(), Syntax::Ident(pos, "#console_read".to_string())),
        ]))
    ])
}

fn go() -> Result<Term, Error> {
    let mut args = std::env::args();
    match args.nth(1) {
        None => {
            print!("> ");
            stdout().flush().unwrap();
            let stdin = io::stdin();
            let inp = stdin.lock().lines().next().unwrap().unwrap();
            let pos = Pos {
                src_name: "input".to_owned(),
                line: 1,
                col: 1,
            };
            let mut data = ParseData {
                pos: pos.clone(),
                expected: None,
                src_iter: inp.chars(),
            };
            let t = parse_term(&mut data)?;
            if let Some(c) = data.src_iter.next() {
                return Err(Error::Parse(data.pos, None, format!("Unexpected {}", c)));
            }
            let stdlib = process_import(&vec!["stdlib".to_owned()]).unwrap();
            let mut mod_methods_hash = builtins(pos.clone());
            mod_methods_hash.insert("main".to_owned(), t);
            let mut mod_methods = mod_methods_hash.into_iter().map(|(a,b)| ("$input".to_string(),a,b)).collect::<Vec<_>>();
            mod_methods.push(stdlib);
            let ob = Syntax::Module(pos, "input".to_string(), mod_methods);
            let t2 = translate(&"input".to_string(), 0, &mut vec![], ob);
            return Ok(t2)
        }
        Some(filename) => {
            let mod_name = remove_suffix(".ct".to_owned(), filename.clone());
            let pos = Pos {
                src_name: filename,
                line: 1,
                col: 1,
            };
            let (_, _, t) = process_import(&vec![mod_name.clone()])?;
            let t2 = translate(&mod_name, 0, &mut vec![], t);
            let entry = Term::Access(pos, Box::new(t2), "main".to_owned());
            return Ok(entry)
        }
    }
}

fn main() {
    let res = go();
    match res {
        Ok(x) => println!("{}", x.pretty()),
        Err(e) => println!("{}", e.pretty()),
    }
}

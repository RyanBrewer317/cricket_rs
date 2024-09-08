use std::collections::HashMap;

use crate::common::*;

pub fn translate(
    mod_name: &String,
    index: i64,
    renames: &mut Vec<(String, i64)>,
    t: Syntax,
) -> Term {
    match t {
        Syntax::Lambda(pos, param, body) => {
            renames.push((param.clone(), index));
            let body2 = translate(mod_name, index + 1, renames, *body);
            renames.pop();
            Term::Lambda(pos, param, Box::new(body2))
        }
        Syntax::Ident(pos, name) => {
            if name.starts_with('#') {
                Term::Ident(pos, 0, name)
            } else {
                match renames.iter().rev().find(|(k, _)| k == &name) {
                    Some((_, i)) => Term::Ident(pos, index - i - 1, name),
                    None => {
                        let t2 = Syntax::Access(
                            pos.clone(),
                            Box::new(Syntax::Ident(pos, mod_name.clone())),
                            name,
                        );
                        translate(mod_name, index, renames, t2)
                    }
                }
            }
        }
        Syntax::Call(pos, foo, bar) => {
            let foo2 = translate(mod_name, index, renames, *foo);
            let bar2 = translate(mod_name, index, renames, *bar);
            Term::Call(pos, Box::new(foo2), Box::new(bar2))
        }
        Syntax::Int(pos, i) => Term::Int(pos, i),
        Syntax::LetForce(pos, ident, val, scope) => {
            let val2 = translate(mod_name, index, renames, *val);
            renames.push((ident.clone(), index));
            let scope2 = translate(mod_name, index + 1, renames, *scope);
            renames.pop();
            Term::LetForce(pos, ident, Box::new(val2), Box::new(scope2))
        }
        Syntax::Object(pos, mb_name, methods) => {
            let mut methods2 = HashMap::new();
            for (this, method, def) in methods {
                renames.push((this.clone(), index));
                let def2 = translate(mod_name, index + 1, renames, def);
                renames.pop();
                methods2.insert(method, (this, def2));
            }
            Term::Object(pos, mb_name, methods2)
        }
        Syntax::Module(pos, name, methods) => {
            let mut methods2 = HashMap::new();
            for (this, method, def) in methods {
                renames.push((this.clone(), index));
                let def2 = translate(&this, index + 1, renames, def);
                renames.pop();
                methods2.insert(method, (this, def2));
            }
            Term::Object(pos, Some("$".to_owned() + &name), methods2)
        }
        Syntax::Access(pos, ob, method) => {
            let ob2 = translate(mod_name, index, renames, *ob);
            Term::Access(pos, Box::new(ob2), method)
        }
        Syntax::Update(pos, ob, this, method, def) => {
            let ob2 = translate(mod_name, index, renames, *ob);
            let def2 = translate(&this, index, renames, *def);
            Term::Update(pos, Box::new(ob2), this, method, Box::new(def2))
        }
        Syntax::Operator(pos, op, lhs, rhs) => {
            let lhs2 = translate(mod_name, index, renames, *lhs);
            let rhs2 = translate(mod_name, index, renames, *rhs);
            Term::Operator(pos, op, Box::new(lhs2), Box::new(rhs2))
        }
        Syntax::String(pos, s) => Term::String(pos, s),
        Syntax::Float(pos, f) => Term::Float(pos, f),
    }
}

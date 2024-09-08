use std::{collections::HashMap, io};

use crate::common::*;

pub fn normalize(t: Term) -> Result<Term, Error> {
    let (out, _, _) = go(t, Stack(List::Nil.into()), Env(List::Nil.into()))?;
    Ok(out)
}

fn go(term: Term, s: Stack, e: Env) -> Result<(Term, Stack, Env), Error> {
    let Stack(ref stack) = s;
    let Env(ref env) = e;
    // println!("{}", term.clone().pretty());
    match term {
        Term::Lambda(p, name, body) => match stack.cons() {
            Some(((Term::Object(p2, _, methods), ob_env), rest)) => {
                let env2 = List::Cons(
                    (
                        Term::Object(p2.clone(), Some(name), methods.clone()),
                        ob_env.clone(),
                    ),
                    env.clone(),
                );
                go(*body, Stack(rest.clone()), Env(env2.into()))
            }
            Some((arg, rest)) => go(
                *body,
                Stack(rest.clone()),
                Env(List::Cons(arg.clone(), env.clone()).into()),
            ),
            None => Ok((Term::Lambda(p, name, body), s, e)),
        },
        Term::Ident(p, i, str) => match str.as_str() {
            "#console_write" => match stack.cons() {
                Some(((arg, arg_env), _)) => {
                    let (normal_form, _, _) =
                        go(arg.clone(), Stack(List::Nil.into()), arg_env.clone())?;
                    match normal_form {
                        Term::String(_, s) => println!("{}", s),
                        Term::Int(_, i) => println!("{}", i),
                        Term::Float(_, f) => println!("{}", f),
                        _ => {
                            return Err(Error::Runtime(
                                p,
                                format!("Can't write `{}` to the console", normal_form.pretty()),
                            ))
                        }
                    }
                    Ok((Term::Int(p, 0), Stack(List::Nil.into()), e))
                }
                None => Err(Error::Runtime(
                    p,
                    "`console.write` with no arguments".to_owned(),
                )),
            },
            "#console_read" => {
                let stdin = io::stdin();
                let str = stdin.lines().next().unwrap().unwrap();
                Ok((Term::String(p, str), s, e))
            }
            name => match i {
                0 => match env.cons() {
                    Some(((def, new_env), _)) => go(def.clone(), s, new_env.clone()),
                    None => Err(Error::Runtime(
                        p,
                        format!("Undefined identifier `{}`", name),
                    )),
                },
                i => match env.cons() {
                    Some((_, rest)) => go(Term::Ident(p, i - 1, str), s, Env(rest.clone())),
                    None => Err(Error::Runtime(
                        p,
                        format!("Undefined identifier `{}`", name),
                    )),
                },
            },
        },
        Term::Call(_, foo, bar) => go(
            *foo,
            Stack(List::Cons((*bar, e.clone()), stack.clone()).into()),
            e,
        ),
        Term::Int(_, _) => Ok((term, s, e)),
        Term::LetForce(_, name, val, scope) => {
            let (normal_form, _, nf_env) = go(*val, Stack(List::Nil.into()), e.clone())?;
            let nf = match normal_form {
                Term::Object(p, _, methods) => Term::Object(p, Some(name), methods),
                _ => normal_form,
            };
            go(
                *scope,
                s.clone(),
                Env(List::Cons((nf, nf_env), env.clone()).into()),
            )
        }
        Term::Object(ref p, ref mb_name, ref methods) => {
            let mut methods2 = HashMap::new();
            for (method, (this, def)) in methods.into_iter() {
                methods2.insert(
                    method.clone(),
                    (
                        this.clone(),
                        Term::InEnv(
                            Box::new(def.clone()),
                            Env(List::Cons((term.clone(), e.clone()), env.clone()).into()),
                        ),
                    ),
                );
            }
            Ok((Term::Object(p.clone(), mb_name.clone(), methods2), s, e))
        }
        Term::Access(p, ob, method) => {
            let (normal_form, _, ob_env) = go(*ob, Stack(List::Nil.into()), e.clone())?;
            match normal_form {
                Term::Object(_, _, ref methods) => match methods.get(&method) {
                    Some((_, def)) => go(
                        def.clone(),
                        s,
                        Env(List::Cons((normal_form, ob_env), env.clone()).into()),
                    ),
                    None => Err(Error::Runtime(
                        p,
                        format!(
                            "Unknown object method `{}` on `{}`",
                            method,
                            normal_form.pretty()
                        ),
                    )),
                },
                _ => Err(Error::Runtime(
                    p,
                    format!(
                        "`{}` is not an object, cannot access method `{}`",
                        normal_form.pretty(),
                        method
                    ),
                )),
            }
        }
        Term::Update(p, ob, this, method, def) => {
            let (normal_form, _, _) = go(*ob, Stack(List::Nil.into()), e.clone())?;
            match normal_form {
                Term::Object(_, mb_name, methods) => {
                    let mut methods2 = methods.clone();
                    methods2.insert(method, (this, Term::InEnv(def, e.clone())));
                    Ok((Term::Object(p, mb_name, methods2), s, e.clone()))
                }
                _ => Err(Error::Runtime(
                    p,
                    format!(
                        "`{}` is not an object, cannot update method `{}`",
                        normal_form.pretty(),
                        method
                    ),
                )),
            }
        }
        Term::Operator(p, op, lhs, rhs) => {
            let (lhs_normal_form, _, _) = go(*lhs, Stack(List::Nil.into()), Env(env.clone()))?;
            let (rhs_normal_form, _, _) = go(*rhs, Stack(List::Nil.into()), Env(env.clone()))?;
            match (lhs_normal_form, rhs_normal_form) {
                (Term::Int(_, i), Term::Int(_, j)) => match op.as_str() {
                    "+" => Ok((Term::Int(p, i + j), s, e)),
                    "-" => Ok((Term::Int(p, i - j), s, e)),
                    "*" => Ok((Term::Int(p, i * j), s, e)),
                    "/" => Ok((Term::Int(p, i / j), s, e)),
                    "%" => Ok((Term::Int(p, i % j), s, e)),
                    "==" => Ok((if i == j { true_val(p) } else { false_val(p) }, s, e)),
                    ">" => Ok((if i > j { true_val(p) } else { false_val(p) }, s, e)),
                    "<" => Ok((if i < j { true_val(p) } else { false_val(p) }, s, e)),
                    ">=" => Ok((if i >= j { true_val(p) } else { false_val(p) }, s, e)),
                    "<=" => Ok((if i <= j { true_val(p) } else { false_val(p) }, s, e)),
                    "!=" => Ok((if i != j { true_val(p) } else { false_val(p) }, s, e)),
                    _ => Err(Error::Runtime(
                        p,
                        format!("Unknown operator `{}` for integers", op),
                    )),
                },
                (Term::String(_, a), Term::String(_, b)) => match op.as_str() {
                    "+" => Ok((Term::String(p, a + &b), s, e)),
                    "==" => Ok((if a == b { true_val(p) } else { false_val(p) }, s, e)),
                    "!=" => Ok((if a != b { true_val(p) } else { false_val(p) }, s, e)),
                    _ => Err(Error::Runtime(
                        p,
                        format!("Unknown operator `{}` for strings", op),
                    )),
                },
                (Term::Float(_, a), Term::Float(_, b)) => match op.as_str() {
                    "+" => Ok((Term::Float(p, a + b), s, e)),
                    "-" => Ok((Term::Float(p, a - b), s, e)),
                    "*" => Ok((Term::Float(p, a * b), s, e)),
                    "/" => Ok((Term::Float(p, a / b), s, e)),
                    "%" => Ok((Term::Float(p, a % b), s, e)),
                    "==" => Ok((if a == b { true_val(p) } else { false_val(p) }, s, e)),
                    ">" => Ok((if a > b { true_val(p) } else { false_val(p) }, s, e)),
                    "<" => Ok((if a < b { true_val(p) } else { false_val(p) }, s, e)),
                    ">=" => Ok((if a >= b { true_val(p) } else { false_val(p) }, s, e)),
                    "<=" => Ok((if a <= b { true_val(p) } else { false_val(p) }, s, e)),
                    "!=" => Ok((if a != b { true_val(p) } else { false_val(p) }, s, e)),
                    _ => Err(Error::Runtime(
                        p,
                        format!("Unknown operator `{}` for floats", op),
                    )),
                },
                (l, r) => Err(Error::Runtime(
                    p,
                    format!(
                        "Unknown operator `{}` for `{}` and `{}`",
                        op,
                        l.pretty(),
                        r.pretty()
                    ),
                )),
            }
        }
        Term::String(_, _) => Ok((term, s, e)),
        Term::Float(_, _) => Ok((term, s, e)),
        Term::InEnv(t, new_env) => go(*t, s, new_env),
    }
}

fn true_val(p: Pos) -> Term {
    Term::Object(
        p.clone(),
        Some("true".to_owned()),
        HashMap::from([(
            "case".to_owned(),
            (
                "".to_owned(),
                Term::Lambda(
                    p.clone(),
                    "c".to_owned(),
                    Box::new(Term::Access(
                        p.clone(),
                        Box::new(Term::Ident(p, 0, "c".to_owned())),
                        "True".to_owned(),
                    )),
                ),
            ),
        )]),
    )
}

fn false_val(p: Pos) -> Term {
    Term::Object(
        p.clone(),
        Some("false".to_owned()),
        HashMap::from([(
            "case".to_owned(),
            (
                "".to_owned(),
                Term::Lambda(
                    p.clone(),
                    "c".to_owned(),
                    Box::new(Term::Access(
                        p.clone(),
                        Box::new(Term::Ident(p, 0, "c".to_owned())),
                        "False".to_owned(),
                    )),
                ),
            ),
        )]),
    )
}

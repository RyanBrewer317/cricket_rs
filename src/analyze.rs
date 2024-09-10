use crate::common::*;

#[derive(Clone, Debug)]
pub enum Type {
    Function(Box<Type>, Box<Type>),
    Int,
    Float,
    String,
    Dynamic,
    Object(Vec<(i64, String, Type)>),
    Union(Vec<Type>),
    Effectful(Box<Type>),
}
impl Type {
    fn subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Function(arg, ret), Type::Function(arg2, ret2)) => {
                arg2.subtype(arg) && ret.subtype(ret2) // notice variance
            }
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (_, Type::Dynamic) => true,
            (Type::Object(methods), Type::Object(methods2)) => {
                methods.iter().all(|(_, name, ty)| {
                    methods2
                        .iter()
                        .any(|(_, name2, ty2)| name == name2 && ty.subtype(ty2))
                })
            }
            (Type::Union(ts), t) => ts.iter().all(|t2| t2.subtype(t)),
            (t, Type::Union(ts)) => ts.iter().any(|t2| t.subtype(t2)),
            (Type::Effectful(t1), t2) => t1.subtype(t2),
            (t1, Type::Effectful(t2)) => t1.subtype(t2),
            _ => false,
        }
    }
    fn equiv(&self, other: &Type) -> bool {
        self.subtype(other) && other.subtype(self)
    }
    fn effectful(self: &Self) -> bool {
        match self {
            Type::Effectful(_) => true,
            _ => false,
        }
    }
}

impl Pretty for Type {
    fn pretty(self: &Self) -> String {
        match self {
            Type::Function(arg, ret) => "(".to_owned() + &arg.pretty() + ") -> " + &ret.pretty(),
            Type::Int => "Int".to_owned(),
            Type::Float => "Float".to_owned(),
            Type::String => "String".to_owned(),
            Type::Dynamic => "Dynamic".to_owned(),
            Type::Object(methods) => {
                "{".to_owned()
                    + &methods
                        .iter()
                        .map(|(_i, name, ty)| format!("{}: {}", name, ty.clone().pretty()))
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Type::Union(types) => types
                .iter()
                .map(|ty| ty.clone().pretty())
                .collect::<Vec<String>>()
                .join(" | "),
            Type::Effectful(t) => "!(".to_owned() + &t.pretty() + ")",
        }
    }
}

pub struct Warning(pub Pos, pub String);

pub fn infer(
    term: &Term,
    env: &mut Vec<Type>,
    psi: &mut Vec<Type>,
    warnings: &mut Vec<Warning>,
    i: i64,
    parent_objects: &mut Vec<Term>,
) -> Type {
    match term {
        Term::Lambda(_p, _ident, body) => {
            let arg_ty = psi.pop().unwrap_or(Type::Dynamic);
            env.push(arg_ty.clone());
            let body2 = infer(body, env, psi, warnings, i, parent_objects);
            env.pop();
            psi.push(arg_ty.clone());
            Type::Function(Box::new(arg_ty), Box::new(body2))
        }
        Term::Ident(p, i, name) => match name.as_str() {
            "#console_write" => Type::Function(
                Box::new(Type::Union(vec![Type::Int, Type::Float, Type::String])),
                Box::new(Type::Effectful(Box::new(Type::Int))),
            ),
            "#console_read" => Type::Effectful(Box::new(Type::String)),
            _ => match env.get(env.len() - *i as usize - 1) {
                // Some(Type::This(_i)) => Type::Dynamic, // TODO: implement Abadi-Cardelli IOC Self Types! We need a recursive-type unroll here, using `parent_objects`
                Some(t) => t.clone(),
                None => {
                    // impossible? the earlier translation rewrites unknown identifiers to module method accesses
                    // in which case we ought to panic here. I'll look into this more
                    warnings.push(Warning(p.clone(), format!("Unknown identifier `{}`", name)));
                    Type::Dynamic
                }
            },
        },
        Term::Call(p, foo, bar) => {
            let bar2 = infer(bar, env, psi, warnings, i, parent_objects);
            if let Type::Effectful(_) = &bar2 {
                warnings.push(Warning(
                    p.clone(),
                    format!("Consider moving this side-effectful value into a `let force`"),
                ));
            }
            psi.push(bar2);
            let foo2 = infer(foo, env, psi, warnings, i, parent_objects);
            psi.pop();
            match foo2 {
                Type::Function(a, b) => {
                    check(&bar, &a, env, psi, warnings); // TODO: check that this is necessary
                    *b
                }
                Type::Dynamic => Type::Dynamic,
                t => {
                    warnings.push(Warning(
                        p.clone(),
                        format!("Expected function, got `{}`", t.pretty()),
                    ));
                    Type::Dynamic
                }
            }
        }
        Term::Int(_, _) => Type::Int,
        Term::LetForce(_p, _ident, val, scope) => {
            let val_type = infer(val, env, psi, warnings, i, parent_objects);
            let (val_type2, effectful) = match val_type {
                Type::Effectful(t) => (*t, true),
                t => (t, false),
            };
            env.push(val_type2);
            let scope_type = infer(scope, env, psi, warnings, i, parent_objects);
            env.pop();
            if effectful && !scope_type.effectful() {
                Type::Effectful(Box::new(scope_type))
            } else {
                scope_type
            }
        }
        Term::Object(_p, _mb_name, methods) => {
            let mut methods2 = vec![];
            env.push(Type::Object(
                methods
                    .iter()
                    .map(|(name, _)| (i, name.clone(), Type::Dynamic))
                    .collect(),
            ));
            parent_objects.push(term.clone());
            for (name, (this, def)) in methods {
                let def2 = infer(def, env, psi, &mut vec![], i + 1, parent_objects);
                methods2.push((name.to_string(), this.to_string(), def2));
            }
            env.pop();
            let this_type = Type::Object(
                methods2
                    .into_iter()
                    .map(|(name, _, t)| (i, name, t))
                    .collect(),
            );
            let mut methods3 = vec![];
            env.push(this_type);
            for (name, (this, def)) in methods {
                let def2 = infer(def, env, psi, warnings, i, parent_objects);
                methods3.push((name.to_string(), this.to_string(), def2));
            }
            env.pop();
            parent_objects.pop();
            Type::Object(
                methods3
                    .into_iter()
                    .map(|(name, _, t)| (i, name, t))
                    .collect(),
            )
        }
        Term::Access(p, obj, name) => {
            let obj2 = infer(obj, env, psi, warnings, i, parent_objects);
            match obj2.clone() {
                Type::Object(methods) => methods
                    .iter()
                    .find(|(_, name2, _)| name == name2)
                    .map(|(_, _, t)| t.clone())
                    .unwrap_or_else(|| {
                        warnings.push(Warning(
                            p.clone(),
                            format!("Unknown method `{}` on `{}`", name, obj2.pretty()),
                        ));
                        Type::Dynamic
                    }),
                Type::Dynamic => Type::Dynamic,
                t => {
                    warnings.push(Warning(
                        p.clone(),
                        format!("Expected object, got `{}`", t.pretty()),
                    ));
                    Type::Dynamic
                }
            }
        }
        Term::Update(p, obj, _this, name, val) => {
            let obj2 = infer(obj, env, psi, warnings, i, parent_objects);
            let val2 = infer(val, env, psi, warnings, i, parent_objects);
            match obj2 {
                Type::Object(mut methods) => {
                    methods.push((i, name.to_string(), val2));
                    Type::Object(methods)
                }
                Type::Dynamic => Type::Dynamic,
                t => {
                    warnings.push(Warning(
                        p.clone(),
                        format!("Expected object, got `{}`", t.pretty()),
                    ));
                    Type::Dynamic
                }
            }
        }
        Term::Operator(p, op, lhs, rhs) => {
            let lhs2 = infer(lhs, env, psi, warnings, i, parent_objects);
            let rhs2 = infer(rhs, env, psi, warnings, i, parent_objects);
            match op.as_str() {
                "+" => {
                    if lhs2.subtype(&Type::Int) && rhs2.subtype(&Type::Int) {
                        Type::Int
                    } else if lhs2.subtype(&Type::Float) && rhs2.subtype(&Type::Float) {
                        Type::Float
                    } else if lhs2.subtype(&Type::String) && rhs2.subtype(&Type::String) {
                        Type::String
                    } else if Type::Dynamic.subtype(&lhs2) || Type::Dynamic.subtype(&rhs2) {
                        Type::Dynamic
                    } else {
                        warnings.push(Warning(
                            p.clone(),
                            format!(
                                "Operator `+` doesn't work on values of type `{}` and `{}`",
                                lhs2.pretty(),
                                rhs2.pretty()
                            ),
                        ));
                        Type::Dynamic
                    }
                }
                "-" | "*" | "/" | "%" => {
                    if lhs2.subtype(&Type::Int) && rhs2.subtype(&Type::Int) {
                        Type::Int
                    } else if lhs2.subtype(&Type::Float) && rhs2.subtype(&Type::Float) {
                        Type::Float
                    } else if lhs2.subtype(&Type::String) && rhs2.subtype(&Type::String) {
                        Type::String
                    } else if Type::Dynamic.subtype(&lhs2) || Type::Dynamic.subtype(&rhs2) {
                        Type::Dynamic
                    } else {
                        warnings.push(Warning(
                            p.clone(),
                            format!(
                                "Expected int or float, got `{}` and `{}`",
                                lhs2.pretty(),
                                rhs2.pretty()
                            ),
                        ));
                        Type::Dynamic
                    }
                }
                "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                    if op == "==" || op == "!=" {
                        if !(lhs2.equiv(&rhs2)
                            || Type::Dynamic.subtype(&lhs2)
                            || Type::Dynamic.subtype(&rhs2))
                        {
                            warnings.push(Warning(
                                p.clone(),
                                format!(
                                    "Operator `{}` has mismatched types: `{}` and `{}`",
                                    op,
                                    lhs2.pretty(),
                                    rhs2.pretty()
                                ),
                            ));
                        }
                    }
                    Type::Object(vec![(
                        i,
                        "case".to_string(),
                        Type::Function(
                            Box::new(Type::Object(vec![
                                (i + 1, "True".to_string(), Type::Dynamic),
                                (i + 1, "False".to_string(), Type::Dynamic),
                            ])),
                            Box::new(Type::Dynamic),
                        ),
                    )])
                }
                _ => panic!("Unknown operator `{}`", op),
            }
        }
        Term::String(_p, _s) => Type::String,
        Term::Float(_p, _f) => Type::Float,
        Term::InEnv(x, _) => infer(x, env, psi, warnings, i, parent_objects),
    }
}

fn check(
    term: &Term,
    against: &Type,
    env: &mut Vec<Type>,
    psi: &mut Vec<Type>,
    warnings: &mut Vec<Warning>,
) -> () {
    match term {
        Term::Lambda(_, _, body) => {
            if let Type::Function(a, b) = against {
                env.push(*a.clone());
                check(body, b, env, psi, warnings);
                env.pop();
            }
        }
        _ => {
            let t = infer(term, env, psi, &mut vec![], 0, &mut Vec::new());
            if !t.subtype(against) && !Type::Dynamic.subtype(&t) {
                warnings.push(Warning(
                    term.pos().clone(),
                    format!(
                        "Expected `{}`, but found `{}`",
                        against.clone().pretty(),
                        t.pretty()
                    ),
                ));
            }
        }
    }
}

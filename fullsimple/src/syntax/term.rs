use std::fmt;

use super::{
    pattern::{PTmpTag, Pattern},
    r#type::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub label: String,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub ptag: PTmpTag,
    pub term: Term,
}

impl fmt::Display for Arm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "| {} => {}", self.ptag, self.term)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tag {
    pub ty: Type,
    pub label: String,
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:::{}", self.ty, self.label)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    TmpVar(String),
    Abs(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Unit,
    True,
    False,
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    Record(Vec<Field>),
    Tagging(Tag),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    Plet(Pattern, Box<Term>, Box<Term>),
    Projection(Box<Term>, String),
    Case(Box<Term>, Vec<Arm>),
    Fix(Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::TmpVar(x) => x.to_string(),
                Term::Abs(ty, t) => {
                    if has_arg_after {
                        format!("(\\:{}.{})", ty, print(t, false, false))
                    } else {
                        format!("\\:{}.{}", ty, print(t, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!(
                            "({} {})",
                            print(t1, true, false),
                            print(t2, has_arg_after, true)
                        )
                    } else {
                        format!(
                            "{} {}",
                            print(t1, true, false),
                            print(t2, has_arg_after, true)
                        )
                    }
                }
                Term::Unit => "unit".to_string(),
                Term::True => "true".to_string(),
                Term::False => "false".to_string(),
                Term::Zero => "zero".to_string(),
                Term::Succ(t) => {
                    if matches!(**t, Term::Zero) {
                        format!("succ {}", print(t, false, false))
                    } else {
                        format!("succ ({})", print(t, false, false))
                    }
                }
                Term::Pred(t) => {
                    if matches!(**t, Term::Zero) {
                        format!("pred {}", print(t, false, false))
                    } else {
                        format!("pred ({})", print(t, false, false))
                    }
                }
                Term::IsZero(t) => {
                    if has_arg_after {
                        format!("iszero ({})", print(t, false, false))
                    } else {
                        format!("iszero {}", print(t, false, false))
                    }
                }
                Term::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| {
                            format!("{}={}", field.label, print(&field.term, false, false))
                        })
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Term::Projection(t, label) => {
                    format!("{}.{}", print(t, false, false), label)
                }
                Term::If(t1, t2, t3) => format!(
                    "if {} then {} else {}",
                    print(t1, false, false),
                    print(t2, false, false),
                    if has_arg_after {
                        format!("({})", print(t3, false, false))
                    } else {
                        print(t3, false, false)
                    }
                ),
                Term::Let(t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let 0 = {} in ({})",
                            print(t1, false, false),
                            print(t2, false, false)
                        )
                    } else {
                        format!(
                            "let 0 = {} in {}",
                            print(t1, false, false),
                            print(t2, false, false)
                        )
                    }
                }
                Term::Plet(p, t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let {} = {} in ({})",
                            p,
                            print(t1, false, false),
                            print(t2, false, false)
                        )
                    } else {
                        format!(
                            "let {} = {} in {}",
                            p,
                            print(t1, false, false),
                            print(t2, false, false)
                        )
                    }
                }
                Term::Tagging(tag) => tag.to_string(),
                Term::Case(t, bs) => {
                    format!("case {} of {}", t, {
                        bs.iter()
                            .map(|b| b.to_string())
                            .fold("".to_string(), |acc, x| acc + &x)
                    })
                }
                Term::Fix(t) => {
                    if has_arg_after {
                        format!("fix ({})", print(t, false, false))
                    } else {
                        format!("fix {}", print(t, false, false))
                    }
                }
            }
        }
        write!(f, "{}", print(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        match self {
            Term::Unit | Term::True | Term::False | Term::Zero | Term::Abs(_, _) => true,
            Term::Succ(v1) if v1.isval() => true,
            Term::Record(fields) => fields.iter().all(|field| field.term.isval()),
            Term::Tagging(_tag) => true,
            Term::App(t1, t2) => {
                fn is_vtag(t: &Term) -> bool {
                    match t {
                        Term::Tagging(_) => true,
                        Term::App(t1, t2) => is_vtag(t1) && t2.isval(),
                        _ => false,
                    }
                }
                is_vtag(t1) && t2.isval()
            }
            _ => false,
        }
    }
}

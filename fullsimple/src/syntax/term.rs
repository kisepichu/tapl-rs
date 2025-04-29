use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone)]
pub struct Field {
    pub label: String,
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    Var(usize),
    TmpVar(String),
    Abs(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Unit,
    True,
    False,
    Record(Vec<Field>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    Projection(Box<Term>, String),
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Var(x), Term::Var(y)) => x == y,
            (Term::TmpVar(x), Term::TmpVar(y)) => x == y,
            (Term::Abs(ty1, t1), Term::Abs(ty2, t2)) => ty1 == ty2 && t1 == t2,
            (Term::App(t1a, t1b), Term::App(t2a, t2b)) => t1a == t2a && t1b == t2b,
            (Term::Unit, Term::Unit) => true,
            (Term::True, Term::True) => true,
            (Term::False, Term::False) => true,
            (Term::Record(fields1), Term::Record(fields2)) => {
                if fields1.len() == fields2.len() {
                    let mut fields1 = fields1.clone();
                    let mut fields2 = fields2.clone();
                    fields1.sort_by(|l, r| l.label.cmp(&r.label));
                    fields2.sort_by(|l, r| l.label.cmp(&r.label));
                    fields1
                        .iter()
                        .zip(fields2.iter())
                        .all(|(e1, e2)| e1.label == e2.label && e1.term == e2.term)
                } else {
                    false
                }
            }
            (Term::If(t1a, t1b, t1c), Term::If(t2a, t2b, t2c)) => {
                t1a == t2a && t1b == t2b && t1c == t2c
            }
            (Term::Let(t1a, t1b), Term::Let(t2a, t2b)) => t1a == t2a && t1b == t2b,
            (Term::Projection(t1, l1), Term::Projection(t2, l2)) => l1 == l2 && t1 == t2,
            _ => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::TmpVar(x) => x.to_string(),
                Term::Abs(ty, t) => {
                    if has_arg_after {
                        format!("(\\:{}.{})", ty, p(t, false, false))
                    } else {
                        format!("\\:{}.{}", ty, p(t, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!("({} {})", p(t1, true, false), p(t2, has_arg_after, true))
                    } else {
                        format!("{} {}", p(t1, true, false), p(t2, has_arg_after, true))
                    }
                }
                Term::Unit => "unit".to_string(),
                Term::True => "true".to_string(),
                Term::False => "false".to_string(),
                Term::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}={}", field.label, p(&field.term, false, false)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Term::Projection(t, label) => {
                    format!("{}.{}", p(t, false, false), label)
                }
                Term::If(t1, t2, t3) => format!(
                    "if {} then {} else {}",
                    p(t1, false, false),
                    p(t2, false, false),
                    if has_arg_after {
                        format!("({})", p(t3, false, false))
                    } else {
                        p(t3, false, false)
                    }
                ),
                Term::Let(t1, t2) => {
                    if has_arg_after {
                        format!(
                            "let 0 = {} in ({})",
                            p(t1, false, false),
                            p(t2, false, false)
                        )
                    } else {
                        format!("let 0 = {} in {}", p(t1, false, false), p(t2, false, false))
                    }
                }
            }
        }
        write!(f, "{}", p(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        match self {
            Term::Unit | Term::True | Term::False | Term::Abs(_, _) => true,
            Term::Record(fields) => fields.iter().all(|field| field.term.isval()),
            _ => false,
        }
    }
}

use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    TmpVar(String),
    Abs(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Unit,
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::TmpVar(x) => format!("{}", x),
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
        matches!(
            self,
            Term::Abs(_, _) | Term::Unit | Term::True | Term::False
        )
    }
}

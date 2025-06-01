use std::fmt;

use super::r#type::Type;
use crate::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    Abs(Type, Box<Spanned<Term>>),
    App(Box<Spanned<Term>>, Box<Spanned<Term>>),
    True,
    False,
    If(Box<Spanned<Term>>, Box<Spanned<Term>>, Box<Spanned<Term>>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::Abs(ty, t) => {
                    if has_arg_after {
                        format!("(\\:{}.{})", ty, p(&t.v, false, false))
                    } else {
                        format!("\\:{}.{}", ty, p(&t.v, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!(
                            "({} {})",
                            p(&t1.v, true, false),
                            p(&t2.v, has_arg_after, true)
                        )
                    } else {
                        format!(
                            "{} {}",
                            p(&t1.v, true, false),
                            p(&t2.v, has_arg_after, true)
                        )
                    }
                }
                Term::True => "true".to_string(),
                Term::False => "false".to_string(),
                Term::If(t1, t2, t3) => format!(
                    "if {} then {} else {}",
                    p(&t1.v, true, false),
                    p(&t2.v, true, false),
                    p(&t3.v, true, false)
                ),
            }
        }
        write!(f, "{}", p(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        matches!(self, Term::Abs(_, _) | Term::True | Term::False)
    }
}

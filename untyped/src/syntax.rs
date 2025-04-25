use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(term: &Term, has_arg_after: bool, is_app_right: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::Abs(t) => {
                    if has_arg_after {
                        format!("(\\{})", p(t, false, false))
                    } else {
                        format!("\\{}", p(t, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_app_right {
                        format!("({} {})", p(t1, true, false), p(t2, has_arg_after, true))
                    } else {
                        format!("{} {}", p(t1, true, false), p(t2, has_arg_after, true))
                    }
                }
            }
        }
        write!(f, "{}", p(self, false, false))
    }
}

impl Term {
    pub fn isval(&self) -> bool {
        matches!(self, Term::Abs(_))
    }
}

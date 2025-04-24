#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn _print_full(&self) -> String {
        match self {
            Term::Var(x) => format!("{}", x),
            Term::Abs(t) => format!("(\\{})", t._print_full()),
            Term::App(t1, t2) => format!("({} {})", t1._print_full(), t2._print_full()),
        }
    }

    pub fn print_tm(&self) -> String {
        fn p(term: &Term, has_arg_right: bool, is_arg_app: bool) -> String {
            match term {
                Term::Var(x) => format!("{}", x),
                Term::Abs(t) => {
                    if has_arg_right {
                        format!("(\\{})", p(t, false, false))
                    } else {
                        format!("\\{}", p(t, false, false))
                    }
                }
                Term::App(t1, t2) => {
                    if is_arg_app {
                        format!("({} {})", p(t1, true, false), p(t2, has_arg_right, true))
                    } else {
                        format!("{} {}", p(t1, true, false), p(t2, has_arg_right, true))
                    }
                }
            }
        }
        p(self, false, false)
    }

    pub fn isval(&self) -> bool {
        matches!(self, Term::Abs(_))
    }
}

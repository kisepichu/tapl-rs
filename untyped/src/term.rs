#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

pub fn _print_tm_full(term: &Term) -> String {
    match term {
        Term::Var(x) => format!("{}", x),
        Term::Abs(t) => format!("(\\{})", _print_tm_full(t)),
        Term::App(t1, t2) => format!("({} {})", _print_tm_full(t1), _print_tm_full(t2)),
    }
}

pub fn print_tm(term: &Term) -> String {
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
    p(term, false, false)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

pub fn print_tm(term: &Term) -> String {
    match term {
        Term::Var(x) => format!("{}", x),
        Term::Abs(t) => format!("(\\{})", print_tm(t)),
        Term::App(t1, t2) => format!("({} {})", print_tm(t1), print_tm(t2)),
    }
}

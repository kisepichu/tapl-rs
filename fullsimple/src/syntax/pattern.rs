use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub label: String,
    pub pat: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PTmpTag {
    pub ty: Type,
    pub label: String,
    pub nargs: Result<usize, Vec<String>>,
}

impl fmt::Display for PTmpTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ps_str = match &self.nargs {
            Ok(n) => (0..*n)
                .rev()
                .map(|i| format!(" {}", i))
                .collect::<Vec<_>>()
                .join(""),
            Err(args) => args
                .iter()
                .map(|arg| format!(" {}", arg))
                .collect::<Vec<_>>()
                .join(""),
        };
        write!(f, "{}:::{}{}", self.ty, self.label, ps_str)
    }
}

impl PTmpTag {
    pub fn len(&self) -> usize {
        match &self.nargs {
            Ok(n) => *n,
            Err(args) => args.len(),
        }
    }
    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String, Type),
    Record(Vec<PatField>),
    TmpTagging(PTmpTag),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(pat: &Pattern) -> String {
            match pat {
                Pattern::Var(x, ty) => format!("{}:{}", x, ty),
                Pattern::Record(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}={}", field.label, p(&field.pat)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Pattern::TmpTagging(ptag) => ptag.to_string(),
            }
        }
        write!(f, "{}", p(self))
    }
}

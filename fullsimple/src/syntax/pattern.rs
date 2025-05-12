use std::fmt;

use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub label: String,
    pub pat: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PTag {
    pub ty: Type,
    pub label: String,
    pub args: Vec<String>,
}

impl fmt::Display for PTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ps_str = self
            .args
            .iter()
            .map(|p| format!(" {}", p))
            .collect::<Vec<_>>()
            .join("");
        write!(f, "{}:::{}{}", self.ty, self.label, ps_str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String, Type),
    Record(Vec<PatField>),
    Tagging(PTag),
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
                Pattern::Tagging(ptag) => ptag.to_string(),
            }
        }
        write!(f, "{}", p(self))
    }
}

// use std::fmt;

// #[derive(Debug, Clone, PartialEq)]
// pub struct PatField {
//     pub label: String,
//     pub pat: Pattern,
// }
// #[derive(Debug, Clone, PartialEq)]
// pub enum Pattern {
//     Var(usize),
//     TmpVar(String),
//     PatRecord(Vec<PatField>),
// }

// impl fmt::Display for Pattern {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         fn p(pat: &Pattern) -> String {
//             match pat {
//                 Pattern::Var(x) => format!("{}", x),
//                 Pattern::TmpVar(x) => x.to_string(),
//                 Pattern::PatRecord(fields) => {
//                     let fields_str: Vec<String> = fields
//                         .iter()
//                         .map(|field| format!("{}={}", field.label, p(&field.pat)))
//                         .collect();
//                     format!("{{{}}}", fields_str.join(", "))
//                 }
//             }
//         }
//         write!(f, "{}", p(self))
//     }
// }

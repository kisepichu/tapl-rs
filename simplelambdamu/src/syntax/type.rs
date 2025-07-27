use std::fmt;

use crate::span::Spanned;
use rstest::rstest;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Arr(Box<Spanned<Type>>, Box<Spanned<Type>>),
    Bot,
    TyVar(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn p(ty: &Type, is_left_arr: bool) -> String {
            match ty {
                Type::Arr(t1, t2) => {
                    if is_left_arr {
                        format!("({}->{})", p(&t1.v, true), p(&t2.v, false))
                    } else {
                        format!("{}->{}", p(&t1.v, true), p(&t2.v, false))
                    }
                }
                Type::Bot => "Bot".to_string(),
                Type::TyVar(x) => x.to_string(),
            }
        }
        write!(f, "{}", p(self, false))
    }
}

// Helper function for creating spanned types in tests
#[cfg(test)]
fn spanned_type(ty: Type) -> Spanned<Type> {
    Spanned {
        v: ty,
        start: 0,
        line: 1,
        column: 1,
    }
}

#[rstest]
#[case(Type::TyVar("Bool".to_string()), r"Bool")]
#[case(
    Type::Arr(Box::new(spanned_type(Type::TyVar("A".to_string()))), Box::new(spanned_type(Type::TyVar("B".to_string())))),
    r"A->B"
)]
#[case(
    Type::Arr(
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        ))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
            Box::new(spanned_type(Type::TyVar("Bool".to_string())))
        )))
    ),
    r"(Bool->Bool)->Bool->Bool"
)]
#[case(
    Type::Arr(
        Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
        Box::new(spanned_type(Type::Arr(
            Box::new(spanned_type(Type::Arr(
                Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
                Box::new(spanned_type(Type::Arr(
                    Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
                    Box::new(spanned_type(Type::TyVar("Bool".to_string())))
                ))),
            ))),
            Box::new(spanned_type(Type::Arr(
                Box::new(spanned_type(Type::TyVar("Bool".to_string()))),
                Box::new(spanned_type(Type::TyVar("Bool".to_string())))
            )))
        )))
    ),
    r"Bool->(Bool->Bool->Bool)->Bool->Bool"
)]
fn test_display(#[case] ty: Type, #[case] expected: &str) {
    assert_eq!(ty.to_string(), expected);
}

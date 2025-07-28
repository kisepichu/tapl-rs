use std::fmt;

use crate::span::{Spanned, dummy_spanned};
use rstest::rstest;

#[derive(Debug, Clone, PartialEq)]
pub struct TyField {
    pub label: String,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Arr(Box<Spanned<Type>>, Box<Spanned<Type>>),
    Unit,
    Bool,
    Nat,
    TyVar(String),
    TyRecord(Vec<TyField>),
    TyTagging(Vec<TyField>),
    TySelf,
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
                Type::Unit => "Unit".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::Nat => "Nat".to_string(),
                Type::TyVar(x) => x.to_string(),
                Type::TyRecord(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}:{}", field.label, p(&field.ty.v, false)))
                        .collect();
                    format!("{{{}}}", fields_str.join(", "))
                }
                Type::TyTagging(fields) => {
                    let fields_str: Vec<String> = fields
                        .iter()
                        .map(|field| format!("{}:{}", field.label, p(&field.ty.v, false)))
                        .collect();
                    format!("<{}>", fields_str.join(", "))
                }
                Type::TySelf => "Self".to_string(),
            }
        }
        write!(f, "{}", p(self, false))
    }
}

impl Type {
    pub fn subst_name(&self, type_name: &str, ty2: &Type) -> Type {
        match self {
            Type::TyVar(s) => {
                if type_name == s {
                    ty2.clone()
                } else {
                    self.clone()
                }
            }
            Type::TyRecord(tyfs) => Type::TyRecord(
                tyfs.iter()
                    .map(|tyf| TyField {
                        label: tyf.label.clone(),
                        ty: dummy_spanned(tyf.ty.v.subst_name(type_name, ty2)),
                    })
                    .collect::<Vec<_>>(),
            ),
            Type::TyTagging(tyfs) => Type::TyTagging(
                tyfs.iter()
                    .map(|tyf| TyField {
                        label: tyf.label.clone(),
                        ty: dummy_spanned(tyf.ty.v.subst_name(type_name, ty2)),
                    })
                    .collect::<Vec<_>>(),
            ),
            Type::Arr(ty1, ty2_) => Type::Arr(
                Box::new(dummy_spanned(ty1.v.subst_name(type_name, ty2))),
                Box::new(dummy_spanned(ty2_.v.subst_name(type_name, ty2))),
            ),
            Type::Nat => Type::Nat,
            Type::Bool => Type::Bool,
            Type::Unit => Type::Unit,
            Type::TySelf => Type::TySelf,
        }
    }
}

#[rstest]
#[case(Type::Unit, r"Unit")]
#[case(Type::Bool, r"Bool")]
#[case(
    Type::Arr(
        Box::new(dummy_spanned(Type::Bool)),
        Box::new(dummy_spanned(Type::Bool))
    ),
    r"Bool->Bool"
)]
#[case(
    Type::Arr(
        Box::new(dummy_spanned(Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Bool))
        ))),
        Box::new(dummy_spanned(Type::Arr(
            Box::new(dummy_spanned(Type::Bool)),
            Box::new(dummy_spanned(Type::Bool))
        )))
    ),
    r"(Bool->Bool)->Bool->Bool"
)]
#[case(
    Type::Arr(
        Box::new(dummy_spanned(Type::Bool)),
        Box::new(dummy_spanned(Type::Arr(
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Arr(
                    Box::new(dummy_spanned(Type::Unit)),
                    Box::new(dummy_spanned(Type::Bool))
                ))),
            ))),
            Box::new(dummy_spanned(Type::Arr(
                Box::new(dummy_spanned(Type::Bool)),
                Box::new(dummy_spanned(Type::Bool))
            )))
        )))
    ),
    r"Bool->(Bool->Unit->Bool)->Bool->Bool"
)]
fn test_display(#[case] ty: Type, #[case] expected: &str) {
    assert_eq!(ty.to_string(), expected);
}

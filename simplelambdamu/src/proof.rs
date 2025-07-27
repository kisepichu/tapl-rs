use crate::{
    span::{ErrorWithPos, Spanned},
    syntax::{context::Context, term::Term, r#type::Type},
    typing::type_of,
};

fn indented(indent: usize, s: &str) -> String {
    let indent_str = " ".repeat(indent * 2);
    s.lines()
        .map(|line| format!("{}{}", indent_str, line))
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

fn with_comma(s: &str) -> String {
    // if last character is "\n", replace it with ",\n"
    if s.ends_with('\n') {
        s.trim_end_matches('\n').to_string() + ",\n"
    } else {
        s.to_string() + ",\n"
    }
}

fn type_to_formula(ty: &Type) -> String {
    fn p(ty: &Type, is_left_arr: bool) -> String {
        match ty {
            Type::Arr(t1, t2) => {
                if is_left_arr {
                    format!("({}->{})", p(&t1.v, true), p(&t2.v, false))
                } else {
                    format!("{}->{}", p(&t1.v, true), p(&t2.v, false))
                }
            }
            Type::Bot => "bot".to_string(),
            Type::TyVar(x) => x.to_string(),
        }
    }
    p(ty, false).to_string()
}

pub fn typst_proof(ctx: &Context, t: &Spanned<Term>) -> Result<String, ErrorWithPos> {
    let mut result = r#"#import "@preview/curryst:0.5.1": rule, prooftree

#prooftree(
"#
    .to_string();

    fn walk(ctx: &Context, t: &Spanned<Term>, d: usize) -> Result<String, ErrorWithPos> {
        match &t.v {
            Term::Var(x) => match ctx.get(*x) {
                Some(ty) => Ok(indented(d, &format!("$[{}]^{}$", type_to_formula(ty), x))),
                None => Err(ErrorWithPos {
                    message: format!("type check failed: {}\n: unbound variable {}", t.v, x),
                    level: 100,
                    kind: None,
                    line: t.line,
                    column: t.column,
                }),
            },
            Term::TmpVar(s) => Err(ErrorWithPos {
                message: format!("type check failed: {}\n: unbound variable {}", t.v, s),
                level: 100,
                kind: None,
                line: t.line,
                column: t.column,
            }),
            // ->_I
            Term::Abs(ty, t2) => {
                let ctx = ctx.clone().push(ty.clone());
                let ty2 = type_of(&ctx, t2)?;
                let pf2 = walk(&ctx, t2, d + 1)?;
                println!("t2= {}", t2.v);
                let mut result = indented(d, "rule(");
                result += &indented(
                    d + 1,
                    &format!("name: $scripts(->)_\"I\", {}$,", "\"todo\""),
                );
                result += &indented(
                    d + 1,
                    &format!(
                        "${}$,",
                        type_to_formula(&Type::Arr(
                            Box::new(Spanned {
                                v: ty.clone(),
                                start: t.start,
                                line: t.line,
                                column: t.column,
                            }),
                            Box::new(Spanned {
                                v: ty2.clone(),
                                start: t2.start,
                                line: t2.line,
                                column: t2.column,
                            }),
                        ))
                    ),
                );
                result += &with_comma(&pf2);
                result += &indented(d, ")");
                Ok(result)
            }
            // bot_C
            Term::MAbs(ty, t2) => {
                let ctx = ctx.clone().push(ty.clone());
                let ty2: Type = type_of(&ctx, t2)?;
                if ty2 != Type::Bot {
                    return Err(ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n: expected bottom type, found {}",
                            t.v, ty2
                        ),
                        level: 100,
                        kind: None,
                        line: t2.line,
                        column: t2.column,
                    });
                }
                if let Type::Arr(ty1, tyb) = ty {
                    if tyb.v != Type::Bot {
                        return Err(ErrorWithPos {
                            message: format!(
                                "type check failed: {}\n: expected negative type, found {}",
                                t.v, tyb.v
                            ),
                            level: 100,
                            kind: None,
                            line: t.line,
                            column: t.column,
                        });
                    }
                    let pf2 = walk(&ctx, t2, d + 1)?;
                    let mut result = indented(d, "rule(");
                    result += &indented(d + 1, &format!("name: $bot_\"C\", {}$,", "\"todo\""));
                    result += &indented(d + 1, &format!("${}$,", type_to_formula(&ty1.v)));
                    result += &with_comma(&pf2);
                    result += &indented(d, ")");
                    Ok(result)
                } else {
                    Err(ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n: expected negative type, found {}",
                            t.v, ty
                        ),
                        level: 100,
                        kind: None,
                        line: t.line,
                        column: t.column,
                    })
                }
            }
            // ->_E
            Term::App(t1, t2) => {
                let ty1 = type_of(ctx, t1)?;
                let pf1 = walk(ctx, t1, d + 1)?;
                let pf2 = walk(ctx, t2, d + 1)?;
                match ty1 {
                    Type::Arr(_ty11, ty12) => {
                        let mut result = indented(d, "rule(");
                        result += &indented(d + 1, "name: $scripts(->)_\"E\"$,");
                        result += &indented(d + 1, &format!("${}$,\n", type_to_formula(&ty12.v)));
                        result += &with_comma(&pf1);
                        result += &with_comma(&pf2);
                        result += &indented(d, ")");
                        Ok(result)
                    }
                    _ => Err(ErrorWithPos {
                        message: format!(
                            "type check failed: {}\n  expected arrow type, but found {}: {}",
                            t.v, t1.v, ty1
                        ),
                        level: 100,
                        kind: None,
                        line: t1.line,
                        column: t1.column,
                    }),
                }
            }
        }
    }

    result += &walk(ctx, t, 1)?;
    result += ")\n";
    Ok(result)
}

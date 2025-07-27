use crate::{
    span::{ErrorWithPos, Spanned},
    syntax::{
        context::Context,
        term::{Info, Term},
        r#type::Type,
    },
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

    fn walk(
        ctx: &Context,
        t: &Spanned<Term>,
        d: usize,
        assumption_count: usize,
    ) -> Result<(String, usize), ErrorWithPos> {
        match &t.v {
            Term::Var(x, _info) => match ctx.get(*x) {
                Some(ty) => {
                    if let Some(a_info) = ctx.get_info(*x) {
                        Ok((
                            indented(
                                d,
                                &format!("$[{}]^{}$", type_to_formula(ty), a_info.assumption_num),
                            ),
                            assumption_count,
                        ))
                    } else {
                        Ok((
                            indented(d, &format!("${}$", type_to_formula(ty))),
                            assumption_count,
                        ))
                    }
                }
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
            Term::Abs(ty, t2, info) => {
                let info = Info {
                    name: info.name.clone(),
                    assumption_num: assumption_count,
                };
                let ctx = ctx.clone().push(ty.clone(), info.clone());
                let ty2 = type_of(&ctx, t2)?;
                let (pf2, assumption_count) = walk(&ctx, t2, d + 1, assumption_count + 1)?;
                println!("t2= {}", t2.v);
                let mut result = indented(d, "rule(");
                result += &indented(
                    d + 1,
                    &format!("name: $scripts(->)_\"I\", {}$,", info.assumption_num),
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
                Ok((result, assumption_count))
            }
            // bot_C
            Term::MAbs(ty, t2, info) => {
                let info = Info {
                    name: info.name.clone(),
                    assumption_num: assumption_count,
                };
                let ctx = ctx.clone().push(ty.clone(), info.clone());
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
                    let (pf2, assumption_count) = walk(&ctx, t2, d + 1, assumption_count + 1)?;
                    let mut result = indented(d, "rule(");
                    result += &indented(
                        d + 1,
                        &format!("name: $bot_\"C\", {}$,", info.assumption_num),
                    );
                    result += &indented(d + 1, &format!("${}$,", type_to_formula(&ty1.v)));
                    result += &with_comma(&pf2);
                    result += &indented(d, ")");
                    Ok((result, assumption_count))
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
                let (pf1, assumption_count) = walk(ctx, t1, d + 1, assumption_count)?;
                let (pf2, assumption_count) = walk(ctx, t2, d + 1, assumption_count)?;
                match ty1 {
                    Type::Arr(_ty11, ty12) => {
                        let mut result = indented(d, "rule(");
                        result += &indented(d + 1, "name: $scripts(->)_\"E\"$,");
                        result += &indented(d + 1, &format!("${}$,\n", type_to_formula(&ty12.v)));
                        result += &with_comma(&pf1);
                        result += &with_comma(&pf2);
                        result += &indented(d, ")");
                        Ok((result, assumption_count))
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

    let (r, _assumption_count) = &walk(ctx, t, 1, 1)?;
    result += r;
    result += ")\n";
    Ok(result)
}

// use typst::...

pub fn render_typst_to_svg(input: &str) -> String {
    use std::path::PathBuf;
    use typst::compile;
    use typst_library::layout::PagedDocument;
    use typst_library::{Library, World, text::FontBook};
    use typst_svg::svg;
    use typst_syntax::{FileId, Source, VirtualPath};
    use typst_utils::LazyHash;

    // 簡単なWorldの実装
    struct SimpleWorld {
        source: Source,
        library: Library,
        book: FontBook,
    }

    impl World for SimpleWorld {
        fn library(&self) -> &LazyHash<Library> {
            // 簡略化のため、デフォルトのライブラリを使用
            static LIBRARY: std::sync::OnceLock<LazyHash<Library>> = std::sync::OnceLock::new();
            LIBRARY.get_or_init(|| LazyHash::new(Library::default()))
        }

        fn book(&self) -> &LazyHash<FontBook> {
            static BOOK: std::sync::OnceLock<LazyHash<FontBook>> = std::sync::OnceLock::new();
            BOOK.get_or_init(|| LazyHash::new(FontBook::new()))
        }

        fn main(&self) -> FileId {
            FileId::new(None, VirtualPath::new("main.typ"))
        }

        fn source(&self, _id: FileId) -> typst_library::diag::FileResult<Source> {
            Ok(self.source.clone())
        }

        fn file(
            &self,
            _id: FileId,
        ) -> typst_library::diag::FileResult<typst_library::foundations::Bytes> {
            Err(typst_library::diag::FileError::NotFound(PathBuf::new()))
        }

        fn font(&self, _index: usize) -> Option<typst_library::text::Font> {
            None
        }

        fn today(&self, _offset: Option<i64>) -> Option<typst_library::foundations::Datetime> {
            None
        }
    }

    // Typstコードをコンパイル
    let source = Source::detached(input);
    let world = SimpleWorld {
        source,
        library: Library::default(),
        book: FontBook::new(),
    };

    let warned = compile::<PagedDocument>(&world);
    match warned.output {
        Ok(document) => {
            // ドキュメントをSVGに変換
            if document.pages.is_empty() {
                format!(
                    r#"<svg width="800" height="600" xmlns="http://www.w3.org/2000/svg">
  <rect width="100%" height="100%" fill="white"/>
  <text x="20" y="40" fill="blue" font-family="monospace" font-size="14">No content to render</text>
</svg>"#
                )
            } else {
                svg(&document.pages[0])
            }
        }
        Err(errors) => {
            // エラーメッセージをSVG形式で返す
            let error_text = errors
                .iter()
                .map(|e| e.message.as_str())
                .collect::<Vec<_>>()
                .join("\n");
            format!(
                r#"<svg width="800" height="600" xmlns="http://www.w3.org/2000/svg">
  <rect width="100%" height="100%" fill="white"/>
  <text x="20" y="40" fill="red" font-family="monospace" font-size="14">Typst Compilation Error:</text>
  <text x="20" y="70" fill="red" font-family="monospace" font-size="12">{}</text>
</svg>"#,
                error_text.replace("<", "&lt;").replace(">", "&gt;")
            )
        }
    }
}

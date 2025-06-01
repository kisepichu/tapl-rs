use simplebool::{parser, syntax::context::Context, typing::type_of_spanned};

fn test_parse_error() {
    println!("=== Parse Error Test ===");
    let input = r#"( \ : Bool . 0"#; // 閉じ括弧がない
    match parser::parse_spanned_and_render_err(input) {
        Ok(t) => println!("Unexpected success: {:?}", t),
        Err((error, position)) => {
            println!("{}", position);
            println!("{}", error);
        }
    }
    println!();
}

fn test_type_error() {
    println!("=== Type Error Test ===");
    let input = r#"if 42 then true else false"#; // 42は変数42だが、未定義
    match parser::parse_spanned_and_render_err(input) {
        Ok(t) => {
            let ctx = Context::default();
            match type_of_spanned(&ctx, &t) {
                Ok(ty) => println!("Unexpected success: {:?}", ty),
                Err(error) => {
                    println!(
                        "{}",
                        parser::display_position(input, error.line, error.column)
                    );
                    println!("{}", error);
                }
            }
        }
        Err((error, position)) => {
            println!("{}", position);
            println!("{}", error);
        }
    }
    println!();
}

fn test_type_mismatch_error() {
    println!("=== Type Mismatch Error Test ===");
    let input = r#"if true then false else 0"#; // elseブランチで変数0(未定義)
    match parser::parse_spanned_and_render_err(input) {
        Ok(t) => {
            let ctx = Context::default();
            match type_of_spanned(&ctx, &t) {
                Ok(ty) => println!("Unexpected success: {:?}", ty),
                Err(error) => {
                    println!(
                        "{}",
                        parser::display_position(input, error.line, error.column)
                    );
                    println!("{}", error);
                }
            }
        }
        Err((error, position)) => {
            println!("{}", position);
            println!("{}", error);
        }
    }
    println!();
}

fn main() {
    test_parse_error();
    test_type_error();
    test_type_mismatch_error();
}

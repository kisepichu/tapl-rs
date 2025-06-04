use simplebool::{
    parser::parse_spanned_and_render_err, syntax::context::Context, typing::type_of_spanned,
};

#[test]
fn test_unknown_type_error() {
    // 未定義の型 Unit を使った場合
    let input = r"\:Unit.0";
    match parse_spanned_and_render_err(input) {
        Err((error, position)) => {
            println!("Parse error: {}", error);
            println!("Position: {}", position);
            assert!(error.contains("Unknown type: Unit"));
            assert!(position.contains("^"));
        }
        Ok(_) => panic!("Expected parse error for unknown type"),
    }
}

#[test]
fn test_unknown_type_error_position() {
    // 未定義の型 Nat を使った場合の位置情報テスト
    let input = r"\:Nat.true";
    match parse_spanned_and_render_err(input) {
        Err((error, position)) => {
            println!("Input: {}", input);
            println!("Error: {}", error);
            println!("Position: {}", position);
            assert!(error.contains("Unknown type: Nat"));
            // エラー位置が Nat の位置 (column 3) であることを確認
            let lines: Vec<&str> = position.lines().collect();
            if lines.len() >= 3 {
                let cursor_line = lines[2];
                let cursor_pos = cursor_line.find('^').unwrap_or(0);
                assert_eq!(cursor_pos, 6); // 0-indexed, so column 3 is index 2
            } else {
                panic!("Position information is not formatted as expected");
            }
        }
        Ok(_) => panic!("Expected parse error for unknown type"),
    }
}

#[test]
fn test_multiple_unknown_types() {
    // 複数の未定義型を含む場合、最初のエラーが報告される
    let input = r"\:String.\:Int.0";
    match parse_spanned_and_render_err(input) {
        Err((error, position)) => {
            println!("Input: {}", input);
            println!("Error: {}", error);
            println!("Position: {}", position);
            assert!(error.contains("Unknown type: String"));
            // 最初の未定義型 String でエラーが出ることを確認
        }
        Ok(_) => panic!("Expected parse error for unknown type"),
    }
}

#[test]
fn test_unknown_type_in_arrow_type() {
    // 矢印型の中に未定義型がある場合
    let input = r"\:Nat->Bool.0";
    match parse_spanned_and_render_err(input) {
        Err((error, position)) => {
            println!("Input: {}", input);
            println!("Error: {}", error);
            println!("Position: {}", position);
            assert!(error.contains("Unknown type: Nat"));
            // エラー位置が Nat の位置であることを確認
        }
        Ok(_) => panic!("Expected parse error for unknown type"),
    }
}

#[test]
fn test_valid_bool_type() {
    // Bool 型は正常にパースされることを確認
    let input = r"\:Bool.true";
    match parse_spanned_and_render_err(input) {
        Ok(ast) => {
            let ctx = Context::default();
            let result = type_of_spanned(&ctx, &ast);
            assert!(result.is_ok());
        }
        Err((error, _)) => panic!("Unexpected parse error: {}", error),
    }
}

#[test]
fn test_complex_arrow_type() {
    // 複雑な矢印型が正常に処理されることを確認
    let input = r"\:Bool->Bool->Bool.0";
    match parse_spanned_and_render_err(input) {
        Ok(ast) => {
            let ctx = Context::default();
            let result = type_of_spanned(&ctx, &ast);
            assert!(result.is_ok());
            println!("Parsed successfully: {}", ast.v);
        }
        Err((error, _)) => panic!("Unexpected parse error: {}", error),
    }
}

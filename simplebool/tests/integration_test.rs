use simplebool::{
    eval::eval,
    parser::{display_position, parse_spanned},
    syntax::context::Context,
    typing::type_of_spanned,
};

/// パーサー、型チェック、評価を順に実行するヘルパー関数
fn run_full_pipeline(input: &str) -> Result<String, String> {
    // 1. パーサー
    let ast = parse_spanned(input).map_err(|e| format!("Parse error: {}", e))?;

    // 2. 型チェック
    let ctx = Context::default();
    let _ty = type_of_spanned(&ctx, &ast).map_err(|e| {
        format!(
            "Type error at line {}, column {}: {}",
            e.line, e.column, e.message
        )
    })?;

    // 3. 評価
    let result = eval(&ast.v).map_err(|e| format!("Eval error: {}", e))?;

    Ok(format!("{}", result))
}

#[test]
fn test_simple_values() {
    // 真理値
    assert_eq!(run_full_pipeline("true").unwrap(), "true");
    assert_eq!(run_full_pipeline("false").unwrap(), "false");
}

#[test]
fn test_identity_function() {
    // 恒等関数の適用
    let input = r"(\:Bool.0) true";
    let result = run_full_pipeline(input).unwrap();
    assert_eq!(result, "true");
}

#[test]
fn test_if_expression() {
    // if式
    assert_eq!(
        run_full_pipeline("if true then false else true").unwrap(),
        "false"
    );
    assert_eq!(
        run_full_pipeline("if false then false else true").unwrap(),
        "true"
    );
}

#[test]
fn test_church_boolean() {
    // Church boolean
    let input = r"(\:Bool.\:Bool.1) true false";
    let result = run_full_pipeline(input).unwrap();
    assert_eq!(result, "true");

    let input2 = r"(\:Bool.\:Bool.0) true false";
    let result2 = run_full_pipeline(input2).unwrap();
    assert_eq!(result2, "false");
}

#[test]
fn test_parse_error_position() {
    // パースエラーの位置情報テスト
    let input = "( \\ : Bool . 0"; // 閉じ括弧がない
    match parse_spanned(input) {
        Err(e) => {
            let position = display_position(input, e.line, e.column);
            assert!(position.contains("^"));
            // パースエラーは最後の位置で発生する
            assert!(e.column > 10);
        }
        Ok(_) => panic!("Expected parse error"),
    }
}

#[test]
fn test_type_error_position() {
    // 型エラーの位置情報テスト
    let input = r"\:Bool. if 42 then true else false";
    let ast = parse_spanned(input).unwrap();
    let ctx = Context::default();

    match type_of_spanned(&ctx, &ast) {
        Err(e) => {
            // 42の位置でエラーが発生する
            assert_eq!(e.line, 1);
            assert!(e.column > 10); // 42の位置
            assert!(e.message.contains("unbound variable 42"));
        }
        Ok(_) => panic!("Expected type error"),
    }
}

#[test]
fn test_type_mismatch_in_conditional() {
    // if文での型不一致テスト
    let input = r"if 0 then true else false";
    let ast = parse_spanned(input).unwrap();
    let ctx = Context::default();

    match type_of_spanned(&ctx, &ast) {
        Err(e) => {
            // 0の位置でエラーが発生する
            assert_eq!(e.line, 1);
            assert!(e.column < 5); // 0の位置
            assert!(e.message.contains("unbound variable 0"));
        }
        Ok(_) => panic!("Expected type error"),
    }
}

#[test]
fn test_complex_expression() {
    // より複雑な式のテスト
    let input = r"if (\:Bool.0) true then (\:Bool.\:Bool.1) true false else false";
    let result = run_full_pipeline(input).unwrap();
    assert_eq!(result, "true");
}

#[test]
fn test_nested_application() {
    // ネストした関数適用
    let input = r"(\:Bool.\:Bool.if 1 then 0 else true) true false";
    let result = run_full_pipeline(input).unwrap();
    assert_eq!(result, "false");
}

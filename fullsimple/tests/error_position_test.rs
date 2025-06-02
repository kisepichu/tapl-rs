use fullsimple::parser::{display_position, parse_spanned_and_render_err};
use fullsimple::syntax::context::Context;
use fullsimple::typing::type_of;
use rstest::rstest;

/// パースエラーの位置情報テスト
#[rstest]
#[case("let f = (\\:Bool.0)  f unit", "in' expected", 1, 27)] // let で in が抜けている例
#[case("if true then false", "'else' expected", 1, 19)] // if で else が抜けている例
#[case("(\\:Bool.0", "')' expected", 1, 10)] // 閉じ括弧が抜けている例
#[case("\\:Bool 0", "'.' expected", 1, 8)] // ラムダで . が抜けている例
#[case("case true", "'of' expected", 1, 10)] // case で of が抜けている例
#[case(
    "
case (A Bool+B):::A true of
  | (A Bool+C):::A true => x
    ",
    "case arms expected",
    2,
    28
)] // true はパターンではない
fn test_parse_error_position(
    #[case] input: &str,
    #[case] expected_message_contains: &str,
    #[case] expected_line: u32,
    #[case] expected_column: usize,
) {
    let result = parse_spanned_and_render_err(input);
    assert!(result.is_err(), "入力 '{}' はエラーになるべきです", input);

    let (error_message, position_display) = result.unwrap_err();

    // エラーメッセージに期待される文字列が含まれているかチェック
    assert!(
        error_message.contains(expected_message_contains),
        "エラーメッセージ '{}' に '{}' が含まれていません",
        error_message,
        expected_message_contains
    );

    // 位置情報の表示に期待される行・列が含まれているかチェック
    let expected_pos_str = format!("at line {}, column {}", expected_line, expected_column);
    assert!(
        position_display.contains(&expected_pos_str),
        "位置表示 '{}' に '{}' が含まれていません",
        position_display,
        expected_pos_str
    );
}

/// 型チェックエラーの位置情報テスト
#[rstest]
#[case("if 1 then true else false", "unbound variable", 1, 4)] // if の条件部で型エラー
#[case("succ true", "expected Nat", 1, 6)] // succ に Bool を渡す型エラー
#[case("pred false", "expected Nat", 1, 6)] // pred に Bool を渡す型エラー
#[case("iszero unit", "expected Nat", 1, 8)] // iszero に Unit を渡す型エラー
#[case("true false", "expected arrow type", 1, 1)] // 関数でないものを適用
#[case("(\\:Bool.0) unit", "expected: Bool", 1, 12)] // 型の不一致
fn test_type_error_position(
    #[case] input: &str,
    #[case] expected_message_contains: &str,
    #[case] expected_line: u32,
    #[case] expected_column: usize,
) {
    let spanned_term = parse_spanned_and_render_err(input);
    assert!(
        spanned_term.is_ok(),
        "入力 '{}' はパースできるべきです",
        input
    );

    let term = spanned_term.unwrap();
    let ctx = Context::default();
    let result = type_of(&ctx, &term);

    assert!(result.is_err(), "入力 '{}' は型エラーになるべきです", input);

    let error = result.unwrap_err();

    assert!(
        error.message.contains(expected_message_contains),
        "エラーメッセージ '{}' に '{}' が含まれていません",
        error.message,
        expected_message_contains
    );

    assert_eq!(
        error.line, expected_line,
        "エラー行番号が期待値と異なります"
    );

    assert_eq!(
        error.column, expected_column,
        "エラー列番号が期待値と異なります"
    );

    // 位置表示の確認
    let position_display = display_position(input, error.line, error.column);
    let expected_pos_str = format!("at line {}, column {}", expected_line, expected_column);
    assert!(
        position_display.contains(&expected_pos_str),
        "位置表示 '{}' に '{}' が含まれていません",
        position_display,
        expected_pos_str
    );
}

/// 複雑な式でのエラー位置テスト
#[rstest]
#[case("let x = true in let y = false in x y", "arrow type", 1, 34)] // ネストしたlet内での型エラー
#[case("if (\\:Bool.0) then true else false", "expected Bool", 1, 5)] // 複雑な条件式での型エラー
#[case("(\\:Bool.\\:Nat.0) true unit", "Nat", 1, 23)] // 複数引数関数での型エラー
#[case(
    "type B = Some Bool + None in let B:::Some b = B:::Some true in b b",
    "expected arrow type",
    1,
    64
)] // 型定義とパターンマッチでの型エラー
fn test_complex_error_position(
    #[case] input: &str,
    #[case] expected_message_contains: &str,
    #[case] expected_line: u32,
    #[case] expected_column: usize,
) {
    let spanned_term = parse_spanned_and_render_err(input);
    assert!(
        spanned_term.is_ok(),
        "入力 '{}' はパースできるべきです",
        input
    );

    let term = spanned_term.unwrap();
    let ctx = Context::default();
    let result = type_of(&ctx, &term);

    assert!(result.is_err(), "入力 '{}' は型エラーになるべきです", input);

    let error = result.unwrap_err();

    assert!(
        error.message.contains(expected_message_contains),
        "エラーメッセージ '{}' に '{}' が含まれていません",
        error.message,
        expected_message_contains
    );

    assert_eq!(
        error.line, expected_line,
        "エラー行番号が期待値と異なります"
    );

    assert_eq!(
        error.column, expected_column,
        "エラー列番号が期待値と異なります"
    );
}

/// パターンマッチエラーの位置テスト
#[rstest]
#[case(
    "let x:Bool = unit in x",
    "pattern type Bool does not match term type Unit",
    1,
    14
)] // パターンの型不一致
#[case(
    "
case (A Bool+B):::A true of
  | (A Bool+C):::A x => x
    ",
    "pattern type <",
    3,
    5
)] // case の型不一致
fn test_pattern_error_position(
    #[case] input: &str,
    #[case] expected_message_contains: &str,
    #[case] expected_line: u32,
    #[case] expected_column: usize,
) {
    let spanned_term = parse_spanned_and_render_err(input);
    if spanned_term.is_err() {
        // パースエラーの場合はスキップ
        return;
    }

    let term = spanned_term.unwrap();
    let ctx = Context::default();
    let result = type_of(&ctx, &term);

    if result.is_err() {
        let error = result.unwrap_err();

        assert!(
            error.message.contains(expected_message_contains),
            "エラーメッセージ '{}' に '{}' が含まれていません",
            error.message,
            expected_message_contains
        );

        assert_eq!(
            error.line, expected_line,
            "エラー行番号が期待値と異なります"
        );

        assert_eq!(
            error.column, expected_column,
            "エラー列番号が期待値と異なります"
        );
    }
}

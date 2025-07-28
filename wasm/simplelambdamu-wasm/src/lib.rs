use serde::{Deserialize, Serialize};
use simplelambdamu::eval::{Strategy, eval};
use simplelambdamu::parser;
use simplelambdamu::proof::typst_proof;
use simplelambdamu::syntax::context::Context;
use simplelambdamu::typing::type_of;
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub struct ParseResult {
    pub success: bool,
    pub error: Option<String>,
    pub term: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct TypeCheckResult {
    pub success: bool,
    pub error: Option<String>,
    pub type_string: Option<String>,
    pub proof_typst: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct EvalResult {
    pub success: bool,
    pub error: Option<String>,
    pub result: Option<String>,
    pub proof_typst: Option<String>,
}

#[wasm_bindgen]
pub fn parse_and_type_check(code: &str) -> JsValue {
    // パース
    let t = match parser::parse_spanned_and_render_err(code) {
        Ok(t) => t,
        Err((e, display_position)) => {
            let result = ParseResult {
                success: false,
                error: Some(format!("{}\n{}", display_position, e)),
                term: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    // 型付け
    let ctx = Context::default();
    let ty = match type_of(&ctx, &t) {
        Ok(ty) => ty,
        Err(e) => {
            let result = TypeCheckResult {
                success: false,
                error: Some(format!(
                    "{}\n{}",
                    parser::display_position(code, e.line, e.column),
                    e
                )),
                type_string: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    // 導出図生成
    let proof = match typst_proof(&ctx, &t) {
        Ok(pf) => pf,
        Err(e) => {
            let result = TypeCheckResult {
                success: false,
                error: Some(format!(
                    "{}\n{}",
                    parser::display_position(code, e.line, e.column),
                    e
                )),
                type_string: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    let result = TypeCheckResult {
        success: true,
        error: None,
        type_string: Some(format!("{}: {}", t.v, ty)),
        proof_typst: Some(proof),
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
}

#[wasm_bindgen]
pub fn evaluate(code: &str, strategy_str: &str) -> JsValue {
    // 評価戦略の解析
    let strategy = match Strategy::from(strategy_str) {
        Ok(s) => s,
        Err(_) => {
            let result = EvalResult {
                success: false,
                error: Some(format!("Unknown strategy: {}", strategy_str)),
                result: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    // パース
    let t = match parser::parse_spanned_and_render_err(code) {
        Ok(t) => t,
        Err((e, display_position)) => {
            let result = EvalResult {
                success: false,
                error: Some(format!("{}\n{}", display_position, e)),
                result: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    // 評価
    let result_term = match eval(&t.v, &strategy) {
        Ok(t) => t,
        Err(e) => {
            let result = EvalResult {
                success: false,
                error: Some(format!("eval error: {}", e)),
                result: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    // 評価後の導出図生成
    let ctx = Context::default();
    let proof = match typst_proof(&ctx, &t) {
        Ok(pf) => pf,
        Err(e) => {
            let result = EvalResult {
                success: false,
                error: Some(format!("proof error: {}", e)),
                result: None,
                proof_typst: None,
            };
            return serde_wasm_bindgen::to_value(&result).unwrap();
        }
    };

    let result = EvalResult {
        success: true,
        error: None,
        result: Some(format!("{}", result_term)),
        proof_typst: Some(proof),
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
}

#[wasm_bindgen]
pub fn get_available_strategies() -> JsValue {
    let strategies = vec![
        "normalorder",
        "normalorderwitheta",
        "cbv",
        "cbn",
        "cbvwitheta",
        "cbnwitheta",
    ];
    serde_wasm_bindgen::to_value(&strategies).unwrap()
}

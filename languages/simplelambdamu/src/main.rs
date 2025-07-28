use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use simplelambdamu::eval::{Strategy, eval};
use simplelambdamu::parser;
use simplelambdamu::proof::typst_proof;
use simplelambdamu::syntax::context::Context;
use simplelambdamu::typing::type_of;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut current_strategy = Strategy::from("cbv").unwrap();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line == "exit" || line == "q" {
                    break;
                }
                if line == "help" || line == "?" {
                    println!("Type check and evaluate a term in lambda mu calculus.");
                    println!(
                        "example: \\s:N->N. \\z:N. /alpha:!N. alpha (s (s /beta:!N. alpha z))"
                    );
                    println!("                                                   : (N->N)->N->N");
                    println!("     ->* \\:N->N.\\:N.0");
                    println!("Ctrl+C to exit");
                    continue;
                }
                if line.starts_with("strategy") {
                    if let Some(input) = line.split_whitespace().nth(1) {
                        if let Ok(strategy) = Strategy::from(input) {
                            current_strategy = strategy;
                            println!("Strategy set to: {}", current_strategy);
                        } else {
                            println!("Unknown strategy: {}", input);
                            println!(
                                "Available strategies: cbv, cbn, cbvwitheta, cbnwitheta, normalorder, normalorderwitheta"
                            );
                        }
                    } else {
                        println!("current strategy: {}", current_strategy);
                        println!("strategy {{strategy}} to set the evaluation strategy");
                        println!(
                            "Available strategies: cbv, cbn, cbvwitheta, cbnwitheta, normalorder, normalorderwitheta"
                        );
                    }

                    continue;
                }

                let t = match parser::parse_spanned_and_render_err(line.as_str()) {
                    Ok(t) => t,
                    Err((e, display_position)) => {
                        println!("{}\n{}", display_position, e);
                        continue;
                    }
                };
                // println!("{:?}", t);

                let ctx = Context::default();
                let ty = match type_of(&ctx, &t) {
                    Ok(ty) => ty,
                    Err(e) => {
                        println!("input= {}", t.v);
                        println!(
                            "{}\n{}",
                            parser::display_position(line.as_str(), e.line, e.column),
                            e
                        );
                        continue;
                    }
                };
                // println!("{:?}", ty);

                let proof = match typst_proof(&ctx, &t) {
                    Ok(pf) => pf,
                    Err(e) => {
                        println!("input= {}", t.v);
                        println!(
                            "{}\n{}",
                            parser::display_position(line.as_str(), e.line, e.column),
                            e
                        );
                        continue;
                    }
                };
                println!("proof:\n\n{}", proof);

                println!("input= {}: {}", t.v, ty);
                let t = match eval(&t.v, &current_strategy) {
                    Ok(t) => t,
                    Err(e) => {
                        println!("eval error: {}", e);
                        continue;
                    }
                };
                println!("   ->* {}\n", t);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}

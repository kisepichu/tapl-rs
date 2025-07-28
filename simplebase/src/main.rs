mod eval;
mod parser;
mod proof;
mod span;
mod syntax;
mod typing;

use eval::{Strategy, eval};
use proof::typst_proof;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use syntax::context::Context;
use typing::type_of_spanned;

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
                    println!("Type check and evaluate a term in simply typed lambda calculus.");
                    println!("example: (\\x:Bool.x) true");
                    println!("     ->* true");
                    println!("     ->* \\:Bool.\\:Bool.1");
                    println!("Ctrl+C to exit");
                    continue;
                }
                if line.starts_with("strategy") {
                    let input = line.split_whitespace().nth(1);
                    if let Some(input) = input {
                        match Strategy::from(input) {
                            Ok(strategy) => {
                                current_strategy = strategy;
                                println!("Strategy set to: {}", input);
                            }
                            Err(_) => {
                                println!("Unknown strategy: {}", input);
                                println!("Available strategies:");
                                println!("  normalorder");
                                println!("  normalorderwitheta");
                                println!("  cbv");
                                println!("  cbn");
                                println!("  cbvwitheta");
                                println!("  cbnwitheta");
                            }
                        }
                    } else {
                        println!("Available strategies:");
                        println!("  normalorder");
                        println!("  normalorderwitheta");
                        println!("  cbv");
                        println!("  cbn");
                        println!("  cbvwitheta");
                        println!("  cbnwitheta");
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
                let ty = match type_of_spanned(&ctx, &t) {
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

                let ctx = Context::default();
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
                println!("proof=\n\n{}", proof);

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

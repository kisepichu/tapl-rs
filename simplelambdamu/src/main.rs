mod eval;
mod parser;
mod proof;
mod span;
mod syntax;
mod typing;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use typing::type_of;

use crate::proof::{render_typst_to_svg, typst_proof};
use crate::syntax::context::Context;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let mut current_strategy = eval::Strategy::Cbv;
    let mut proof = String::new();
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
                if line == "svg" {
                    // print svg string
                    println!("{}", render_typst_to_svg(&proof));
                }
                if line.starts_with("strategy") {
                    let strategies = [
                        ("normalorder", eval::Strategy::NormalOrder),
                        ("cbv", eval::Strategy::Cbv),
                        ("cbn", eval::Strategy::Cbn),
                        ("cbvwitheta", eval::Strategy::CbvWithEta),
                        ("cbnwitheta", eval::Strategy::CbnWithEta),
                    ];
                    let input = line.split_whitespace().nth(1);
                    if let Some(input) = input {
                        if let Some((name, strategy)) = strategies.iter().find(|(n, _)| n == &input)
                        {
                            current_strategy = strategy.clone();
                            println!("Strategy set to: {}", name);
                        } else {
                            println!("Unknown strategy: {}", input);
                            println!("Available strategies:");
                            for (name, _) in &strategies {
                                println!("  {}", name);
                            }
                        }
                    } else {
                        println!("Available strategies:");
                        for (name, _) in &strategies {
                            println!("  {}", name);
                        }
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

                proof = match typst_proof(&ctx, &t) {
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
                let t = match eval::eval(&t.v, &current_strategy) {
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

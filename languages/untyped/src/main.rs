use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use untyped::eval::eval;
use untyped::parser;
use untyped::span::Spanned;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line == "exit" || line == "q" {
                    break;
                }
                if line == "help" || line == "?" {
                    println!(
                        "Evaluate a term in the untyped lambda calculus using de Bruijn indices."
                    );
                    println!("example: (\\\\1 0)\\0 ->* \\(\\0)0");
                    println!("         (corresponds to (λx.λy.x y)λx.x ->* λy.(λx.x)y )");
                    println!("Ctrl+C to exit");
                    continue;
                }

                let t = match parser::parse_and_render_err(line.as_str()) {
                    Ok(t) => t,
                    Err((e, display_position)) => {
                        println!("{}\n{}", display_position, e);
                        continue;
                    }
                };
                let t = Spanned {
                    v: t,
                    start: 0,
                    line: 0,
                    column: 0,
                };

                println!("input= {}", t);
                // println!("{:?}", t);
                let t = match eval(&t) {
                    Ok(t) => t,
                    Err(e) => {
                        println!("eval error: {:?}", e);
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

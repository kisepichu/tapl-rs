mod eval;
mod parser;
mod syntax;
mod typing;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use syntax::context::Context;
use typing::type_of;

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

                let t = match parser::parse(line.as_str()) {
                    Ok(t) => t,
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    }
                };

                let ctx = Context::default();

                let ty = match type_of(&ctx, &t) {
                    Ok(ty) => ty,
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    }
                };

                println!("input: {}: {}", t, ty);
                let t = match eval::eval(&t) {
                    Ok(t) => t,
                    Err(e) => {
                        println!("eval error: {}", e);
                        continue;
                    }
                };
                println!("   ->* {}", t);
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

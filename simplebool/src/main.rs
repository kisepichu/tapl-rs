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
                    println!("Type check and evaluate a term in simply typed lambda calculus.");
                    println!(
                        "example: (\\:Bool.if 0 then \\:Bool.\\:Bool.1 else \\:Bool.\\:Bool.0) true"
                    );
                    println!(
                        "                                                   : Bool->Bool->Bool"
                    );
                    println!("     ->* \\:Bool.\\:Bool.1");
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
                // println!("{:?}", t);

                let ctx = Context::default();

                let ty = match type_of(&ctx, &t) {
                    Ok(ty) => ty,
                    Err(e) => {
                        println!("input= {}", t);
                        println!("{}", e);
                        continue;
                    }
                };
                // println!("{:?}", ty);

                println!("input= {}: {}", t, ty);
                let t = match eval::eval(&t) {
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

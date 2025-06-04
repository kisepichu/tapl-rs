mod eval;
mod parser;
mod span;
mod syntax;
mod typing;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use syntax::context::Context;
use typing::type_of_spanned;

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

                println!("input= {}: {}", t.v, ty);
                let t = match eval::eval(&t.v) {
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

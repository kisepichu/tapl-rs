mod eval;
mod parser;
mod span;
mod syntax;
mod typing;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use syntax::context::Context;
use typing::type_of;

fn main() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
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
                    println!("Ctrl+C to exit\n");
                    continue;
                }

                let spanned_t = match parser::parse_spanned_and_render_err(line.as_str()) {
                    Ok(spanned_t) => spanned_t,
                    Err((err_msg, err_display)) => {
                        println!("Parse Error: {}", err_msg);
                        println!("{}\n", err_display);
                        continue;
                    }
                };
                // println!("{:?}", t);

                let ctx = Context::default();

                let ty = match type_of(&ctx, &spanned_t) {
                    Ok(ty) => ty,
                    Err(e) => {
                        println!("\ninput= {}", spanned_t.v);
                        println!("{}", e);
                        println!(
                            "{}\n",
                            parser::display_position(line.as_str(), e.line, e.column)
                        );
                        continue;
                    }
                };
                // println!("{:?}", ty);

                println!("\ninput= {}\n     : {}", spanned_t.v, ty);
                let t = match eval::eval(&spanned_t.v) {
                    Ok(t) => t,
                    Err(e) => {
                        println!("eval error: {}\n", e);
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
    if rl.save_history("history.txt").is_err() {
        println!("error saving history.txt");
    }
    Ok(())
}

pub mod eval;
pub mod parser;
pub mod term;

fn main() {
    let input = r"\\(1)\0";
    let t = parser::parse(input).unwrap();
    println!("{}", term::print_tm(&t));
}

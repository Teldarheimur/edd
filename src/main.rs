use edd::{parse, Query};

use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();

    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parse_result = parse(line.trim());
        match parse_result {
            Ok(q) => {
                println!("{q}");

                match q {
                    Query::Inquire(e) => println!(" = {}", e.eval_const()),
                    _ => eprintln!("unsupported"),
                }

            }
            Err(e) => println!(" Syntax error: {e}"),
        }
    }
}

use gobble::{LCChars, Parser};
extern crate ion_parse;

fn main() {
    let ip = std::io::stdin();
    let p = ion_parse::statement();
    let ed = ion_parse::to_end();
    loop {
        let mut s = String::new();
        ip.read_line(&mut s).ok();

        let mut it = LCChars::str(&s);
        let mut more = true;
        while more {
            match p.parse(&it) {
                Ok((i2, s)) => {
                    println!("{:?}", s);
                    it = i2;
                    if let Ok(_) = ed.parse(&it) {
                        more = false;
                    }
                }
                Err(e) => {
                    println!("Error: {}", e);
                    more = false;
                }
            }
        }
    }
}

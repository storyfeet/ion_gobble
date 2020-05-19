use gobble::{LCChars, Parser};
extern crate ion_parse;
use std::collections::BTreeMap;

use ion_parse::color::*;
use ion_parse::partial::err_end;
use std::io::Write;
use termion::cursor::Goto;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

fn drop_last_char(s: &mut String) {
    let l = s.len();
    for x in 1..6 {
        if l < x {
            return;
        }
        if let Some(_) = s.as_str().get(l - x..) {
            s.remove(l - x);
            return;
        }
    }
}

fn main() {
    let (ch_s, ch_r) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let stdin = std::io::stdin();
        for k in stdin.keys() {
            ch_s.send(k).expect("Could not send on channel");
        }
    });

    let mut screen = std::io::stdout()
        .into_raw_mode()
        .expect("could not get raw mode");

    write!(
        screen,
        "{}{}Type Away  -- esc to clear then quit: \n\r>",
        termion::clear::All,
        Goto(1, 1)
    )
    .expect("Could not clear screen");
    screen.flush().ok();
    let mut main_s = String::new();

    loop {
        let mut print_out = false;
        while let Ok(Ok(k)) = ch_r.try_recv() {
            match k {
                Key::Char(c) => {
                    main_s.push(c);
                    if c == '\u{A}' || c == '\u{D}' {
                        print_out = true;
                    }
                }
                Key::Backspace => {
                    drop_last_char(&mut main_s);
                }
                Key::Esc => {
                    if main_s.len() > 0 {
                        main_s = String::new();
                    } else {
                        return;
                    }
                }
                _ => {}
            }

            let sr = try_partial(&main_s);

            write!(
                screen,
                "{}{}Type Away  -- esc to clear then quit: ",
                termion::clear::All,
                Goto(1, 1),
            )
            .expect("Could not output result");
            //let mut cpos = (1,1)
            for s in sr.0.split("\n") {
                print!("\n\r> {}", s);
            }

            if print_out {
                for r in sr.1 {
                    print!("----------------\n\r");
                    print!("{:?}\n\r", r);
                }
            }
            screen.flush().expect("could not get screen out");
        }
    }
}

pub fn try_partial(
    s: &str,
) -> (
    String,
    Vec<Result<ion_parse::Statement, gobble::ParseError>>,
) {
    let end = ion_parse::to_end();
    let ppart = ion_parse::partial::statement();
    let spars = ion_parse::statement();
    let mut lc = LCChars::str(s);
    let mut mp = BTreeMap::new();
    let mut p_res = Vec::new();
    loop {
        if let Ok((it, c)) = end.parse(&lc) {
            if c == '_' {
                break;
            }
            lc = it;
            continue;
        }
        p_res.push(spars.parse(&lc).map(|(_, v)| v));
        match ppart.parse(&lc) {
            Ok((it, pst)) => {
                ion_parse::color::col_statement(&pst, &mut mp);
                lc = it;
            }
            Err(_) => {
                let (it, errs) = err_end().parse(&lc).expect("Error should end OK");
                ins_eor(&mut mp, &errs, SType::Err);
                lc = it;
            }
        }
    }
    (print_tree(s, &mp), p_res)
}

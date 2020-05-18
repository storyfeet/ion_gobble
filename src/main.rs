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

    write!(screen, "{}{}", termion::clear::All, Goto(1, 1)).expect("Could not clear screen");
    screen.flush().ok();
    let mut main_s = String::new();

    loop {
        while let Ok(Ok(k)) = ch_r.try_recv() {
            match k {
                Key::Char(c) => {
                    main_s.push(c);
                }
                Key::Backspace => {
                    drop_last_char(&mut main_s);
                }
                Key::Esc => {
                    return;
                }
                _ => {}
            }

            let sr = try_partial(&main_s);

            write!(screen, "{}{}{}", termion::clear::All, Goto(1, 1), sr)
                .expect("Could not output result");
            screen.flush().expect("could not get screen out");
        }
    }
}

pub fn try_partial(s: &str) -> String {
    let ed = ion_parse::to_end();
    let ppart = ion_parse::partial::statement();
    let mut lc = LCChars::str(s);
    let mut mp = BTreeMap::new();
    loop {
        if let Ok(_) = ed.parse(&lc) {
            break;
        }
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
    print_tree(s, &mp)
}

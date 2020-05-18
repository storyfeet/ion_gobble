use crate::partial::*;
use std::collections::BTreeMap;
use std::fmt;
use termion::{color, color::Fg};

#[derive(Debug, Clone, PartialEq)]
pub enum SType {
    Keyword,
    Lit,
    Var,
    Op,
    Err,
    Reset,
}

impl fmt::Display for SType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SType::Keyword => write!(f, "{}", Fg(color::Green)),
            SType::Lit => write!(f, "{}", Fg(color::Magenta)),
            SType::Reset => write!(f, "{}", Fg(color::Black)),
            SType::Var => write!(f, "{}", Fg(color::Blue)),
            SType::Op => write!(f, "{}", Fg(color::Yellow)),
            SType::Err => write!(f, "{}", Fg(color::Red)),
        }
    }
}

pub fn ins_eor<V>(mp: &mut BTreeMap<usize, SType>, e: &EOr<V>, st: SType) {
    ins(mp, e.start, st);
    ins(mp, e.fin, SType::Reset);
}

pub fn ins(mp: &mut BTreeMap<usize, SType>, op: Option<usize>, st: SType) {
    if let Some(u) = op {
        mp.insert(u, st);
    }
}

pub fn col_statement(s: &PStatement, cmap: &mut BTreeMap<usize, SType>) {
    match s {
        PStatement::LetList(lt) => {
            ins_eor(cmap, lt, SType::Keyword);
        }
        PStatement::Let(lt, vars, assop, items) => {
            ins_eor(cmap, lt, SType::Keyword);
            for v in vars {
                ins_eor(cmap, v, SType::Var);
            }
            ins_eor(cmap, assop, SType::Op);
            for i in items {
                ins_eor(cmap, i, SType::Var);
            }
        }
        PStatement::ExportList(ex) => {
            ins_eor(cmap, ex, SType::Keyword);
        }
        PStatement::Export(ex, vars, assop, items) => {
            ins_eor(cmap, ex, SType::Keyword);
            for v in vars {
                ins_eor(cmap, v, SType::Var);
            }
            ins_eor(cmap, assop, SType::Op);
            for i in items {
                ins_eor(cmap, i, SType::Var);
            }
        }
        _ => {}
    }
}

pub fn print_tree(s: &str, map: &BTreeMap<usize, SType>) -> String {
    let mut pos = 0;
    let mut res = String::new();

    for (k, v) in map {
        if *k > pos {
            res.push_str(&s[pos..*k]);
        }
        res.push_str(&v.to_string());
        pos = *k;
    }
    res.push_str(&s[pos..]);
    res.push_str(&SType::Reset.to_string());
    res
}

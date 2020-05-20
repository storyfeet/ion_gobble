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
    Ident,
    Err,
    Reset,
    Doc,
}

impl fmt::Display for SType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SType::Keyword => write!(f, "{}", Fg(color::Green)),
            SType::Ident => write!(f, "{}", Fg(color::Cyan)),
            SType::Lit => write!(f, "{}", Fg(color::Magenta)),
            SType::Reset => write!(f, "{}", Fg(color::Reset)),
            SType::Var => write!(f, "{}", Fg(color::LightMagenta)),
            SType::Doc => write!(f, "{}", Fg(color::Blue)),
            SType::Op => write!(f, "{}", Fg(color::Yellow)),
            SType::Err => write!(f, "{}", Fg(color::Red)),
        }
    }
}

pub fn ins_oloc<V>(mp: &mut BTreeMap<usize, SType>, e: &OLoc<V>, st: SType) {
    if let Some(i) = e {
        ins_loc(mp, i, st)
    }
}

pub fn ins_loc<V>(mp: &mut BTreeMap<usize, SType>, e: &Loc<V>, st: SType) {
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
        PStatement::LetList(kw)
        | PStatement::ExportList(kw)
        | PStatement::Else(kw)
        | PStatement::Continue(kw)
        | PStatement::FuncList(kw)
        | PStatement::End(kw)
        | PStatement::Break(kw) => {
            ins_loc(cmap, kw, SType::Keyword);
        }
        PStatement::Let(lt, vars, assop, items) | PStatement::Export(lt, vars, assop, items) => {
            ins_loc(cmap, lt, SType::Keyword);
            for v in vars {
                ins_loc(cmap, v, SType::Var);
            }
            ins_oloc(cmap, assop, SType::Op);
            for oi in items {
                if let Some(i) = oi {
                    col_item(&i.v, cmap);
                }
                //ins_eor(cmap, i, SType::Var);
            }
        }
        PStatement::Elif(kw, ex) | PStatement::If(kw, ex) | PStatement::While(kw, ex) => {
            ins_loc(cmap, kw, SType::Keyword);
            ins_oloc(cmap, ex, SType::Var);
        }
        PStatement::Match(kw, it) | PStatement::Case(kw, it) => {
            ins_loc(cmap, kw, SType::Keyword);
            ins_oloc(cmap, it, SType::Var);
        }
        PStatement::FuncDef(kw, nm, doc, vars) => {
            ins_loc(cmap, kw, SType::Keyword);
            ins_loc(cmap, nm, SType::Ident);
            for v in vars {
                ins_loc(cmap, v, SType::Var);
            }
            if let Some(d) = doc {
                ins_loc(cmap, d, SType::Doc);
            }
        }
        PStatement::For(kw, vars, ikw, it) => {
            ins_loc(cmap, kw, SType::Keyword);
            for v in vars {
                ins_oloc(cmap, v, SType::Var);
            }
            ins_oloc(cmap, ikw, SType::Keyword);
            ins_oloc(cmap, it, SType::Var);
        }
        PStatement::Expr(e) => {
            col_expr(e, cmap);
        }
    }
}

pub fn col_expr(e: &PExpr, cmap: &mut BTreeMap<usize, SType>) {
    match e {
        PExpr::Command(c) => {
            for i in &c.v {
                col_item(i, cmap);
            }
        }
        PExpr::Pipe(p, c, expr) => {
            for i in &c.v {
                col_item(i, cmap);
            }
            ins_loc(cmap, p, SType::Op);
            if let Some(lex) = expr.as_ref() {
                col_expr(&lex.v, cmap);
            }
            //col_expr(expr, cmap);
        }
    }
}

pub fn col_item(i: &Item, cmap: &mut BTreeMap<usize, SType>) {
    match i {
        Item::Bool(b) => {
            ins_loc(cmap, b, SType::Lit);
        }
        Item::Float(f) => {
            ins_loc(cmap, f, SType::Lit);
        }
        Item::Int(i) => {
            ins_loc(cmap, i, SType::Lit);
        }
        Item::Array(s, it, f) => {
            ins_loc(cmap, s, SType::Op);
            for i in it {
                col_item(i, cmap);
            }
            ins_oloc(cmap, f, SType::Op);
        }
        Item::Str(pts) => {
            for p in pts {
                match &p.v {
                    UnquotedPart::Quoted(s, q, f) => {
                        ins_loc(cmap, s, SType::Lit);
                        for qp in q {
                            col_quoted(qp, cmap);
                        }
                        ins_oloc(cmap, f, SType::Lit);
                    }
                    UnquotedPart::Lit(l) => {
                        ins_loc(cmap, l, SType::Reset);
                    }
                    _ => {}
                }
            }
        }
    }
}

pub fn col_quoted(sp: &StringPart, cmap: &mut BTreeMap<usize, SType>) {
    match sp {
        StringPart::Lit(l) => ins_loc(cmap, l, SType::Var),
        StringPart::Sub(_) => {}
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

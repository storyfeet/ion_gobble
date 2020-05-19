use gobble::*;

use crate::{ass_op, ident, iws, wst, AssOp};

pub type Kw = EOr<&'static str>;

#[derive(Clone, Debug, PartialEq)]
pub struct EOr<V> {
    pub start: Option<usize>,
    pub fin: Option<usize>,
    pub v: Option<V>,
}

pub fn err_end() -> impl Parser<EOr<()>> {
    e_or(
        (
            skip_while(Any.except("\n;"), 1),
            or(";\n".one().asv(()), eoi),
        )
            .asv(()),
    )
}

pub fn locate<P: Parser<V>, V>(p: P) -> impl Parser<EOr<V>> {
    (gobble::index, p, gobble::index).map(|(start, v, fin)| EOr {
        start,
        v: Some(v),
        fin,
    })
}

pub fn e_or<P: Parser<V>, V>(p: P) -> impl Parser<EOr<V>> {
    (
        gobble::index,
        or(
            peek(to_end().try_map(|c| {
                if c == '_' {
                    Ok(None)
                } else {
                    Err(ECode::SMess("partial ending on ; or \\n"))
                }
            })),
            p.map(|v| Some(v)),
        ),
        gobble::index,
    )
        .map(|(start, v, fin)| EOr { start, v, fin })
}

pub fn wst_eor<P: Parser<V>, V>(p: P) -> impl Parser<EOr<V>> {
    wst(e_or(p))
}

pub fn eor_word<P: Parser<V>, V>(p: P) -> impl Parser<EOr<V>> {
    e_or(keyword(p))
}

pub fn to_end() -> impl Parser<char> {
    (
        skip_repeat(
            or3(
                " \t".min_n(1).asv(()),
                "\\\n".asv(()),
                "#".then_ig(skip_while(Any.except(";\n"), 1)).asv(()),
            ),
            0,
        ),
        or("\n;".one(), eoi.asv('_')),
    )
        .map(|(_, e)| e)
}

//Main Code

#[derive(Debug, PartialEq, Clone)]
pub enum PStatement {
    LetList(EOr<&'static str>),
    Let(Kw, Vec<EOr<PVar>>, EOr<AssOp>, Vec<EOr<Item>>),
    ExportList(EOr<&'static str>),
    Export(Kw, Vec<EOr<PVar>>, EOr<AssOp>, Vec<EOr<Item>>),
    If(Kw, EOr<PExpr>),
    Else(Kw),
    Elif(Kw, EOr<PExpr>),
    End(Kw),
    While(Kw, EOr<PExpr>),
    For(Kw, Vec<EOr<PVar>>, Kw, EOr<PExpr>),
    Match(Kw, EOr<Item>),
    Case(Kw, EOr<Item>),
    //TODO work out where ion puts func description
    FuncList(Kw),
    FuncDef(Kw, EOr<String>, Option<EOr<String>>, Vec<EOr<PVar>>),
    Expr(PExpr),
    Break(Kw),
    Continue(Kw),
}
pub fn statement() -> impl Parser<PStatement> {
    wst(or(
        or6(
            let_statement(),
            export_statement(),
            if_statement(),
            else_statement(),
            loop_statement(),
            func_def(),
        ),
        or6(
            eor_word("end").map(|e| PStatement::End(e)),
            eor_word("break").map(|e| PStatement::Break(e)),
            eor_word("continue").map(|e| PStatement::Continue(e)),
            (eor_word("match"), wst_eor(item)).map(|(kw, i)| PStatement::Match(kw, i)),
            (eor_word("case"), wst_eor(item)).map(|(kw, i)| PStatement::Case(kw, i)),
            expr.map(|e| PStatement::Expr(e)),
        ),
    ))
    .then_ig(to_end())
}

#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Str,
    Bool,
    Int,
    Float,
    Arr(Box<VarType>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RangeEnd {
    Int(isize),
    Sub(Substitution),
}

pub fn range_end() -> impl Parser<RangeEnd> {
    or(
        common_int.map(|n| RangeEnd::Int(n)),
        substitution.map(|s| RangeEnd::Sub(s)),
    )
}
#[derive(Debug, PartialEq, Clone)]
pub enum RangeOp {
    Exc,
    Inc,
}
pub fn range_op() -> impl Parser<RangeOp> {
    ("..", maybe("=")).map(|(_, e)| match e {
        Some(_) => RangeOp::Inc,
        None => RangeOp::Exc,
    })
}

#[derive(Debug, PartialEq, Clone)]
pub struct Range {
    start: Option<RangeEnd>,
    fin: Option<RangeEnd>,
    op: Option<RangeOp>,
}

pub fn range() -> impl Parser<Range> {
    or(
        (range_end(), maybe((range_op(), maybe(range_end())))).map(|(s, op)| match op {
            Some((op, fin)) => Range {
                start: Some(s),
                fin,
                op: Some(op),
            },
            None => Range {
                start: Some(s),
                fin: None,
                op: None,
            },
        }),
        (range_op(), range_end()).map(|(op, fin)| Range {
            start: None,
            fin: Some(fin),
            op: Some(op),
        }),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index(Vec<Range>);

pub fn index() -> impl Parser<Index> {
    ('[', (repeat(wst(range()), 1), wst(']')).brk()).map(|(_, (i, _))| Index(i))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Bool(EOr<bool>),
    Int(EOr<isize>),
    Float(EOr<f64>),
    Str(Vec<EOr<UnquotedPart>>),
    Array(Kw, Vec<Item>, Kw),
}
pub fn item<'a>(it: &LCChars<'a>) -> ParseRes<'a, Item> {
    //TODO float
    let p = or5(
        locate(common_bool).map(|b| Item::Bool(b)),
        (locate("["), repeat_until(wst(item), wst(e_or("]"))))
            .map(|(s, (l, e))| Item::Array(s, l, e)),
        locate(common_float).map(|f| Item::Float(f)),
        locate(common_int).map(|n| Item::Int(n)),
        unquoted().map(|i| Item::Str(i)),
    );
    p.parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Substitution {
    sub: Sub,
    index: Option<Index>,
}
pub fn substitution<'a>(it: &LCChars<'a>) -> ParseRes<'a, Substitution> {
    (sub(), maybe(index()))
        .map(|(sub, index)| Substitution { sub, index })
        .parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sub {
    Var(String),
    DollarB(PExpr),
    AtVar(String),
    AtB(PExpr),
    NameSpace(String, String),
}

pub fn sub() -> impl Parser<Sub> {
    or5(
        "$(".ig_then(wst(expr))
            .then_ig(wst(")"))
            .map(|e| Sub::DollarB(e)),
        "$".ig_then(ident()).map(|i| Sub::Var(i)),
        ("${", ident(), "::", ident(), "}").map(|(_, a, _, b, _)| Sub::NameSpace(a, b)),
        "@(".ig_then(wst(expr))
            .then_ig(wst(")"))
            .map(|e| Sub::AtB(e)),
        "@".ig_then(ident()).map(|i| Sub::AtVar(i)),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct PVar {
    name: String,
    vtype: Option<EOr<VarType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Command {
    pub v: Vec<Item>,
}

pub fn command() -> impl Parser<Command> {
    repeat(wst(item), 1).map(|v| Command { v })
}

#[derive(Debug, PartialEq, Clone)]
pub enum OutputType {
    StdErr,
    StdOut,
    Combined,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PipeType {
    Bar,
    Append,
    Write,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pipe {
    out: OutputType,
    tp: PipeType,
}

pub fn output_type() -> impl Parser<OutputType> {
    maybe("^&".one()).map(|op| match op {
        Some('^') => OutputType::StdErr,
        Some('&') => OutputType::Combined,
        _ => OutputType::StdOut,
    })
}

pub fn pipe_type() -> impl Parser<PipeType> {
    or3(
        ">>".map(|_| PipeType::Append),
        ">".map(|_| PipeType::Write),
        "|".map(|_| PipeType::Bar),
    )
}
pub fn pipe() -> impl Parser<Pipe> {
    (output_type(), pipe_type()).map(|(out, tp)| Pipe { out, tp })
}

#[derive(Debug, PartialEq, Clone)]
pub enum PExpr {
    Command(Command),
    Pipe(EOr<Pipe>, Box<Command>, Box<PExpr>),
}
pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, PExpr> {
    (command(), maybe((wst(locate(pipe())), expr)))
        .map(|(l, popt)| match popt {
            Some((p, r)) => PExpr::Pipe(p, Box::new(l), Box::new(r)),
            None => PExpr::Command(l),
        })
        .parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPart {
    Lit(EOr<String>),
    Sub(Substitution),
}

pub fn quoted() -> impl Parser<(Kw, Vec<StringPart>, Kw)> {
    locate("\"")
        .then(repeat_until(string_part(), e_or("\"")))
        .map(|(s, (m, f))| (s, m, f))
}

pub fn quoted_escape() -> impl Parser<String> {
    '\\'.ig_then(or4(
        iws(1).map(|_| String::new()),
        't'.map(|_| '\t'.to_string()),
        'n'.map(|_| '\n'.to_string()),
        Any.one().map(|s| s.to_string()),
    ))
}

pub fn string_part() -> impl Parser<StringPart> {
    or(
        locate(string_repeat(
            or(Any.except("@$\"\\").min_n(1), quoted_escape()),
            1,
        ))
        .map(|s| StringPart::Lit(s)),
        substitution.map(|e| StringPart::Sub(e)),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnquotedPart {
    Lit(EOr<String>),
    Sub(Substitution),
    Quoted(Kw, Vec<StringPart>, Kw),
}

pub fn unquoted_escape() -> impl Parser<String> {
    '\\'.ig_then(or3(
        //iws(1).map(|_| String::new()),
        't'.map(|_| '\t'.to_string()),
        'n'.map(|_| '\n'.to_string()),
        Any.one().map(|s| s.to_string()),
    ))
}
pub fn unquoted_string_part() -> impl Parser<UnquotedPart> {
    or3(
        locate(string_repeat(
            or(
                Any.except(" \n\r()@$\\\"|&><^#;[]").min_n(1),
                unquoted_escape(),
            ),
            1,
        ))
        .map(|s| UnquotedPart::Lit(s)),
        quoted().map(|(s, q, f)| UnquotedPart::Quoted(s, q, f)),
        substitution.map(|e| UnquotedPart::Sub(e)),
    )
}

pub fn unquoted() -> impl Parser<Vec<EOr<UnquotedPart>>> {
    repeat(locate(unquoted_string_part()), 1)
}

pub fn var_type<'a>(it: &LCChars<'a>) -> ParseRes<'a, VarType> {
    wst(keyword("str")
        .asv(VarType::Str)
        .or(keyword("bool").asv(VarType::Bool))
        .or(keyword("float").asv(VarType::Float))
        .or(keyword("int").asv(VarType::Int))
        .or('['
            .ig_then(var_type)
            .then_ig(wst(']'))
            .map(|c| VarType::Arr(Box::new(c)))))
    .parse(it)
}

pub fn var() -> impl Parser<PVar> {
    ident()
        .then(maybe(wst(':').ig_then(e_or(var_type))))
        .map(|(n, t)| PVar { name: n, vtype: t })
}

pub fn let_statement() -> impl Parser<PStatement> {
    or(
        (eor_word("let"), peek(to_end())).map(|(e, _)| PStatement::LetList(e)),
        (
            eor_word("let"),
            reflect(wst_eor(var()), wst_eor(ass_op()), wst_eor(item)).brk(),
        )
            .map(|(l, (a, b, c))| PStatement::Let(l, a, b, c)),
    )
}

pub fn export_statement() -> impl Parser<PStatement> {
    or(
        (eor_word("export"), peek(to_end())).map(|(e, _)| PStatement::ExportList(e)),
        (
            eor_word("export"),
            reflect(wst_eor(var()), wst_eor(ass_op()), wst_eor(item)).brk(),
        )
            .map(|(l, (a, b, c))| PStatement::Export(l, a, b, c)),
    )
}

pub fn if_statement() -> impl Parser<PStatement> {
    (eor_word("if"), e_or(expr.brk())).map(|(kw, e)| PStatement::If(kw, e))
}

pub fn else_statement() -> impl Parser<PStatement> {
    or(
        eor_word("else").map(|e| PStatement::Else(e)),
        (eor_word("elif"), e_or(expr.brk())).map(|(kw, e)| PStatement::Elif(kw, e)),
    )
}

pub fn loop_statement() -> impl Parser<PStatement> {
    or(
        (
            eor_word("for"),
            (
                repeat_until(wst_eor(var()), wst_eor(keyword("in"))),
                wst_eor(expr),
            )
                .brk(),
        )
            .map(|(fk, ((vars, ik), ex))| PStatement::For(fk, vars, ik, ex)),
        (eor_word("while"), e_or(expr.brk())).map(|(kw, ex)| PStatement::While(kw, ex)),
    )
}

pub fn func_def() -> impl Parser<PStatement> {
    //TODO work out how function hints are written
    or(
        (eor_word("fn"), peek(to_end())).map(|(kw, _)| PStatement::FuncList(kw)),
        (
            eor_word("fn"),
            (
                wst_eor(ident()),
                repeat_until_ig(wst_eor(var()), or(peek(wst('-')), to_end())),
                maybe(e_or(wst("--").ig_then(Any.except("\n;").any()))),
            )
                .brk(),
        )
            .map(|(kw, (nm, vars, doc))| PStatement::FuncDef(kw, nm, doc, vars)),
    )
}

pub fn command_statement() -> impl Parser<Vec<Item>> {
    repeat(wst(item), 1)
}

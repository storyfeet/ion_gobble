use gobble::*;

use crate::{ass_op, ident, iws, wst, AssOp};

pub type Kw = Loc<&'static str>;
pub type OKw = OLoc<&'static str>;
pub type OLoc<V> = Option<Loc<V>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Loc<V> {
    pub start: Option<usize>,
    pub fin: Option<usize>,
    pub v: V,
}

pub fn err_end() -> impl Parser<Out = OLoc<()>> {
    o_loc(
        (
            skip_while(Any.except("\n;"), 1),
            or(";\n".one().asv(()), eoi),
        )
            .asv(()),
    )
}

pub fn locate<P: Parser>(p: P) -> impl Parser<Out = Loc<P::Out>> {
    (gobble::index, p, gobble::index).map(|(start, v, fin)| Loc { start, v: v, fin })
}

pub fn l_word<P: Parser>(p: P) -> impl Parser<Out = Loc<P::Out>> {
    locate(keyword(p))
}

pub fn ol_word<P: Parser>(p: P) -> impl Parser<Out = OLoc<P::Out>> {
    o_loc(keyword(p))
}

pub fn o_loc<P: Parser>(p: P) -> impl Parser<Out = OLoc<P::Out>> {
    or(
        peek(to_end().try_map(|c| {
            if c == '_' {
                Ok(None)
            } else {
                Err(ECode::SMess("partial ending on ; or \\n"))
            }
        })),
        locate(p).map(|v| Some(v)),
    )
}

pub fn to_end() -> impl Parser<Out = char> {
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
    LetList(Loc<&'static str>),
    Let(Kw, Vec<Loc<PVar>>, OLoc<AssOp>, Vec<OLoc<Item>>),
    ExportList(Loc<&'static str>),
    Export(Kw, Vec<Loc<PVar>>, OLoc<AssOp>, Vec<OLoc<Item>>),
    If(Kw, OLoc<PExpr>),
    Else(Kw),
    Elif(Kw, OLoc<PExpr>),
    End(Kw),
    While(Kw, OLoc<PExpr>),
    For(Kw, Vec<OLoc<PVar>>, OKw, OLoc<PExpr>),
    Match(Kw, OLoc<Item>),
    Case(Kw, OLoc<Item>),
    //TODO work out where ion puts func description
    FuncList(Kw),
    FuncDef(Kw, Loc<String>, Option<Loc<String>>, Vec<Loc<PVar>>),
    Expr(PExpr),
    Break(Kw),
    Continue(Kw),
}
pub fn statement() -> impl Parser<Out = PStatement> {
    wst(or(
        or5(
            ass_statement(),
            //let_statement(),
            //export_statement(),
            if_statement(),
            else_statement(),
            loop_statement(),
            func_def(),
        ),
        or6(
            locate("end").map(|e| PStatement::End(e)),
            locate("break").map(|e| PStatement::Break(e)),
            locate("continue").map(|e| PStatement::Continue(e)),
            (locate("match"), wst(o_loc(item))).map(|(kw, i)| PStatement::Match(kw, i)),
            (locate("case"), wst(o_loc(item))).map(|(kw, i)| PStatement::Case(kw, i)),
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

pub fn range_end() -> impl Parser<Out = RangeEnd> {
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
pub fn range_op() -> impl Parser<Out = RangeOp> {
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

pub fn range() -> impl Parser<Out = Range> {
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

pub fn index() -> impl Parser<Out = Index> {
    ('[', (repeat(wst(range()), 1), wst(']')).brk()).map(|(_, (i, _))| Index(i))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Bool(Loc<bool>),
    Int(Loc<isize>),
    Float(Loc<f64>),
    Str(Vec<Loc<UnquotedPart>>),
    Array(Kw, Vec<Item>, OKw),
}
pub fn item<'a>(it: &LCChars<'a>) -> ParseRes<'a, Item> {
    //TODO float
    let p = or5(
        locate(common_bool).map(|b| Item::Bool(b)),
        (locate("["), repeat_until(wst(item), wst(o_loc("]"))))
            .map(|(s, (l, e))| Item::Array(s, l, e)),
        locate(common_float).map(|f| Item::Float(f)),
        locate(common_int).map(|n| Item::Int(n)),
        unquoted().map(|i| Item::Str(i)),
    );
    p.parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Substitution {
    pub sub: Sub,
    pub index: Option<Index>,
}
pub fn substitution<'a>(it: &LCChars<'a>) -> ParseRes<'a, Substitution> {
    (sub(), maybe(index()))
        .map(|(sub, index)| Substitution { sub, index })
        .parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sub {
    Var(Kw, OLoc<String>),
    DollarB(Kw, OLoc<PExpr>, OKw),
    AtVar(Kw, OLoc<String>),
    AtB(Kw, OLoc<PExpr>, OKw),
    VarWrap(Kw, OLoc<String>, OLoc<Option<(&'static str, String)>>, OKw),
}

pub fn sub() -> impl Parser<Out = Sub> {
    or3(
        (locate(or("$(", "@(")), wst(o_loc(expr)), wst(o_loc(")"))).map(|(a, b, c)| match a.v {
            "$(" => Sub::DollarB(a, b, c),
            _ => Sub::AtB(a, b, c),
        }),
        (locate(or("@", "$")), o_loc(ident())).map(|(d, v)| match d.v {
            "@" => Sub::AtVar(d, v),
            _ => Sub::Var(d, v),
        }),
        (
            locate("${"),
            o_loc(ident()),
            o_loc(maybe(("::", ident()))),
            o_loc("}"),
        )
            .map(|(st, v, exp, fin)| Sub::VarWrap(st, v, exp, fin)),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct PVar {
    name: String,
    vtype: Option<OLoc<VarType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PEnvItem {
    k: Loc<String>,
    eq: OLoc<char>,
    v: OLoc<Item>,
}

pub fn env_item() -> impl Parser<Out = PEnvItem> {
    (locate(ident()), o_loc('='), o_loc(item)).map(|(k, eq, v)| PEnvItem { k, eq, v })
}

#[derive(Debug, PartialEq, Clone)]
pub struct PEnvSet {
    pub env: Kw,
    pub v: Vec<PEnvItem>,
}

pub fn env_set() -> impl Parser<Out = PEnvSet> {
    (l_word("env"), repeat(wst(env_item()), 1)).map(|(env, v)| PEnvSet { env, v })
}

#[derive(Debug, PartialEq, Clone)]
pub struct Command {
    pub env: Option<PEnvSet>,
    pub v: Vec<Loc<Item>>,
}

pub fn command() -> impl Parser<Out = Command> {
    (maybe(env_set()), repeat(wst(locate(item)), 0)).map(|(env, v)| Command { env, v })
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

pub fn output_type() -> impl Parser<Out = OutputType> {
    maybe("^&".one()).map(|op| match op {
        Some('^') => OutputType::StdErr,
        Some('&') => OutputType::Combined,
        _ => OutputType::StdOut,
    })
}

pub fn pipe_type() -> impl Parser<Out = PipeType> {
    or3(
        ">>".map(|_| PipeType::Append),
        ">".map(|_| PipeType::Write),
        "|".map(|_| PipeType::Bar),
    )
}
pub fn pipe() -> impl Parser<Out = Pipe> {
    (output_type(), pipe_type()).map(|(out, tp)| Pipe { out, tp })
}

#[derive(Debug, PartialEq, Clone)]
pub enum PExpr {
    Command(Command),
    Pipe(Loc<Pipe>, Box<Command>, Box<OLoc<PExpr>>),
}
pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, PExpr> {
    (command(), maybe((wst(locate(pipe())), o_loc(expr))))
        .map(|(l, popt)| match popt {
            Some((p, r)) => PExpr::Pipe(p, Box::new(l), Box::new(r)),
            None => PExpr::Command(l),
        })
        .parse(it)
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPart {
    Lit(Loc<String>),
    Sub(Substitution),
}

pub fn quoted() -> impl Parser<Out = (Kw, Vec<StringPart>, OKw)> {
    locate("\"")
        .then(repeat_until(string_part(), o_loc("\"")))
        .map(|(s, (m, f))| (s, m, f))
}

pub fn quoted_escape() -> impl Parser<Out = String> {
    '\\'.ig_then(or4(
        iws(1).map(|_| String::new()),
        't'.map(|_| '\t'.to_string()),
        'n'.map(|_| '\n'.to_string()),
        Any.one().map(|s| s.to_string()),
    ))
}

pub fn string_part() -> impl Parser<Out = StringPart> {
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
    Lit(Loc<String>),
    Sub(Substitution),
    Quoted(Kw, Vec<StringPart>, OKw),
}

pub fn unquoted_escape() -> impl Parser<Out = String> {
    '\\'.ig_then(or3(
        //iws(1).map(|_| String::new()),
        't'.map(|_| '\t'.to_string()),
        'n'.map(|_| '\n'.to_string()),
        Any.one().map(|s| s.to_string()),
    ))
}
pub fn unquoted_string_part() -> impl Parser<Out = UnquotedPart> {
    or3(
        locate(string_repeat(
            or(
                Any.except(" \t\n\r()@$\\\"|&><^#;[]=").min_n(1),
                unquoted_escape(),
            ),
            1,
        ))
        .map(|s| UnquotedPart::Lit(s)),
        quoted().map(|(s, q, f)| UnquotedPart::Quoted(s, q, f)),
        substitution.map(|e| UnquotedPart::Sub(e)),
    )
}

pub fn unquoted() -> impl Parser<Out = Vec<Loc<UnquotedPart>>> {
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

pub fn var() -> impl Parser<Out = PVar> {
    ident()
        .then(maybe(wst(':').ig_then(o_loc(var_type))))
        .map(|(n, t)| PVar { name: n, vtype: t })
}

pub fn ass_statement() -> impl Parser<Out = PStatement> {
    (
        l_word(or("let", "export")),
        or(
            peek(to_end()).map(|_| None),
            reflect(locate(wst(var())), o_loc(wst(ass_op())), o_loc(wst(item)))
                .brk()
                .map(|t| Some(t)),
        ),
    )
        .map(|(k, t)| match (k.v, t) {
            ("let", Some((a, b, c))) => PStatement::Let(k, a, b, c),
            ("export", Some((a, b, c))) => PStatement::Export(k, a, b, c),
            ("let", None) => PStatement::LetList(k),
            _ => PStatement::ExportList(k),
        })
}

pub fn if_statement() -> impl Parser<Out = PStatement> {
    (l_word("if"), o_loc(expr.brk())).map(|(kw, e)| PStatement::If(kw, e))
}

pub fn else_statement() -> impl Parser<Out = PStatement> {
    or(
        l_word("else").map(|e| PStatement::Else(e)),
        (l_word("elif"), o_loc(expr.brk())).map(|(kw, e)| PStatement::Elif(kw, e)),
    )
}

pub fn loop_statement() -> impl Parser<Out = PStatement> {
    or(
        (
            l_word("for"),
            (
                repeat_until(wst(o_loc(var())), wst(ol_word("in"))),
                wst(o_loc(expr)),
            )
                .brk(),
        )
            .map(|(fk, ((vars, ik), ex))| PStatement::For(fk, vars, ik, ex)),
        (l_word("while"), o_loc(expr.brk())).map(|(kw, ex)| PStatement::While(kw, ex)),
    )
}

pub fn func_def() -> impl Parser<Out = PStatement> {
    //TODO work out how function hints are written
    or(
        (l_word("fn"), peek(to_end())).map(|(kw, _)| PStatement::FuncList(kw)),
        (
            l_word("fn"),
            (
                locate(ident()),
                repeat_until_ig(wst(locate(var())), or(peek(wst('-')), to_end())),
                maybe(locate(wst("--").ig_then(Any.except("\n;").any()))),
            )
                .brk(),
        )
            .map(|(kw, (nm, vars, doc))| PStatement::FuncDef(kw, nm, doc, vars)),
    )
}

pub fn command_statement() -> impl Parser<Out = Vec<Item>> {
    repeat(wst(item), 1)
}

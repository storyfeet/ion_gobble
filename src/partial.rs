use gobble::*;

#[cfg(test)]
mod tests;

use crate::{ass_op, AssOp};

// Util Section
fn iws(n: usize) -> impl Parser<()> {
    skip_repeat(or3(" ", "\t", "\\\n"), n)
}

fn wst<A: Parser<AV>, AV>(p: A) -> impl Parser<AV> {
    //ws(0).ig_then(p)
    iws(0).ig_then(p)
}

pub fn to_end() -> impl Parser<()> {
    (
        skip_while(" \t", 0),
        maybe(("#", skip_while(Any.except(";\n"), 0))),
        (or("\n;".one().asv(()), eoi)),
    )
        .map(|_| ())
}

fn ident() -> impl Parser<String> {
    string_2_parts((Alpha, '_').min_n(1), (Alpha, NumDigit, '_').any())
}

//Main Code

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetList,
    Let(Vec<Var>, AssOp, Vec<Item>),
    ExportList,
    Export(Vec<Var>, AssOp, Vec<Item>),
    If(Expr),
    Else,
    Elif(Expr),
    End,
    While(Expr),
    For(Vec<Var>, Expr),
    Match(Item),
    Case(Item),
    //TODO work out where ion puts func description
    FuncList,
    FuncDef(String, Option<String>, Vec<Var>),
    Expr(Expr),
    Break,
    Continue,
}
pub fn statement() -> impl Parser<Statement> {
    wst(let_statement()
        .or(export_statement())
        .or(if_statement())
        .or(else_statement())
        .or(loop_statement())
        .or(func_def())
        .or(keyword("end").map(|_| Statement::End))
        .or(keyword("break").map(|_| Statement::Break))
        .or(keyword("continue").map(|_| Statement::Continue))
        .or((keyword("match"), wst(item)).map(|(_, i)| Statement::Match(i)))
        .or((keyword("case"), wst(item)).map(|(_, i)| Statement::Case(i))))
    .or(expr.map(|e| Statement::Expr(e)))
    //.or(command_statement().map(|c| Statement::Command(c)))
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

#[derive(Debug, PartialEq)]
pub enum Index {
    Pos(Item),
    Range(Option<Item>, Option<Item>),
    RangeInc(Option<Item>, Option<Item>),
}

pub fn index() -> impl Parser<Index> {
    "[".ig_then(
        (wst(item))
            .then_ig("]")
            .map(|i| Index::Pos(i))
            // Or do with ranges
            .or(maybe(wst(item))
                .then(wst("..=".or("..")))
                .then(maybe(wst(item)))
                .then_ig(wst("]"))
                .map(|((l, op), r)| match op {
                    "..=" => Index::RangeInc(l, r),
                    ".." => Index::Range(l, r),
                    _ => Index::Range(l, r),
                })),
    )
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Bool(bool),
    Int(isize),
    Float(f64),
    Str(Vec<UnquotedPart>),
    Array(Vec<Item>),
    Quoted(Vec<StringPart>),
    Sub(Box<Substitution>),
}
pub fn item<'a>(it: &LCChars<'a>) -> ParseRes<'a, Item> {
    //TODO float
    let p = substitution()
        .map(|s| Item::Sub(Box::new(s)))
        .or(quoted().map(|q| Item::Quoted(q)))
        .or(common_bool.map(|b| Item::Bool(b)))
        .or("["
            .ig_then(repeat_until_ig(wst(item), wst("]")))
            .map(|l| Item::Array(l)))
        .or(common_float.map(|f| Item::Float(f)))
        .or(common_int.map(|n| Item::Int(n)))
        .or(unquoted().map(|i| Item::Str(i)));
    p.parse(it)
}

#[derive(Debug, PartialEq)]
pub struct Substitution {
    sub: Sub,
    index: Option<Index>,
}
pub fn substitution() -> impl Parser<Substitution> {
    (sub(), maybe(index())).map(|(sub, index)| Substitution { sub, index })
}

#[derive(Debug, PartialEq)]
pub enum Sub {
    Var(String),
    DollarB(Expr),
    AtVar(String),
    AtB(Expr),
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

#[derive(Debug, PartialEq)]
pub struct Var {
    name: String,
    vtype: Option<VarType>,
}

#[derive(Debug, PartialEq)]
pub struct Command {
    v: Vec<Item>,
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    Command(Command),
    Pipe(Pipe, Box<Command>, Box<Expr>),
}
pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, Expr> {
    (command(), maybe((wst(pipe()), expr)))
        .map(|(l, popt)| match popt {
            Some((p, r)) => Expr::Pipe(p, Box::new(l), Box::new(r)),
            None => Expr::Command(l),
        })
        .parse(it)
}

#[derive(Debug, PartialEq)]
pub enum StringPart {
    Lit(String),
    Sub(Substitution),
}
pub fn quoted() -> impl Parser<Vec<StringPart>> {
    '"'.ig_then(repeat_until_ig(string_part(), '"'))
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
        string_repeat(or(Any.except("@$\"\\").min_n(1), quoted_escape()), 1)
            .map(|s| StringPart::Lit(s)),
        substitution().map(|e| StringPart::Sub(e)),
    )
}

#[derive(Debug, PartialEq)]
pub enum UnquotedPart {
    Lit(String),
    Sub(Substitution),
    Quoted(Vec<StringPart>),
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
        string_repeat(
            or(
                Any.except(" \n\r()@$\\\"|&><^#;").min_n(1),
                unquoted_escape(),
            ),
            1,
        )
        .map(|s| UnquotedPart::Lit(s)),
        quoted().map(|q| UnquotedPart::Quoted(q)),
        substitution().map(|e| UnquotedPart::Sub(e)),
    )
}

pub fn unquoted() -> impl Parser<Vec<UnquotedPart>> {
    repeat(unquoted_string_part(), 1)
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

pub fn var() -> impl Parser<Var> {
    ident()
        .then(maybe(wst(':').ig_then(var_type)))
        .map(|(n, t)| Var { name: n, vtype: t })
}

pub fn let_statement() -> impl Parser<Statement> {
    keyword("let").ig_then(
        or(
            peek(to_end()).map(|_| Statement::LetList),
            reflect(wst(var()), wst(ass_op()), wst(item)).map(|(a, b, c)| Statement::Let(a, b, c)),
        )
        .brk(),
    )
}

pub fn export_statement() -> impl Parser<Statement> {
    keyword("export").ig_then(
        or(
            peek(to_end()).map(|_| Statement::ExportList),
            reflect(wst(var()), wst(ass_op()), wst(item))
                .map(|(a, b, c)| Statement::Export(a, b, c)),
        )
        .brk(),
    )
}

pub fn if_statement() -> impl Parser<Statement> {
    keyword("if").ig_then(expr.brk()).map(|e| Statement::If(e))
}

pub fn else_statement() -> impl Parser<Statement> {
    keyword("else").map(|_| Statement::Else).or(keyword("elif")
        .ig_then(expr.brk())
        .map(|e| Statement::Elif(e)))
}

pub fn loop_statement() -> impl Parser<Statement> {
    keyword("for")
        .ig_then(
            repeat_until_ig(wst(var()), wst(keyword("in")))
                .then(wst(expr))
                .brk(),
        )
        .map(|(vars, ex)| Statement::For(vars, ex))
        .or(keyword("while")
            .ig_then(expr.brk())
            .map(|ex| Statement::While(ex)))
}

pub fn func_def() -> impl Parser<Statement> {
    //TODO work out how function hints are written
    keyword("fn").ig_then(or(
        peek(to_end()).map(|_| Statement::FuncList),
        (
            wst(ident()),
            repeat(wst(var()), 1).brk(),
            maybe(wst("--").ig_then(Any.except("\n;").any())),
        )
            .map(|(nm, vars, doc)| Statement::FuncDef(nm, doc, vars)),
    ))
}

pub fn command_statement() -> impl Parser<Vec<Item>> {
    repeat(wst(item), 1)
}

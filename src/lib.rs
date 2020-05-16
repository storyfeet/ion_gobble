use gobble::*;

pub mod partial;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub enum AssOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Concat,
    PreConcat,
}
pub fn ass_op() -> impl Parser<AssOp> {
    or3(
        "=".map(|_| AssOp::Assign),
        or4(
            "+=".map(|_| AssOp::Add),
            "-=".map(|_| AssOp::Sub),
            "*=".map(|_| AssOp::Mul),
            "/=".map(|_| AssOp::Div),
        ),
        or(
            "++=".map(|_| AssOp::Concat),
            "::=".map(|_| AssOp::PreConcat),
        ),
    )
}

// Util Section
fn iws(n: usize) -> impl Parser<()> {
    skip_repeat(or4(" ", "\t", "\\\n", "\\;"), n)
}

fn wst<A: Parser<AV>, AV>(p: A) -> impl Parser<AV> {
    //ws(0).ig_then(p)
    iws(0).ig_then(p)
}

pub fn to_end() -> impl Parser<()> {
    (
        skip_repeat(
            or4(
                ("\\\n", fail_on(eoi)).asv(()),
                ("\\;", maybe('\n'), fail_on(eoi)).asv(()), //escape ; for easier tests
                " \t".min_n(1).asv(()),
                ("#", skip_while(Any.except(";\n"), 0)).asv(()),
            ),
            0,
        ),
        or("\n;".one().asv(()), eoi),
    )
        .asv(())
}

fn ident() -> impl Parser<String> {
    string_2_parts((Alpha, '_').min_n(1), (Alpha, NumDigit, '_').any())
}

//Main Code

#[derive(Debug, PartialEq, Clone)]
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
            keyword("end").map(|_| Statement::End),
            keyword("break").map(|_| Statement::Break),
            keyword("continue").map(|_| Statement::Continue),
            (keyword("match"), wst(item)).map(|(_, i)| Statement::Match(i)),
            (keyword("case"), wst(item)).map(|(_, i)| Statement::Case(i)),
            expr.map(|e| Statement::Expr(e)),
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
    let p = substitution
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

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    name: String,
    vtype: Option<VarType>,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
        substitution.map(|e| StringPart::Sub(e)),
    )
}

#[derive(Debug, PartialEq, Clone)]
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
                Any.except(" \n\r()@$\\\"|&><^#;[]").min_n(1),
                unquoted_escape(),
            ),
            1,
        )
        .map(|s| UnquotedPart::Lit(s)),
        quoted().map(|q| UnquotedPart::Quoted(q)),
        substitution.map(|e| UnquotedPart::Sub(e)),
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
            repeat(wst(var()), 0),
            maybe(wst("--").ig_then(Any.except("\n;").any())),
        )
            .brk()
            .map(|(nm, vars, doc)| Statement::FuncDef(nm, doc, vars)),
    ))
}

pub fn command_statement() -> impl Parser<Vec<Item>> {
    repeat(wst(item), 1)
}

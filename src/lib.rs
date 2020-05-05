use gobble::*;

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
}

// Util Section

fn wst<A: Parser<AV>, AV>(p: A) -> impl Parser<AV> {
    ws(0).ig_then(p)
}

pub fn to_end() -> impl Parser<()> {
    skip_while(|x| x == ' ' || x == '\t', 0).then_ig(tag("\n").or(tag(";")).map(|_| ()).or(eoi))
}

fn ident() -> impl Parser<String> {
    read_fs(is_alpha, 1)
        .then(read_fs(is_alpha_num, 0))
        .map(|(mut a, b)| {
            a.push_str(&b);
            a
        })
}

//Main Code

#[derive(Debug)]
pub enum Statement {
    LetList,
    Let(Vec<Var>, Option<Op>, Vec<Item>),
    ExportList,
    Export(Vec<Var>, Option<Op>, Vec<Item>),
    If(Expr),
    Else,
    Elif(Expr),
    End,
    While(Expr),
    For(Vec<Var>, Expr),
    //TODO
    FuncDef(String, Option<String>, Vec<Var>),
    Break,
    Continue,
}

#[derive(Debug, PartialEq)]
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
    tag("[").ig_then(
        (wst(item))
            .then_ig(tag("]"))
            .map(|i| Index::Pos(i))
            .or(maybe(wst(item))
                .then(wst(tag("..=").or(tag(".."))))
                .then(maybe(wst(item)))
                .then_ig(wst(tag("]")))
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
    Str(String),
    Quoted(Vec<StringPart>),
    Sub(Box<Substitution>),
}
pub fn item<'a>(it: &LCChars<'a>) -> ParseRes<'a, Item> {
    let p = substitution()
        .map(|s| Item::Sub(Box::new(s)))
        .or(quoted().map(|q| Item::Quoted(q)))
        .or(ident().map(|i| Item::Str(i)));
    p.parse(it)
}

#[derive(Debug, PartialEq)]
pub struct Substitution {
    sub: Sub,
    index: Option<Index>,
}
pub fn substitution() -> impl Parser<Substitution> {
    sub()
        .then(maybe(index()))
        .map(|(sub, index)| Substitution { sub, index })
}

#[derive(Debug, PartialEq)]
pub enum Sub {
    Var(String),
    DollarB(Expr),
    AtVar(String),
    AtB(Expr),
}
pub fn sub() -> impl Parser<Sub> {
    tag("$(")
        .ig_then(wst(expr))
        .then_ig(wst(tag(")")))
        .map(|e| Sub::DollarB(e))
        .or(tag("$").ig_then(ident()).map(|i| Sub::Var(i)))
        .or(tag("@(")
            .ig_then(wst(expr))
            .then_ig(wst(tag(")")))
            .map(|e| Sub::AtB(e)))
        .or(tag("@").ig_then(ident()).map(|i| Sub::AtVar(i)))
}

#[derive(Debug, PartialEq)]
pub struct Var {
    name: String,
    vtype: Option<VarType>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Command(Vec<Item>),
    Pipe(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum StringPart {
    Lit(String),
    Esc(char),
    Sub(Substitution),
}
pub fn quoted() -> impl Parser<Vec<StringPart>> {
    tag("\"").ig_then(repeat_until_ig(string_part(), tag("\"")))
}

pub fn string_part() -> impl Parser<StringPart> {
    (read_fs(|c| c != '@' && c != '$' && c != '"' && c != '\\', 1).map(|s| StringPart::Lit(s)))
        .or(substitution().map(|e| StringPart::Sub(e)))
        .or(tag("\\").ig_then(take_char).map(|c| StringPart::Esc(c)))
}

pub fn op() -> impl Parser<Op> {
    s_tag("+").map(|_| Op::Add)
}

pub fn var_type<'a>(it: &LCChars<'a>) -> ParseRes<'a, VarType> {
    wst(keyword("str")
        .map(|_| VarType::Str)
        .or(keyword("bool").map(|_| VarType::Bool))
        .or(keyword("float").map(|_| VarType::Float))
        .or(keyword("int").map(|_| VarType::Int))
        .or(tag("[")
            .ig_then(var_type)
            .then_ig(wst(tag("]")))
            .map(|c| VarType::Arr(Box::new(c)))))
    .parse(it)
}

pub fn var() -> impl Parser<Var> {
    ident()
        .then(maybe(wst(tag(":")).ig_then(var_type)))
        .map(|(n, t)| Var { name: n, vtype: t })
}

pub fn let_statement() -> impl Parser<Statement> {
    keyword("let").ig_then(
        maybe(reflect(
            wst(var()),
            wst(maybe(op()).then_ig(tag("="))),
            wst(expr),
        ))
        .map(|op| match op {
            Some((a, b, c)) => Statement::Let(a, b, c),
            None => Statement::LetList,
        }),
    )
}

pub fn export_statement() -> impl Parser<Statement> {
    keyword("export").ig_then(
        maybe(reflect(
            wst(var()),
            wst(maybe(op()).then_ig(tag("="))),
            wst(expr),
        ))
        .map(|op| match op {
            Some((a, b, c)) => Statement::Export(a, b, c),
            None => Statement::ExportList,
        }),
    )
}

pub fn if_statement() -> impl Parser<Statement> {
    keyword("if").ig_then(expr).map(|e| Statement::If(e))
}

pub fn else_statement() -> impl Parser<Statement> {
    keyword("else")
        .map(|_| Statement::Else)
        .or(keyword("elif").ig_then(expr).map(|e| Statement::Elif(e)))
}

pub fn loop_statement() -> impl Parser<Statement> {
    keyword("for")
        .ig_then(repeat_until_ig(wst(var()), wst(tag("in "))).then(wst(expr)))
        .map(|(vars, ex)| Statement::For(vars, ex))
        .or(tag("while ").ig_then(expr).map(|ex| Statement::While(ex)))
}

pub fn func_def() -> impl Parser<Statement> {
    //TODO work out how function hints are written
    keyword("fn")
        .ig_then(wst(ident()))
        .then(repeat(wst(var()), 1))
        .map(|(nm, vars)| Statement::FuncDef(nm, None, vars))
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
        .or(keyword("continue").map(|_| Statement::Continue)))
    .then_ig(to_end())
}

pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, Expr> {
    let p = repeat(wst(item), 1).map(|l| Expr::Command(l));
    p.parse(it)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_string_handles_escapes_correctly() {
        let st = quoted()
            .parse_s(r#""there are no \n \t $("53 +4")""#)
            .unwrap();
        let mut it = st.into_iter();
        assert_eq!(
            it.next(),
            Some(StringPart::Lit("there are no ".to_string()))
        );
        assert_eq!(it.next(), Some(StringPart::Esc('n')));
        assert_eq!(it.next(), Some(StringPart::Lit(" ".to_string())));
        assert_eq!(it.next(), Some(StringPart::Esc('t')));
    }
}

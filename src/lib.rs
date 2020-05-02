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
    Let(Vec<Var>, Option<Op>, Vec<Expr>),
    ExportList,
    Export(Vec<Var>, Option<Op>, Vec<Expr>),
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

#[derive(Debug)]
pub enum VarType {
    Str,
    Bool,
    Int,
    Float,
    Arr(Box<VarType>),
}

#[derive(Debug)]
pub struct Var {
    name: String,
    vtype: Option<VarType>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Null,
    Quoted(Vec<StringPart>),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    line: usize,
    col: usize,
    d: TokenData,
}

#[derive(Debug, PartialEq)]
pub enum TokenData {
    Let,
    For,
    Pipe,
    DollarOpen,
    Close,
    Ident(String), //Just the Var name
    Var(String),   //$Ident
}

#[derive(Debug, PartialEq)]
pub enum StringPart {
    Lit(String),
    Esc(char),
    Ident(String),
    Expr(Expr),
}

pub fn quoted() -> impl Parser<Vec<StringPart>> {
    tag("\"").ig_then(repeat_until_ig(string_part(), tag("\"")))
}

pub fn string_part() -> impl Parser<StringPart> {
    (read_fs(|c| c != '$' && c != '"' && c != '\\', 1).map(|s| StringPart::Lit(s)))
        .or(tag("$").ig_then(ident()).map(|id| StringPart::Ident(id)))
        .or(tag("$(")
            .ig_then(expr)
            .then_ig(tag(")"))
            .map(|e| StringPart::Expr(e)))
        //TODO conside other escape options
        .or(tag("\\").ig_then(take_char).map(|c| StringPart::Esc(c)))
}

pub fn op() -> impl Parser<Op> {
    s_tag("+").map(|_| Op::Add)
}

pub fn var_type<'a>(it: &LCChars<'a>) -> ParseRes<'a, VarType> {
    wst(tag("str")
        .map(|_| VarType::Str)
        .or(keyword("bool").map(|_| VarType::Bool))
        .or(keyword("float").map(|_| VarType::Float))
        .or(keyword("int").map(|_| VarType::Int))
        .or(tag("[").ig_then(var_type).then_ig(wst(tag("]")))))
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

pub fn statement() -> impl Parser<Statement> {
    wst(let_statement()
        .or(export_statement())
        .or(if_statement())
        .or(else_statement())
        .or(loop_statement())
        .or(keyword("end").map(|_| Statement::End))
        .or(keyword("break").map(|_| Statement::Break))
        .or(keyword("continue").map(|_| Statement::Continue)))
    .then_ig(to_end())
}

pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, Expr> {
    let p = quoted().map(|r| Expr::Quoted(r));
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

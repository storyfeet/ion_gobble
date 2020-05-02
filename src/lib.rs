use gobble::*;

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

//Main Code

pub enum Statement {
    LetList,
    Let(Vec<String>, Option<Op>, Vec<Expr>),
    ExportList,
    Export(Vec<String>, Option<Op>, Vec<Expr>),
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

pub fn let_statement() -> impl Parser<Statement> {
    wst(tag("let").then_ig(to_end()).map(|_| Statement::LetList)).or(tag("let ")
        .ig_then(reflect(
            wst(ident()),
            wst(maybe(op()).then_ig(tag("="))),
            wst(expr),
        ))
        .map(|(a, b, c)| Statement::Let(a, b, c)))
}

pub fn export_statement() -> impl Parser<Statement> {
    wst(tag("export").then_ig(to_end()).map(|_| Statement::LetList)).or(tag("export ")
        .ig_then(reflect(
            wst(ident()),
            wst(maybe(op()).then_ig(tag("="))),
            wst(expr),
        ))
        .map(|(a, b, c)| Statement::Export(a, b, c)))
}

pub fn statement() -> impl Parser<Statement> {
    let_statement().or(export_statement())
}

pub fn expr<'a>(it: &LCChars<'a>) -> ParseRes<'a, Expr> {
    let p = quoted().map(|r| Expr::Quoted(r));
    p.parse(it)
}

fn ident() -> impl Parser<String> {
    read_fs(is_alpha, 1)
        .then(read_fs(is_alpha_num, 0))
        .map(|(mut a, b)| {
            a.push_str(&b);
            a
        })
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

use gobble::*;

pub enum Op {
    Add,
    Sub,
}

pub enum Statement {
    LetList,
    Let(String, Expr),
    LetOp(String, Op, Expr),
    ExportList,
    Export(String, Expr),
    ExportOp(String, Op, Expr),
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

pub fn to_end() -> impl Parser<()> {
    skip_while(|x| x == ' ' || x == '\t', 0).then_ig(tag("\n").or(tag(";")).map(|_| ()).or(eoi))
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
    tag("\"").ig_then(repeat_until(string_part(), tag("\"")))
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
    (s_tag("let").then_ig(to_end()).map(|_| Statement::LetList)).or(s_tag("let")
        .ig_then(ident())
        .then(maybe(op()))
        .then_ig(s_tag("="))
        .then(expr)
        .then_ig(to_end())
        .map(|((id, op), ex)| Statement::Let())
}

pub fn statement() -> impl Parser<Statement> {
    let_statement()
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

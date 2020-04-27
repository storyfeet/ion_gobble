use gobble::*;

pub enum Statement {}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Null,
    Quoted(Vec<StringPart>),
}

#[derive(Debug, PartialEq)]
pub enum StringPart {
    Lit(String),
    Esc(char),
    Var(String),
    Expr(Expr),
}

pub fn quoted() -> impl Parser<Vec<StringPart>> {
    tag("\"").ig_then(repeat_until(string_part(), tag("\"")))
}

pub fn string_part() -> impl Parser<StringPart> {
    (read_fs(|c| c != '$' && c != '"' && c != '\\', 1).map(|s| StringPart::Lit(s)))
        .or(tag("$").ig_then(ident()).map(|id| StringPart::Var(id)))
        .or(tag("$(")
            .ig_then(expr)
            .then_ig(tag(")"))
            .map(|e| StringPart::Expr(e)))
        //TODO conside other escape options
        .or(tag("\\").ig_then(take_char).map(|c| StringPart::Esc(c)))
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

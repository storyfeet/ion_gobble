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

#[test]
fn test_statements() {
    let p = statement();
    assert_eq!(
        p.parse_sn("do thing;other"),
        (
            "other",
            Statement::Expr(Expr::Command({
                v: vec![Item::Str({ vec![UnquotedPart::Lit("do".to_string())] })]
            }))
        )
    );
}

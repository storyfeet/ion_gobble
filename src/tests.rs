use super::*;
#[test]
fn test_string_handles_escapes_correctly() {
    let st = quoted()
        .parse_s(r#""there are no \n \t $("53 +4")""#)
        .unwrap();
    let mut it = st.into_iter();
    assert_eq!(
        it.next(),
        Some(StringPart::Lit("there are no \n \t ".to_string()))
    );
    match it.next() {
        Some(StringPart::Sub(_)) => {}
        Some(n) => panic!("Expected sub got {:?}", n),
        None => panic!("Ended before Sob"),
    }
}

fn simple_item(s: &str) -> Item {
    Item::Str(vec![UnquotedPart::Lit(s.to_string())])
}

#[test]
pub fn test_statements() {
    let p = statement();
    assert_eq!(
        p.parse_sn("do thing;other").unwrap(),
        (
            "other",
            Statement::Expr(Expr::Command(Command {
                env: None,
                v: vec![simple_item("do"), simple_item("thing")],
            }))
        )
    );
}

#[test]
fn test_ident() {
    let p = ident();
    assert_eq!(p.parse_sn("ident["), Ok(("[", "ident".to_string())));
}

#[test]
fn test_index_creats_valid_range() {
    let p = index();
    let r1 = Range {
        start: Some(RangeEnd::Int(3)),
        fin: None,
        op: None,
    };
    let r2 = Range {
        start: None,
        op: Some(RangeOp::Inc),
        fin: Some(RangeEnd::Int(4)),
    };
    let r3 = Range {
        start: Some(RangeEnd::Sub(Substitution {
            sub: Sub::Var("hello".to_string()),
            index: None,
        })),
        op: Some(RangeOp::Exc),
        fin: Some(RangeEnd::Int(8)),
    };

    assert_eq!(p.parse_s("[3]"), Ok(Index(vec![r1.clone()])));

    assert_eq!(
        p.parse_s("[3 ..=4]").unwrap(),
        Index(vec![r1.clone(), r2.clone()])
    );
    assert_eq!(
        p.parse_s("[$hello..8 3]").unwrap(),
        Index(vec![r3.clone(), r1.clone()]),
    );
}

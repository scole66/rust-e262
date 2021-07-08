use super::*;

#[test]
fn token_type_debug() {
    assert_ne!(format!("{:?}", TokenType::Numeric), "");
}
#[test]
#[allow(clippy::clone_on_copy)]
fn token_type_clone() {
    let t1 = TokenType::Numeric;
    let t2 = t1.clone();
    assert!(matches!(t2, TokenType::Numeric));
    assert!(false);
}
#[test]
fn token_type_display() {
    for (tt, disp) in vec![
        (TokenType::Keyword, "Keyword"),
        (TokenType::Punctuator, "Punctuator"),
        (TokenType::IdentifierName, "IdentifierName"),
        (TokenType::RegularExpression, "RegularExpressionLiteral"),
        (TokenType::Numeric, "Numeric"),
        (TokenType::String, "String"),
        (TokenType::NoSubTemplate, "NoSubTemplate"),
        (TokenType::TemplateHead, "TemplateHead"),
        (TokenType::TemplateMiddle, "TemplateMiddle"),
        (TokenType::TemplateTail, "TemplateTail"),
        (TokenType::PrivateIdentifier, "PrivateIdentifier"),
    ] {
        assert_eq!(format!("{}", tt), disp);
    }
}

#[test]
#[allow(clippy::clone_on_copy)]
fn spot_clone() {
    let s1 = Spot::Final;
    let s2 = s1.clone();
    assert!(matches!(s2, Spot::Final));
}

#[test]
fn prettypad_01() {
    let (first, successive) = prettypad("padding", Spot::Initial);
    assert_eq!(first, "padding");
    assert_eq!(successive, "padding");
}
#[test]
fn prettypad_02() {
    let (first, successive) = prettypad("padding", Spot::NotFinal);
    assert_eq!(first, "padding├── ");
    assert_eq!(successive, "padding│   ");
}
#[test]
fn prettypad_03() {
    let (first, successive) = prettypad("padding", Spot::Final);
    assert_eq!(first, "padding└── ");
    assert_eq!(successive, "padding    ");
}

#[test]
fn pprint_token_01() {
    let mut msg = Vec::new();
    pprint_token(&mut msg, "anything", TokenType::Keyword, "-pad-", Spot::NotFinal).unwrap();
    let whole_message = std::str::from_utf8(&msg).unwrap();
    assert_eq!(whole_message, "-pad-├── Keyword: anything\n");
}

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

struct TestProduction {}
impl fmt::Display for TestProduction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tp")
    }
}
impl PrettyPrint for TestProduction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}TestProduction: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}TestProduction: {}", first, self)
    }
}

#[test]
fn pretty_print_testprod() {
    let tp = TestProduction {};
    let mut msg = Vec::new();
    tp.pprint(&mut msg).unwrap();
    let whole_message = std::str::from_utf8(&msg).unwrap();
    assert_eq!(whole_message, "TestProduction: tp\n");
}

#[test]
fn concise_print_testprod() {
    let tp = TestProduction {};
    let mut msg = Vec::new();
    tp.pprint_concise(&mut msg).unwrap();
    let whole_message = std::str::from_utf8(&msg).unwrap();
    assert_eq!(whole_message, "TestProduction: tp\n");
}

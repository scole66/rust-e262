use super::testhelp::newparser;
use super::*;
use test_case::test_case;

#[test_case("import(a.#valid)" => true; "valid")]
#[test_case("import(a.#invalid)" => false; "invalid")]
fn import_call_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ImportCall::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

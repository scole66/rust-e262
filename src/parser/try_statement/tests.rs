use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// TRY STATEMENT
#[test]
fn try_statement_test_01() {
    let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } catch {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 19);
    pretty_check(&*node, "TryStatement: try { a ; } catch { }", vec!["Block: { a ; }", "Catch: catch { }"]);
    concise_check(&*node, "TryStatement: try { a ; } catch { }", vec!["Keyword: try", "Block: { a ; }", "Catch: catch { }"]);
    format!("{:?}", node);
}
#[test]
fn try_statement_test_02() {
    let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } finally {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 21);
    pretty_check(&*node, "TryStatement: try { a ; } finally { }", vec!["Block: { a ; }", "Finally: finally { }"]);
    concise_check(&*node, "TryStatement: try { a ; } finally { }", vec!["Keyword: try", "Block: { a ; }", "Finally: finally { }"]);
    format!("{:?}", node);
}
#[test]
fn try_statement_test_03() {
    let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } catch { b; } finally { c; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 38);
    pretty_check(&*node, "TryStatement: try { a ; } catch { b ; } finally { c ; }", vec!["Block: { a ; }", "Catch: catch { b ; }", "Finally: finally { c ; }"]);
    concise_check(&*node, "TryStatement: try { a ; } catch { b ; } finally { c ; }", vec!["Keyword: try", "Block: { a ; }", "Catch: catch { b ; }", "Finally: finally { c ; }"]);
    format!("{:?}", node);
}
#[test]
fn try_statement_test_err_01() {
    check_err(TryStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘try’ expected", 1, 1);
}
#[test]
fn try_statement_test_err_02() {
    check_err(TryStatement::parse(&mut newparser("try"), Scanner::new(), false, false, true), "‘{’ expected", 1, 4);
}
#[test]
fn try_statement_test_err_03() {
    check_err(TryStatement::parse(&mut newparser("try {}"), Scanner::new(), false, false, true), "Catch or Finally block expected", 1, 7);
}
#[test]
fn try_statement_test_prettyerrors_1() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch (e) { console.log(`Got the error ${e}.`); }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn try_statement_test_prettyerrors_2() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } finally { a; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn try_statement_test_prettyerrors_3() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch(foo) { print(foo); } finally { a; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn try_statement_test_conciseerrors_1() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch (e) { console.log(`Got the error ${e}.`); }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn try_statement_test_conciseerrors_2() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } finally { a; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn try_statement_test_conciseerrors_3() {
    let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch(foo) { print(foo); } finally { a; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
fn try_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn try_statement_test_var_declared_names() {
    try_vdn_check("try{var a;}catch(b){var c;}", &["a", "c"]);
    try_vdn_check("try{var a;}finally{var c;}", &["a", "c"]);
    try_vdn_check("try{var a;}catch(b){var c;}finally{var d;}", &["a", "c", "d"]);
}
fn try_cubt_check(src: &str) {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn try_statement_test_contains_undefined_break_target() {
    try_cubt_check("try{break t;}catch(b){}");
    try_cubt_check("try{}catch(b){break t;}");
    try_cubt_check("try{break t;}finally{}");
    try_cubt_check("try{}finally{break t;}");
    try_cubt_check("try{break t;}catch(b){}finally{}");
    try_cubt_check("try{}catch(b){break t;}finally{}");
    try_cubt_check("try{}catch(b){}finally{break t;}");
}
fn try_contains_check(src: &str, has_literal: bool) {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn try_statement_test_contains() {
    try_contains_check("try{0;}catch(b){}", true);
    try_contains_check("try{}catch(b){0;}", true);
    try_contains_check("try{}catch(b){}", false);
    try_contains_check("try{0;}finally{}", true);
    try_contains_check("try{}finally{0;}", true);
    try_contains_check("try{}finally{}", false);
    try_contains_check("try{0;}catch(b){}finally{}", true);
    try_contains_check("try{}catch(b){0;}finally{}", true);
    try_contains_check("try{}catch(b){}finally{0;}", true);
    try_contains_check("try{}catch(b){}finally{}", false);
}
fn try_cdl_check(src: &str) {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn try_statement_test_contains_duplicate_labels() {
    try_cdl_check("try{t:;}catch{}");
    try_cdl_check("try{}catch{t:;}");
    try_cdl_check("try{t:;}finally{}");
    try_cdl_check("try{}finally{t:;}");
    try_cdl_check("try{t:;}catch{}finally{}");
    try_cdl_check("try{}catch{t:;}finally{}");
    try_cdl_check("try{}catch{}finally{t:;}");
}
#[test_case("try { continue x; } catch { }" => (false, true); "try { continue x; } catch { }")]
#[test_case("try { } catch { continue x; }" => (false, true); "try { } catch { continue x; }")]
#[test_case("try { continue x; } finally { }" => (false, true); "try { continue x; } finally { }")]
#[test_case("try { } finally { continue x; }" => (false, true); "try { } finally { continue x; }")]
#[test_case("try { continue x; } catch { } finally { }" => (false, true); "try { continue x; } catch { } finally { }")]
#[test_case("try { } catch { continue x; } finally { }" => (false, true); "try { } catch { continue x; } finally { }")]
#[test_case("try { } catch { } finally { continue x; }" => (false, true); "try { } catch { } finally { continue x; }")]
fn try_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("try {a.#valid;} catch{}" => true; "tc: block valid")]
#[test_case("try {} catch{a.#valid;}" => true; "tc: catch valid")]
#[test_case("try {a.#valid} finally{}" => true; "tf: block valid")]
#[test_case("try {} finally{a.#valid;}" => true; "tf: finally valid")]
#[test_case("try{a.#valid;} catch{} finally{}" => true; "tcf: block valid")]
#[test_case("try{} catch{a.#valid;} finally{}" => true; "tcf: catch valid")]
#[test_case("try{} catch{} finally{a.#valid;}" => true; "tcf: finally valid")]
#[test_case("try {a.#invalid;} catch{}" => false; "tc: block invalid")]
#[test_case("try {} catch{a.#invalid;}" => false; "tc: catch invalid")]
#[test_case("try {a.#invalid} finally{}" => false; "tf: block invalid")]
#[test_case("try {} finally{a.#invalid;}" => false; "tf: finally invalid")]
#[test_case("try{a.#invalid;} catch{} finally{}" => false; "tcf: block invalid")]
#[test_case("try{} catch{a.#invalid;} finally{}" => false; "tcf: catch invalid")]
#[test_case("try{} catch{} finally{a.#invalid;}" => false; "tcf: finally invalid")]
fn try_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = TryStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod try_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        TryStatement::parse(&mut newparser("try{}finally{}"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// CATCH
#[test]
fn catch_test_01() {
    let (node, scanner) = check(Catch::parse(&mut newparser("catch {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "Catch: catch { }", vec!["Block: { }"]);
    concise_check(&*node, "Catch: catch { }", vec!["Keyword: catch", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn catch_test_02() {
    let (node, scanner) = check(Catch::parse(&mut newparser("catch (e) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "Catch: catch ( e ) { }", vec!["CatchParameter: e", "Block: { }"]);
    concise_check(&*node, "Catch: catch ( e ) { }", vec!["Keyword: catch", "Punctuator: (", "IdentifierName: e", "Punctuator: )", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn catch_test_err_01() {
    check_err(Catch::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘catch’ expected", 1, 1);
}
#[test]
fn catch_test_err_02() {
    check_err(Catch::parse(&mut newparser("catch"), Scanner::new(), false, false, true), "( or { expected", 1, 6);
}
#[test]
fn catch_test_err_03() {
    check_err(Catch::parse(&mut newparser("catch("), Scanner::new(), false, false, true), "CatchParameter expected", 1, 7);
}
#[test]
fn catch_test_err_04() {
    check_err(Catch::parse(&mut newparser("catch(e"), Scanner::new(), false, false, true), "‘)’ expected", 1, 8);
}
#[test]
fn catch_test_err_05() {
    check_err(Catch::parse(&mut newparser("catch(e)"), Scanner::new(), false, false, true), "‘{’ expected", 1, 9);
}
#[test]
fn catch_test_prettyerrors_1() {
    let (item, _) = Catch::parse(&mut newparser("catch { a; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn catch_test_prettyerrors_2() {
    let (item, _) = Catch::parse(&mut newparser("catch (abcd) { a; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn catch_test_conciseerrors_1() {
    let (item, _) = Catch::parse(&mut newparser("catch { a; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn catch_test_conciseerrors_2() {
    let (item, _) = Catch::parse(&mut newparser("catch (abcd) { a; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn catch_test_var_declared_names() {
    let (item, _) = Catch::parse(&mut newparser("catch{var a;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn catch_test_contains_undefined_break_target() {
    let (item, _) = Catch::parse(&mut newparser("catch{break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
fn catch_contains_check(src: &str, has_literal: bool) {
    let (item, _) = Catch::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn catch_test_contains() {
    catch_contains_check("catch{}", false);
    catch_contains_check("catch{0;}", true);
    catch_contains_check("catch(a){}", false);
    catch_contains_check("catch([a=0]){}", true);
    catch_contains_check("catch(a){0;}", true);
}
#[test]
fn catch_test_contains_duplicate_labels() {
    let (item, _) = Catch::parse(&mut newparser("catch{t:;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("catch { continue x; }" => (false, true); "catch { continue x; }")]
#[test_case("catch (e) { continue x; }" => (false, true); "catch (e) { continue x; }")]
fn catch_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = Catch::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("catch([a=b.#valid]){}" => true; "cpb: param valid")]
#[test_case("catch([a=b]){c.#valid;}" => true; "cpb: block valid")]
#[test_case("catch{a.#valid;}" => true; "cb: block valid")]
#[test_case("catch([a=b.#invalid]){}" => false; "cpb: param invalid")]
#[test_case("catch([a=b]){c.#invalid;}" => false; "cpb: block invalid")]
#[test_case("catch{a.#invalid;}" => false; "cb: block invalid")]
fn catch_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Catch::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod catch {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        Catch::parse(&mut newparser("catch{}"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// FINALLY
#[test]
fn finally_test_01() {
    let (node, scanner) = check(Finally::parse(&mut newparser("finally {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "Finally: finally { }", vec!["Block: { }"]);
    concise_check(&*node, "Finally: finally { }", vec!["Keyword: finally", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn finally_test_err_01() {
    check_err(Finally::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘finally’ expected", 1, 1);
}
#[test]
fn finally_test_err_02() {
    check_err(Finally::parse(&mut newparser("finally"), Scanner::new(), false, false, true), "‘{’ expected", 1, 8);
}
#[test]
fn finally_test_prettyerrors_1() {
    let (item, _) = Finally::parse(&mut newparser("finally { a; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn finally_test_conciseerrors_1() {
    let (item, _) = Finally::parse(&mut newparser("finally { a; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn finally_test_var_declared_names() {
    let (item, _) = Finally::parse(&mut newparser("finally{var a;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn finally_test_contains_undefined_break_target() {
    let (item, _) = Finally::parse(&mut newparser("finally{break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
fn finally_contains_check(src: &str, has_literal: bool) {
    let (item, _) = Finally::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn finally_test_contains() {
    finally_contains_check("finally{}", false);
    finally_contains_check("finally{0;}", true);
}
#[test]
fn finally_test_contains_duplicate_labels() {
    let (item, _) = Finally::parse(&mut newparser("finally{t:;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("finally { continue x; }" => (false, true); "finally { continue x; }")]
fn finally_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = Finally::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("finally {a.#valid;}" => true; "valid")]
#[test_case("finally {a.#invalid;}" => false; "invalid")]
fn finally_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Finally::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod finally {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        Finally::parse(&mut newparser("finally{}"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// CATCH PARAMETER
#[test]
fn catch_parameter_test_01() {
    let (node, scanner) = check(CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "CatchParameter: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn catch_parameter_test_02() {
    let (node, scanner) = check(CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    pretty_check(&*node, "CatchParameter: [ a ]", vec!["BindingPattern: [ a ]"]);
    concise_check(&*node, "ArrayBindingPattern: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    format!("{:?}", node);
}
#[test]
fn catch_parameter_test_err_01() {
    check_err(CatchParameter::parse(&mut newparser(""), Scanner::new(), false, false), "CatchParameter expected", 1, 1);
}
#[test]
fn catch_parameter_test_prettyerrors_1() {
    let (item, _) = CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn catch_parameter_test_prettyerrors_2() {
    let (item, _) = CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn catch_parameter_test_conciseerrors_1() {
    let (item, _) = CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn catch_parameter_test_conciseerrors_2() {
    let (item, _) = CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn catch_parameter_test_cache_01() {
    let mut parser = newparser("{a,b}");
    let (node, scanner) = check(CatchParameter::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(CatchParameter::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
fn cp_contains_check(src: &str, has_literal: bool) {
    let (item, _) = CatchParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn catch_parameter_test_contains() {
    cp_contains_check("a", false);
    cp_contains_check("[a]", false);
    cp_contains_check("[a=0]", true);
}
#[test_case("a" => true; "identifier")]
#[test_case("[a=b.#valid]" => true; "pattern valid")]
#[test_case("[a=b.#invalid]" => false; "pattern invalid")]
fn catch_parameter_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CatchParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod catch_parameter {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        CatchParameter::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

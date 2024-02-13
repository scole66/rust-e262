use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// MEMBER EXPRESSION
#[test]
fn member_expression_test_primary_expression() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*me, MemberExpression::PrimaryExpression(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: a", &["PrimaryExpression: a"]);
    concise_check(&*me, "IdentifierName: a", &[]);
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_meta_property() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("new.target"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*me, MemberExpression::MetaProperty(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: new . target", &["MetaProperty: new . target"]);
    concise_check(&*me, "MetaProperty: new . target", &["Keyword: new", "Punctuator: .", "Keyword: target"]);
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_super_property() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("super.ior"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(&*me, MemberExpression::SuperProperty(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: super . ior", &["SuperProperty: super . ior"]);
    concise_check(&*me, "SuperProperty: super . ior", &["Keyword: super", "Punctuator: .", "IdentifierName: ior"]);
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_new_me_args() {
    let (me, scanner) =
        check(MemberExpression::parse(&mut newparser("new shoes('red', 'leather')"), Scanner::new(), false, false));
    chk_scan(&scanner, 27);
    assert!(matches!(&*me, MemberExpression::NewArguments(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(
        &*me,
        "MemberExpression: new shoes ( 'red' , 'leather' )",
        &["MemberExpression: shoes", "Arguments: ( 'red' , 'leather' )"],
    );
    concise_check(
        &*me,
        "MemberExpression: new shoes ( 'red' , 'leather' )",
        &["Keyword: new", "IdentifierName: shoes", "Arguments: ( 'red' , 'leather' )"],
    );
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_me_expression() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("bill[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*me, MemberExpression::Expression(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: bill [ a ]", &["MemberExpression: bill", "Expression: a"]);
    concise_check(
        &*me,
        "MemberExpression: bill [ a ]",
        &["IdentifierName: bill", "Punctuator: [", "IdentifierName: a", "Punctuator: ]"],
    );
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_me_ident() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice.name"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*me, MemberExpression::IdentifierName(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: alice . name", &["MemberExpression: alice"]);
    concise_check(
        &*me,
        "MemberExpression: alice . name",
        &["IdentifierName: alice", "Punctuator: .", "IdentifierName: name"],
    );
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_me_private() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice.#name"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(&*me, MemberExpression::PrivateId(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: alice . #name", &["MemberExpression: alice"]);
    concise_check(
        &*me,
        "MemberExpression: alice . #name",
        &["IdentifierName: alice", "Punctuator: .", "PrivateIdentifier: #name"],
    );
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_me_template() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice`${a}`"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(&*me, MemberExpression::TemplateLiteral(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{me:?}");
    pretty_check(&*me, "MemberExpression: alice `${ a }`", &["MemberExpression: alice", "TemplateLiteral: `${ a }`"]);
    concise_check(
        &*me,
        "MemberExpression: alice `${ a }`",
        &["IdentifierName: alice", "SubstitutionTemplate: `${ a }`"],
    );
    assert_eq!(me.is_function_definition(), false);
}
#[test]
fn member_expression_test_errs_01() {
    check_err(
        MemberExpression::parse(&mut newparser(""), Scanner::new(), false, false),
        "MemberExpression expected",
        1,
        1,
    );
}
#[test]
fn member_expression_test_errs_02() {
    check_err(
        MemberExpression::parse(&mut newparser("super"), Scanner::new(), false, false),
        "one of [‘.’, ‘[’] expected",
        1,
        6,
    );
}
#[test]
fn member_expression_test_errs_04() {
    check_err(MemberExpression::parse(&mut newparser("new"), Scanner::new(), false, false), "‘.’ expected", 1, 4);
}
#[test]
fn member_expression_test_errs_05() {
    check_err(MemberExpression::parse(&mut newparser("new."), Scanner::new(), false, false), "‘target’ expected", 1, 5);
}
#[test]
fn member_expression_test_errs_06() {
    check_err(MemberExpression::parse(&mut newparser("new a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
}
#[test]
fn member_expression_test_prettyerrors_1() {
    let (item, _) = MemberExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_2() {
    let (item, _) = MemberExpression::parse(&mut newparser("a[0]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_3() {
    let (item, _) = MemberExpression::parse(&mut newparser("a.b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_4() {
    let (item, _) = MemberExpression::parse(&mut newparser("a`b`"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_5() {
    let (item, _) = MemberExpression::parse(&mut newparser("super.a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_6() {
    let (item, _) = MemberExpression::parse(&mut newparser("import.meta"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_7() {
    let (item, _) = MemberExpression::parse(&mut newparser("new a()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn member_expression_test_prettyerrors_8() {
    let (item, _) = MemberExpression::parse(&mut newparser("a.#b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}

#[test]
fn member_expression_test_conciseerrors_1() {
    let (item, _) = MemberExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_2() {
    let (item, _) = MemberExpression::parse(&mut newparser("a[0]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_3() {
    let (item, _) = MemberExpression::parse(&mut newparser("a.b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_4() {
    let (item, _) = MemberExpression::parse(&mut newparser("a`b`"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_5() {
    let (item, _) = MemberExpression::parse(&mut newparser("super.a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_6() {
    let (item, _) = MemberExpression::parse(&mut newparser("import.meta"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_7() {
    let (item, _) = MemberExpression::parse(&mut newparser("new a()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_conciseerrors_8() {
    let (item, _) = MemberExpression::parse(&mut newparser("a.#b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn member_expression_test_contains_01() {
    let (item, _) = MemberExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_02() {
    let (item, _) = MemberExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn member_expression_test_contains_03() {
    let (item, _) = MemberExpression::parse(&mut newparser("this[10]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_04() {
    let (item, _) = MemberExpression::parse(&mut newparser("a[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_05() {
    let (item, _) = MemberExpression::parse(&mut newparser("a[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn member_expression_test_contains_06() {
    let (item, _) = MemberExpression::parse(&mut newparser("this.name"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_07() {
    let (item, _) = MemberExpression::parse(&mut newparser("a.name"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn member_expression_test_contains_08() {
    let (item, _) = MemberExpression::parse(&mut newparser("this`${thang}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_09() {
    let (item, _) = MemberExpression::parse(&mut newparser("a`${this}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_10() {
    let (item, _) = MemberExpression::parse(&mut newparser("a`${0}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn member_expression_test_contains_11() {
    let (item, _) = MemberExpression::parse(&mut newparser("super[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn member_expression_test_contains_12() {
    let (item, _) = MemberExpression::parse(&mut newparser("super[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn member_expression_test_contains_13() {
    let (item, _) = MemberExpression::parse(&mut newparser("new.target"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn member_expression_test_contains_14() {
    let (item, _) = MemberExpression::parse(&mut newparser("new this()"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_15() {
    let (item, _) = MemberExpression::parse(&mut newparser("new a(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn member_expression_test_contains_16() {
    let (item, _) = MemberExpression::parse(&mut newparser("new a(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn member_expression_test_contains_17() {
    let (item, _) = MemberExpression::parse(&mut newparser("this.#blue"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::NewTarget), false);
}
mod member_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("a.b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = MemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }

    #[test_case("new.target" => true; "new.target")]
    #[test_case("(a.#valid)" => true; "PrimaryExpression valid")]
    #[test_case("a[b.#valid]" => true; "ME Expression valid")]
    #[test_case("a.#valid[b]" => true; "ME valid Expression")]
    #[test_case("a.#valid.c" => true; "ME valid id")]
    #[test_case("a`${b.#valid}`" => true; "ME template valid")]
    #[test_case("a.#valid`${b}`" => true; "ME valid template")]
    #[test_case("super[a.#valid]" => true; "super valid")]
    #[test_case("new a(b.#valid)" => true; "new args valid")]
    #[test_case("new a.#valid(b)" => true; "new me valid")]
    #[test_case("a.#valid" => true; "me private valid")]
    #[test_case("a.#valid.#valid2" => true; "me valid private")]
    #[test_case("(a.#invalid)" => false; "PrimaryExpression invalid")]
    #[test_case("a[b.#invalid]" => false; "ME Expression invalid")]
    #[test_case("a.#invalid[b]" => false; "ME invalid Expression")]
    #[test_case("a.#invalid.c" => false; "ME invalid id")]
    #[test_case("a`${b.#invalid}`" => false; "ME template invalid")]
    #[test_case("a.#invalid`${b}`" => false; "ME invalid template")]
    #[test_case("super[a.#invalid]" => false; "super invalid")]
    #[test_case("new a(b.#invalid)" => false; "new args invalid")]
    #[test_case("new a.#invalid(b)" => false; "new me invalid")]
    #[test_case("a.#invalid" => false; "me private invalid")]
    #[test_case("a.#invalid.#valid2" => false; "me invalid private")]
    #[test_case("a.#valid.#invalid2" => false; "me invalid private2")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = MemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid"), JSString::from("#valid2")])
    }

    #[test_case("[]" => true; "array literal")]
    #[test_case("{}" => true; "object literal")]
    #[test_case("3" => false; "other literal")]
    #[test_case("a[3]" => false; "MemberExpression [ Expression ]")]
    #[test_case("a.b" => false; "MembreExpression . IdentifierName")]
    #[test_case("a`b`" => false; "MemberExpression TemplateLiteral")]
    #[test_case("super.a" => false; "SuperProperty")]
    #[test_case("new.target" => false; "MetaProperty")]
    #[test_case("new a()" => false; "new MemberExpression Arguments")]
    #[test_case("a.#b" => false; "MemberExpression . PrivateIdentifier")]
    fn is_object_or_array_literal(src: &str) -> bool {
        MemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_object_or_array_literal()
    }

    const IMPORT_META_ERR: &str = "import.meta allowed only in Module code";

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "primary expression")]
    #[test_case("package[0]", true => sset(&[PACKAGE_NOT_ALLOWED]); "array syntax (id bad)")]
    #[test_case("a[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "array syntax (exp bad)")]
    #[test_case("package.a", true => sset(&[PACKAGE_NOT_ALLOWED]); "member syntax (id bad)")]
    #[test_case("package``", true => sset(&[PACKAGE_NOT_ALLOWED]); "templ syntax (id bad)")]
    #[test_case("a`${package}`", true => sset(&[PACKAGE_NOT_ALLOWED]); "templ syntax (templ bad)")]
    #[test_case("super.package", true => sset(&[]); "super property")]
    #[test_case("import.meta", true => sset(&[IMPORT_META_ERR]); "meta property")]
    #[test_case("new package(0)", true => sset(&[PACKAGE_NOT_ALLOWED]); "new expr (id bad)")]
    #[test_case("new a(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "new expr (args bad)")]
    #[test_case("package.#a", true => sset(&[PACKAGE_NOT_ALLOWED]); "private id")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        MemberExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a[0]" => true; "square brackets")]
    #[test_case("a.b" => true; "member notation")]
    #[test_case("a`${1}`" => true; "tagged template")]
    #[test_case("super.a" => true; "super property")]
    #[test_case("new.target" => true; "meta property")]
    #[test_case("new a()" => true; "new member")]
    #[test_case("a.#u" => false; "private id")]
    fn is_strictly_deletable(src: &str) -> bool {
        MemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("new.target" => false; "Meta")]
    #[test_case("arguments" => true; "Primary (yes)")]
    #[test_case("arguments[bob]" => true; "ME [ Exp ] (left)")]
    #[test_case("bob[arguments]" => true; "ME [ Exp ] (right)")]
    #[test_case("arguments.name" => true; "ME . Id (yes)")]
    #[test_case("arguments`${bob}`" => true; "ME TL (left)")]
    #[test_case("bob`${arguments}`" => true; "ME TL (right)")]
    #[test_case("super[arguments]" => true; "Super (yes)")]
    #[test_case("new arguments()" => true; "new ME Args (left)")]
    #[test_case("new bob(arguments)" => true; "new ME Args (right)")]
    #[test_case("arguments.#pid" => true; "privateid (yes)")]
    #[test_case("xyzzy" => false; "Primary (no)")]
    #[test_case("xyzzy[bob]" => false; "ME [ Exp ] (no)")]
    #[test_case("xyzzy.name" => false; "ME . Id (no)")]
    #[test_case("xyzzy`${bob}`" => false; "ME TL (no)")]
    #[test_case("super[xyzzy]" => false; "Super (no)")]
    #[test_case("new xyzzy()" => false; "new ME Args (no)")]
    #[test_case("xyzzy.#pid" => false; "privateid (no)")]
    fn contains_arguments(src: &str) -> bool {
        MemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "primary (simple)")]
    #[test_case("eval", true => ATTKind::Invalid; "primary (invalid)")]
    #[test_case("a[b]", false => ATTKind::Simple; "brackets")]
    #[test_case("a.b", false => ATTKind::Simple; "property id")]
    #[test_case("a`${b}`", false => ATTKind::Invalid; "template")]
    #[test_case("super.a", false => ATTKind::Simple; "super prop")]
    #[test_case("new.target", false => ATTKind::Invalid; "meta")]
    #[test_case("new a(b)", false => ATTKind::Invalid; "new me")]
    #[test_case("a.#b", false => ATTKind::Simple; "private id")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).member_expression().assignment_target_type(strict)
    }

    #[test_case("a[b]" => false; "brackets")]
    #[test_case("a.b" => false; "property id")]
    #[test_case("a`${b}`" => false; "template")]
    #[test_case("super.a" => false; "super prop")]
    #[test_case("new.target" => false; "meta")]
    #[test_case("new a(b)" => false; "new me")]
    #[test_case("a.#b" => false; "private id")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).member_expression().is_named_function()
    }

    #[test_case("a[b]" => false; "brackets")]
    #[test_case("a.b" => false; "property id")]
    #[test_case("a`${b}`" => false; "template")]
    #[test_case("super.a" => false; "super prop")]
    #[test_case("new.target" => false; "meta")]
    #[test_case("new a(b)" => false; "new me")]
    #[test_case("a.#b" => false; "private id")]
    #[test_case("beetle" => true; "id")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_identifier_ref(src: &str) -> bool {
        Maker::new(src).member_expression().is_identifier_ref()
    }

    #[test_case("a[b]" => None; "brackets")]
    #[test_case("a.b" => None; "property id")]
    #[test_case("a`${b}`" => None; "template")]
    #[test_case("super.a" => None; "super prop")]
    #[test_case("new.target" => None; "meta")]
    #[test_case("new a(b)" => None; "new me")]
    #[test_case("a.#b" => None; "private id")]
    #[test_case("beetle" => ssome("beetle"); "id")]
    #[test_case("1" => None; "literal fallthru")]
    fn identifier_ref(src: &str) -> Option<String> {
        Maker::new(src).member_expression().identifier_ref().map(|node| node.to_string())
    }

    #[test_case("  a[b]" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "brackets")]
    #[test_case("  a.b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "property id")]
    #[test_case("  a`${b}`" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "template")]
    #[test_case("  super.a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "super prop")]
    #[test_case("  new.target" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 10 }}; "meta")]
    #[test_case("  new a(b)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "new me")]
    #[test_case("  a.#b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "private id")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).member_expression().location()
    }

    #[test_case("[a]" => true; "ArrayLiteral")]
    #[test_case("{a}" => true; "ObjectLiteral")]
    #[test_case("1" => false; "other literal")]
    #[test_case("a[1]" => false; "MemberExpression [ Expression ]")]
    #[test_case("a.b" => false; "MemberExpression . IdentifierName")]
    #[test_case("a`${b}`" => false; "template")]
    #[test_case("super.a" => false; "super prop")]
    #[test_case("new.target" => false; "meta")]
    #[test_case("new a(b)" => false; "new me")]
    #[test_case("a.#b" => false; "private id")]
    fn is_destructuring(src: &str) -> bool {
        Maker::new(src).member_expression().is_destructuring()
    }
}

// SUPER PROPERTY
#[test]
fn super_property_test_expression() {
    let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(*sp, SuperProperty::Expression { .. }));
    // Excersize the Debug formatter, for code coverage
    format!("{sp:?}");
    pretty_check(&*sp, "SuperProperty: super [ a ]", &["Expression: a"]);
    concise_check(
        &*sp,
        "SuperProperty: super [ a ]",
        &["Keyword: super", "Punctuator: [", "IdentifierName: a", "Punctuator: ]"],
    );
}
#[test]
fn super_property_test_ident() {
    let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super.bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(*sp, SuperProperty::IdentifierName { .. }));
    // Excersize the Debug formatter, for code coverage
    format!("{sp:?}");
    pretty_check(&*sp, "SuperProperty: super . bob", &[]);
    concise_check(&*sp, "SuperProperty: super . bob", &["Keyword: super", "Punctuator: .", "IdentifierName: bob"]);
}
#[test]
fn super_property_test_nomatch() {
    check_err(SuperProperty::parse(&mut newparser("silly"), Scanner::new(), false, false), "‘super’ expected", 1, 1);
}
#[test]
fn super_property_test_bad_ident() {
    let r = SuperProperty::parse(&mut newparser("super.*"), Scanner::new(), false, false);
    check_err(r, "IdentifierName expected", 1, 7);
}
#[test]
fn super_property_test_bad_expression() {
    let r = SuperProperty::parse(&mut newparser("super[while]"), Scanner::new(), false, false);
    check_err(r, "Expression expected", 1, 7);
}
#[test]
fn super_property_test_incomplete_expression() {
    let r = SuperProperty::parse(&mut newparser("super[99"), Scanner::new(), false, false);
    check_err(r, "‘]’ expected", 1, 9);
}
#[test]
fn super_property_test_bad_following_token() {
    check_err(
        SuperProperty::parse(&mut newparser("super duper"), Scanner::new(), false, false),
        "one of [‘.’, ‘[’] expected",
        1,
        7,
    );
}
#[test]
fn super_property_test_prettyerrors_1() {
    let (item, _) = SuperProperty::parse(&mut newparser("super[a]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn super_property_test_prettyerrors_2() {
    let (item, _) = SuperProperty::parse(&mut newparser("super.a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn super_property_test_conciseerrors_1() {
    let (item, _) = SuperProperty::parse(&mut newparser("super[a]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn super_property_test_conciseerrors_2() {
    let (item, _) = SuperProperty::parse(&mut newparser("super.a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn super_property_test_contains_01() {
    let (item, _) = SuperProperty::parse(&mut newparser("super[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn super_property_test_contains_02() {
    let (item, _) = SuperProperty::parse(&mut newparser("super[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn super_property_test_contains_03() {
    let (item, _) = SuperProperty::parse(&mut newparser("super.a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
mod super_property {
    use super::*;
    use test_case::test_case;

    #[test_case("super.a" => true; "super identifier")]
    #[test_case("super[a.#valid]" => true; "expression valid")]
    #[test_case("super[a.#invalid]" => false; "expression invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = SuperProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("super.package", true => sset(&[]); "super.member")]
    #[test_case("super[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "super[exp]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        SuperProperty::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("super[arguments]" => true; "Expression (yes)")]
    #[test_case("super[xyzzy]" => false; "Expression (no)")]
    #[test_case("super.arguments" => false; "IdentifierName")]
    fn contains_arguments(src: &str) -> bool {
        SuperProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  super.a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "ident")]
    #[test_case("  super[a]" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "expression")]
    fn location(src: &str) -> Location {
        Maker::new(src).super_property().location()
    }
}

// META PROPERTY
#[test]
fn meta_property_test_newtarget() {
    let (mp, scanner) = check(MetaProperty::parse(&mut newparser("new.target"), Scanner::new()));
    chk_scan(&scanner, 10);
    assert!(matches!(*mp, MetaProperty::NewTarget { .. }));
    format!("{mp:?}");
    pretty_check(&*mp, "MetaProperty: new . target", &[]);
    concise_check(&*mp, "MetaProperty: new . target", &["Keyword: new", "Punctuator: .", "Keyword: target"]);
}
#[test_case(ParseGoal::Script => Some(ParseGoal::Script); "script")]
#[test_case(ParseGoal::Module => Some(ParseGoal::Module); "module")]
fn meta_property_test_importmeta(goal: ParseGoal) -> Option<ParseGoal> {
    let (mp, scanner) = check(MetaProperty::parse(&mut Parser::new("import.meta", false, goal), Scanner::new()));
    chk_scan(&scanner, 11);
    assert!(matches!(*mp, MetaProperty::ImportMeta { .. }));
    format!("{mp:?}");
    pretty_check(&*mp, "MetaProperty: import . meta", &[]);
    concise_check(&*mp, "MetaProperty: import . meta", &["Keyword: import", "Punctuator: .", "Keyword: meta"]);
    match *mp {
        MetaProperty::NewTarget { .. } => None,
        MetaProperty::ImportMeta { goal, .. } => Some(goal),
    }
}
#[test]
fn meta_property_test_nomatch_01() {
    check_err(MetaProperty::parse(&mut newparser("silly"), Scanner::new()), "one of [‘new’, ‘import’] expected", 1, 1);
}
#[test]
fn meta_property_test_nomatch_02() {
    check_err(MetaProperty::parse(&mut newparser("new silly"), Scanner::new()), "‘.’ expected", 1, 5);
}
#[test]
fn meta_property_test_nomatch_03() {
    check_err(MetaProperty::parse(&mut newparser("new.silly"), Scanner::new()), "‘target’ expected", 1, 5);
}
#[test]
fn meta_property_test_nomatch_04() {
    check_err(MetaProperty::parse(&mut newparser("import silly"), Scanner::new()), "‘.’ expected", 1, 8);
}
#[test]
fn meta_property_test_nomatch_05() {
    check_err(MetaProperty::parse(&mut newparser("import.silly"), Scanner::new()), "‘meta’ expected", 1, 8);
}
#[test]
fn meta_property_test_prettyerrors_1() {
    let (item, _) = MetaProperty::parse(&mut newparser("new.target"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn meta_property_test_prettyerrors_2() {
    let (item, _) = MetaProperty::parse(&mut newparser("import.meta"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn meta_property_test_conciseerrors_1() {
    let (item, _) = MetaProperty::parse(&mut newparser("new.target"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn meta_property_test_conciseerrors_2() {
    let (item, _) = MetaProperty::parse(&mut newparser("import.meta"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn meta_property_test_contains_01() {
    let (item, _) = MetaProperty::parse(&mut newparser("new.target"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn meta_property_test_contains_02() {
    let (item, _) = MetaProperty::parse(&mut newparser("import.meta"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::NewTarget), false);
}
mod meta_property {
    use super::*;
    use test_case::test_case;

    #[test_case("new.target", ParseGoal::Script => AHashSet::<String>::new(); "new.target")]
    #[test_case("import.meta", ParseGoal::Script => sset(&["import.meta allowed only in Module code"]); "import.meta (in script)")]
    #[test_case("import.meta", ParseGoal::Module => AHashSet::<String>::new(); "import.meta (in module)")]
    fn early_errors(src: &str, goal: ParseGoal) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        MetaProperty::parse(&mut Parser::new(src, false, goal), Scanner::new()).unwrap().0.early_errors(&mut errs);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("  new.target" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 10 }}; "new.target")]
    #[test_case("  import.meta" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 11 }}; "import.meta")]
    fn location(src: &str) -> Location {
        Maker::new(src).meta_property().location()
    }
}

// ARGUMENTS
#[test]
fn arguments_test_onlyparens() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("()"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*args, Arguments::Empty { .. }));
    format!("{args:?}");
    pretty_check(&*args, "Arguments: ( )", &[]);
    concise_check(&*args, "Arguments: ( )", &["Punctuator: (", "Punctuator: )"]);
}
#[test]
fn arguments_test_trailing_comma() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("(a,)"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*args, Arguments::ArgumentListComma(..)));
    format!("{args:?}");
    pretty_check(&*args, "Arguments: ( a , )", &["ArgumentList: a"]);
    concise_check(
        &*args,
        "Arguments: ( a , )",
        &["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: )"],
    );
}
#[test]
fn arguments_test_arglist() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(*args, Arguments::ArgumentList(..)));
    format!("{args:?}");
    pretty_check(&*args, "Arguments: ( a , b )", &["ArgumentList: a , b"]);
    concise_check(&*args, "Arguments: ( a , b )", &["Punctuator: (", "ArgumentList: a , b", "Punctuator: )"]);
}
#[test]
fn arguments_test_nomatch() {
    check_err(Arguments::parse(&mut newparser("*"), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn arguments_test_unclosed_01() {
    check_err(Arguments::parse(&mut newparser("("), Scanner::new(), false, false), "‘)’ expected", 1, 2);
}
#[test]
fn arguments_test_unclosed_02() {
    check_err(
        Arguments::parse(&mut newparser("(88"), Scanner::new(), false, false),
        "one of [‘,’, ‘)’] expected",
        1,
        4,
    );
}
#[test]
fn arguments_test_unclosed_03() {
    check_err(Arguments::parse(&mut newparser("(91,"), Scanner::new(), false, false), "‘)’ expected", 1, 5);
}
#[test]
fn arguments_test_prettyerrors_1() {
    let (item, _) = Arguments::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arguments_test_prettyerrors_2() {
    let (item, _) = Arguments::parse(&mut newparser("(A)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arguments_test_prettyerrors_3() {
    let (item, _) = Arguments::parse(&mut newparser("(A,)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arguments_test_conciseerrors_1() {
    let (item, _) = Arguments::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arguments_test_conciseerrors_2() {
    let (item, _) = Arguments::parse(&mut newparser("(A)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arguments_test_conciseerrors_3() {
    let (item, _) = Arguments::parse(&mut newparser("(A,)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arguments_test_contains_01() {
    let (item, _) = Arguments::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arguments_test_contains_02() {
    let (item, _) = Arguments::parse(&mut newparser("(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arguments_test_contains_03() {
    let (item, _) = Arguments::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arguments_test_contains_04() {
    let (item, _) = Arguments::parse(&mut newparser("(this,)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arguments_test_contains_05() {
    let (item, _) = Arguments::parse(&mut newparser("(0,)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod arguments {
    use super::*;
    use test_case::test_case;

    #[test_case("()" => true; "empty")]
    #[test_case("(a=b.#valid)" => true; "arg valid")]
    #[test_case("(a=b.#valid,)" => true; "comma valid")]
    #[test_case("(a=b.#invalid)" => false; "arg invalid")]
    #[test_case("(a=b.#invalid,)" => false; "comma invalid")]
    fn arguments_test_all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = Arguments::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("()", true => AHashSet::<String>::new(); "empty")]
    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "argument list")]
    #[test_case("(package,)", true => sset(&[PACKAGE_NOT_ALLOWED]); "argument list; comma")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Arguments::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("()" => false; "empty")]
    #[test_case("(arguments)" => true; "List (yes)")]
    #[test_case("(arguments,)" => true; "List-comma (yes)")]
    #[test_case("(xyzzy)" => false; "List (no)")]
    #[test_case("(xyzzy,)" => false; "List-comma (no)")]
    fn contains_arguments(src: &str) -> bool {
        Arguments::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  ()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "empty")]
    #[test_case("  (a,b)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "list")]
    #[test_case("  (a,b,)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "list+comma")]
    fn location(src: &str) -> Location {
        Maker::new(src).arguments().location()
    }
}

// ARGUMENT LIST
#[test]
fn argument_list_test_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*al, ArgumentList::FallThru(_)));
    format!("{al:?}");
    pretty_check(&*al, "ArgumentList: aba", &["AssignmentExpression: aba"]);
    concise_check(&*al, "IdentifierName: aba", &[]);
}
#[test]
fn argument_list_test_dots_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("...aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*al, ArgumentList::Dots(_)));
    format!("{al:?}");
    pretty_check(&*al, "ArgumentList: ... aba", &["AssignmentExpression: aba"]);
    concise_check(&*al, "ArgumentList: ... aba", &["Punctuator: ...", "IdentifierName: aba"]);
}
#[test]
fn argument_list_test_al_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*al, ArgumentList::ArgumentList(..)));
    format!("{al:?}");
    pretty_check(&*al, "ArgumentList: ab , aba", &["ArgumentList: ab", "AssignmentExpression: aba"]);
    concise_check(&*al, "ArgumentList: ab , aba", &["IdentifierName: ab", "Punctuator: ,", "IdentifierName: aba"]);
}
#[test]
fn argument_list_test_al_dots_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,...aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(*al, ArgumentList::ArgumentListDots(..)));
    format!("{al:?}");
    pretty_check(&*al, "ArgumentList: ab , ... aba", &["ArgumentList: ab", "AssignmentExpression: aba"]);
    concise_check(
        &*al,
        "ArgumentList: ab , ... aba",
        &["IdentifierName: ab", "Punctuator: ,", "Punctuator: ...", "IdentifierName: aba"],
    );
}
#[test]
fn argument_list_test_nomatch() {
    check_err(
        ArgumentList::parse(&mut newparser("*"), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        1,
    );
}
#[test]
fn argument_list_test_dotsonly() {
    check_err(
        ArgumentList::parse(&mut newparser("..."), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        4,
    );
}
#[test]
fn argument_list_test_dots_term() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,..."), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*al, ArgumentList::FallThru(_)));
}
#[test]
fn argument_list_test_commas() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,,10"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*al, ArgumentList::FallThru(_)));
}
#[test]
fn argument_list_test_prettyerrors_1() {
    let (item, _) = ArgumentList::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn argument_list_test_prettyerrors_2() {
    let (item, _) = ArgumentList::parse(&mut newparser("...0"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn argument_list_test_prettyerrors_3() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,1"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn argument_list_test_prettyerrors_4() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,...a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn argument_list_test_conciseerrors_1() {
    let (item, _) = ArgumentList::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn argument_list_test_conciseerrors_2() {
    let (item, _) = ArgumentList::parse(&mut newparser("...0"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn argument_list_test_conciseerrors_3() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,1"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn argument_list_test_conciseerrors_4() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,...a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn argument_list_test_contains_01() {
    let (item, _) = ArgumentList::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_02() {
    let (item, _) = ArgumentList::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn argument_list_test_contains_03() {
    let (item, _) = ArgumentList::parse(&mut newparser("...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_04() {
    let (item, _) = ArgumentList::parse(&mut newparser("...0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn argument_list_test_contains_05() {
    let (item, _) = ArgumentList::parse(&mut newparser("this,0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_06() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_07() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn argument_list_test_contains_08() {
    let (item, _) = ArgumentList::parse(&mut newparser("this,...0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_09() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn argument_list_test_contains_10() {
    let (item, _) = ArgumentList::parse(&mut newparser("0,...1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod argument_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a.#valid" => true; "fallthru valid")]
    #[test_case("...a.#valid" => true; "dots valid")]
    #[test_case("a.#valid,b" => true; "list left valid")]
    #[test_case("a,b.#valid" => true; "list right valid")]
    #[test_case("a.#valid,...b" => true; "dotlist lft valid")]
    #[test_case("a,...b.#valid" => true; "dotlist rgt valid")]
    #[test_case("a.#invalid" => false; "fallthru invalid")]
    #[test_case("...a.#invalid" => false; "dots invalid")]
    #[test_case("a.#invalid,b" => false; "list left invalid")]
    #[test_case("a,b.#invalid" => false; "list right invalid")]
    #[test_case("a.#invalid,...b" => false; "dotlist lft invalid")]
    #[test_case("a,...b.#invalid" => false; "dotlist rgt invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ArgumentList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "one expression")]
    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "spread expression")]
    #[test_case("package,0", true => sset(&[PACKAGE_NOT_ALLOWED]); "list; head bad")]
    #[test_case("0,package", true => sset(&[PACKAGE_NOT_ALLOWED]); "list; tail bad")]
    #[test_case("package,...a", true => sset(&[PACKAGE_NOT_ALLOWED]); "list, rest; head bad")]
    #[test_case("0,...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "list, rest; rest bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ArgumentList::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("arguments" => true; "AssignmentExpression (yes)")]
    #[test_case("...arguments" => true; "SpreadExpression (yes)")]
    #[test_case("arguments,bob" => true; "List , Exp (left)")]
    #[test_case("bob,arguments" => true; "List , Exp (right)")]
    #[test_case("arguments,...bob" => true; "List , Spread (left)")]
    #[test_case("bob,...arguments" => true; "List , Spread (right)")]
    #[test_case("xyzzy" => false; "AssignmentExpression (no)")]
    #[test_case("...xyzzy" => false; "SpreadExpression (no)")]
    #[test_case("xyzzy,bob" => false; "List , Exp (no)")]
    #[test_case("xyzzy,...bob" => false; "List , Spread (no)")]
    fn contains_arguments(src: &str) -> bool {
        ArgumentList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }
}

// NEW EXPRESSION
mod new_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn me() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("true"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ne, NewExpression::MemberExpression(_)));
        format!("{ne:?}");
        pretty_check(&*ne, "NewExpression: true", &["MemberExpression: true"]);
        concise_check(&*ne, "Keyword: true", &[]);
        assert_eq!(ne.is_function_definition(), false);
    }
    #[test]
    fn new() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(*ne, NewExpression::NewExpression(..)));
        format!("{ne:?}");
        pretty_check(&*ne, "NewExpression: new bob", &["NewExpression: bob"]);
        concise_check(&*ne, "NewExpression: new bob", &["Keyword: new", "IdentifierName: bob"]);
        assert_eq!(ne.is_function_definition(), false);
    }
    #[test]
    fn new_me() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob()"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(*ne, NewExpression::MemberExpression(_)));
        format!("{ne:?}");
        pretty_check(&*ne, "NewExpression: new bob ( )", &["MemberExpression: new bob ( )"]);
        concise_check(
            &*ne,
            "MemberExpression: new bob ( )",
            &["Keyword: new", "IdentifierName: bob", "Arguments: ( )"],
        );
        assert_eq!(ne.is_function_definition(), false);
    }
    #[test]
    fn nomatch() {
        check_err(
            NewExpression::parse(&mut newparser("*"), Scanner::new(), false, false),
            "‘new’ or MemberExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn chopped() {
        check_err(NewExpression::parse(&mut newparser("new"), Scanner::new(), false, false), "‘.’ expected", 1, 4);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = NewExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = NewExpression::parse(&mut newparser("new this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = NewExpression::parse(&mut newparser("new 0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("new b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = NewExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("a.#valid" => true; "fallthru valid")]
    #[test_case("new a.#valid" => true; "new valid")]
    #[test_case("a.#invalid" => false; "fallthru invalid")]
    #[test_case("new a.#invalid" => false; "new invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = NewExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("{}" => true; "ObjectLiteral")]
    #[test_case("3" => false; "Other Literal")]
    #[test_case("new x" => false; "new NewExpression")]
    fn is_object_or_array_literal(src: &str) -> bool {
        NewExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_object_or_array_literal()
    }
    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp")]
    #[test_case("new package", true => sset(&[PACKAGE_NOT_ALLOWED]); "new exp")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        NewExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("new a" => true; "new expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        NewExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("new arguments" => true; "new Exp (yes)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("new xyzzy" => false; "new Exp (no)")]
    fn contains_arguments(src: &str) -> bool {
        NewExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("new a", false => ATTKind::Invalid; "new expr")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).new_expression().assignment_target_type(strict)
    }

    #[test_case("  new x" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "new exp")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).new_expression().location()
    }

    #[test_case("new a" => false; "new exp")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).new_expression().is_named_function()
    }

    #[test_case("idref" => true; "Id Ref")]
    #[test_case("10" => false; "literal")]
    #[test_case("new a" => false; "other new expr")]
    fn is_identifier_ref(src: &str) -> bool {
        Maker::new(src).new_expression().is_identifier_ref()
    }

    #[test_case("idref" => ssome("idref"); "Id Ref")]
    #[test_case("10" => None; "literal")]
    #[test_case("new a" => None; "other new expr")]
    fn identifier_ref(src: &str) -> Option<String> {
        Maker::new(src).new_expression().identifier_ref().map(|id| id.to_string())
    }

    #[test_case("1" => false; "literal")]
    #[test_case("new a" => false; "other new expr")]
    #[test_case("[a]" => true; "destructuring")]
    fn is_destructuring(src: &str) -> bool {
        Maker::new(src).new_expression().is_destructuring()
    }
}

// CALL MEMBER EXPRESSION
mod call_member_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn me_args() {
        let (cme, scanner) = check(CallMemberExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        format!("{cme:?}");
        pretty_check(&*cme, "CallMemberExpression: a ( )", &["MemberExpression: a", "Arguments: ( )"]);
        concise_check(&*cme, "CallMemberExpression: a ( )", &["IdentifierName: a", "Arguments: ( )"]);
    }
    #[test]
    fn nomatch() {
        check_err(
            CallMemberExpression::parse(&mut newparser("++"), Scanner::new(), false, false),
            "MemberExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn incomplete() {
        check_err(
            CallMemberExpression::parse(&mut newparser("pop"), Scanner::new(), false, false),
            "‘(’ expected",
            1,
            4,
        );
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = CallMemberExpression::parse(&mut newparser("o(0)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = CallMemberExpression::parse(&mut newparser("o(0)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) =
            CallMemberExpression::parse(&mut newparser("this.fcn()"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) =
            CallMemberExpression::parse(&mut newparser("Math.sin(this.angle)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            CallMemberExpression::parse(&mut newparser("Math.sin(angle)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("a.#valid()" => true; "MemberExpression valid")]
    #[test_case("a(b.#valid)" => true; "Arguments valid")]
    #[test_case("a.#invalid()" => false; "MemberExpression invalid")]
    #[test_case("a(b.#invalid)" => false; "Arguments invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = CallMemberExpression::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package()", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp bad")]
    #[test_case("a(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "args bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        CallMemberExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("arguments()" => true; "Exp Args (left)")]
    #[test_case("bob(arguments)" => true; "Exp Args (right)")]
    #[test_case("bob(xyzzy)" => false; "Exp Args (no)")]
    fn contains_arguments(src: &str) -> bool {
        CallMemberExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  abcd(xyz)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 9 }}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).call_member_expression().location()
    }
}

// SUPER CALL
mod super_call {
    use super::*;
    use test_case::test_case;

    #[test]
    fn args() {
        let (sc, scanner) = check(SuperCall::parse(&mut newparser("super()"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(*sc.arguments, Arguments::Empty { .. }));
        format!("{sc:?}");
        pretty_check(&*sc, "SuperCall: super ( )", &["Arguments: ( )"]);
        concise_check(&*sc, "SuperCall: super ( )", &["Keyword: super", "Arguments: ( )"]);
    }
    #[test]
    fn nomatch() {
        check_err(SuperCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘super’ expected", 1, 1);
    }
    #[test]
    fn incomplete() {
        check_err(SuperCall::parse(&mut newparser("super"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = SuperCall::parse(&mut newparser("super(0)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = SuperCall::parse(&mut newparser("super(0)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = SuperCall::parse(&mut newparser("super(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::Super), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = SuperCall::parse(&mut newparser("super(zyz)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::Super), true);
    }
    #[test_case("super(a.#valid)" => true; "valid")]
    #[test_case("super(a.#invalid)" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = SuperCall::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("super(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        SuperCall::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("super(arguments)" => true; "yes")]
    #[test_case("super()" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        SuperCall::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  super()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).super_call().location()
    }
}

// IMPORT CALL
mod import_call {
    use super::*;
    use test_case::test_case;

    #[test]
    fn ae() {
        let (ic, scanner) = check(ImportCall::parse(&mut newparser("import(bob)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        format!("{ic:?}");
        pretty_check(&*ic, "ImportCall: import ( bob )", &["AssignmentExpression: bob"]);
        concise_check(
            &*ic,
            "ImportCall: import ( bob )",
            &["Keyword: import", "Punctuator: (", "IdentifierName: bob", "Punctuator: )"],
        );
    }
    #[test]
    fn nomatch() {
        check_err(ImportCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘import’ expected", 1, 1);
    }
    #[test]
    fn incomplete() {
        check_err(ImportCall::parse(&mut newparser("import"), Scanner::new(), false, false), "‘(’ expected", 1, 7);
    }
    #[test]
    fn incomplete2() {
        check_err(
            ImportCall::parse(&mut newparser("import("), Scanner::new(), false, false),
            "AssignmentExpression expected",
            1,
            8,
        );
    }
    #[test]
    fn incomplete3() {
        check_err(ImportCall::parse(&mut newparser("import(bob"), Scanner::new(), false, false), "‘)’ expected", 1, 11);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = ImportCall::parse(&mut newparser("import(blue)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = ImportCall::parse(&mut newparser("import(blue)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = ImportCall::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = ImportCall::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test_case("import(a.#valid)" => true; "valid")]
    #[test_case("import(a.#invalid)" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ImportCall::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("import(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ImportCall::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("import(arguments)" => true; "yes")]
    #[test_case("import(xyzzy)" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        ImportCall::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  import(foo)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 11 }}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).import_call().location()
    }
}

// CALL EXPRESSION
mod call_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn me_args() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ce, CallExpression::CallMemberExpression(_)));
        format!("{ce:?}");
        pretty_check(&*ce, "CallExpression: a ( )", &["CallMemberExpression: a ( )"]);
        concise_check(&*ce, "CallMemberExpression: a ( )", &["IdentifierName: a", "Arguments: ( )"]);
    }
    #[test]
    fn super_x() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("super()"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(*ce, CallExpression::SuperCall(_)));
        format!("{ce:?}");
        pretty_check(&*ce, "CallExpression: super ( )", &["SuperCall: super ( )"]);
        concise_check(&*ce, "SuperCall: super ( )", &["Keyword: super", "Arguments: ( )"]);
    }
    #[test]
    fn import() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("import(pop)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        assert!(matches!(*ce, CallExpression::ImportCall(_)));
        format!("{ce:?}");
        pretty_check(&*ce, "CallExpression: import ( pop )", &["ImportCall: import ( pop )"]);
        concise_check(
            &*ce,
            "ImportCall: import ( pop )",
            &["Keyword: import", "Punctuator: (", "IdentifierName: pop", "Punctuator: )"],
        );
    }
    #[test]
    fn ce_args() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(20)"), Scanner::new(), false, false));
        chk_scan(&scanner, 23);
        assert!(matches!(*ce, CallExpression::CallExpressionArguments(..)));
        format!("{ce:?}");
        pretty_check(
            &*ce,
            "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )",
            &["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"],
        );
        concise_check(
            &*ce,
            "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )",
            &["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"],
        );
    }
    #[test]
    fn ce_args2() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(++)"), Scanner::new(), false, false));
        chk_scan(&scanner, 19);
        assert!(matches!(*ce, CallExpression::CallExpressionArguments(..)));
        format!("{ce:?}");
    }
    #[test]
    fn ce_exp() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop)[snap]"), Scanner::new(), false, false));
        chk_scan(&scanner, 15);
        assert!(matches!(*ce, CallExpression::CallExpressionExpression(..)));
        format!("{ce:?}");
        pretty_check(
            &*ce,
            "CallExpression: blue ( pop ) [ snap ]",
            &["CallExpression: blue ( pop )", "Expression: snap"],
        );
        concise_check(
            &*ce,
            "CallExpression: blue ( pop ) [ snap ]",
            &["CallMemberExpression: blue ( pop )", "Punctuator: [", "IdentifierName: snap", "Punctuator: ]"],
        );
    }
    #[test]
    fn ce_ident() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop).snap"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(*ce, CallExpression::CallExpressionIdentifierName(..)));
        format!("{ce:?}");
        pretty_check(&*ce, "CallExpression: blue ( pop ) . snap", &["CallExpression: blue ( pop )"]);
        concise_check(
            &*ce,
            "CallExpression: blue ( pop ) . snap",
            &["CallMemberExpression: blue ( pop )", "Punctuator: .", "IdentifierName: snap"],
        );
    }
    #[test]
    fn ce_pid() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop).#snap"), Scanner::new(), false, false));
        chk_scan(&scanner, 15);
        assert!(matches!(*ce, CallExpression::CallExpressionPrivateId(..)));
        format!("{ce:?}");
        pretty_check(&*ce, "CallExpression: blue ( pop ) . #snap", &["CallExpression: blue ( pop )"]);
        concise_check(
            &*ce,
            "CallExpression: blue ( pop ) . #snap",
            &["CallMemberExpression: blue ( pop )", "Punctuator: .", "PrivateIdentifier: #snap"],
        );
    }
    #[test]
    fn ce_template() {
        let (ce, scanner) =
            check(CallExpression::parse(&mut newparser("blue(pop)`snap`"), Scanner::new(), false, false));
        chk_scan(&scanner, 15);
        assert!(matches!(*ce, CallExpression::CallExpressionTemplateLiteral(..)));
        format!("{ce:?}");
        pretty_check(
            &*ce,
            "CallExpression: blue ( pop ) `snap`",
            &["CallExpression: blue ( pop )", "TemplateLiteral: `snap`"],
        );
        concise_check(
            &*ce,
            "CallExpression: blue ( pop ) `snap`",
            &["CallMemberExpression: blue ( pop )", "NoSubTemplate: `snap`"],
        );
    }
    #[test]
    fn nomatch() {
        check_err(
            CallExpression::parse(&mut newparser(""), Scanner::new(), false, false),
            "CallExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn incomplete_01() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)["), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(*ce, CallExpression::CallMemberExpression(_)));
    }
    #[test]
    fn incomplete_02() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[99"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(*ce, CallExpression::CallMemberExpression(_)));
    }
    #[test]
    fn incomplete_03() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)."), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(*ce, CallExpression::CallMemberExpression(_)));
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = CallExpression::parse(&mut newparser("super(b)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = CallExpression::parse(&mut newparser("import(b)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_4() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)(c)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_5() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)[c]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_6() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b).c"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_7() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)`c`"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_8() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b).#c"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = CallExpression::parse(&mut newparser("super(b)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = CallExpression::parse(&mut newparser("import(b)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_4() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)(c)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_5() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)[c]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_6() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b).c"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_7() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)`c`"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_8() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b).#c"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = CallExpression::parse(&mut newparser("a(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = CallExpression::parse(&mut newparser("super(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = CallExpression::parse(&mut newparser("super(a)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) = CallExpression::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_06() {
        let (item, _) = CallExpression::parse(&mut newparser("import(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_07() {
        let (item, _) = CallExpression::parse(&mut newparser("a(b)(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) = CallExpression::parse(&mut newparser("a(this)(b)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_09() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)(b)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_10() {
        let (item, _) = CallExpression::parse(&mut newparser("a(this)[0]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_11() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)[this]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_12() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)[0]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_13() {
        let (item, _) = CallExpression::parse(&mut newparser("a(this).b"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_14() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0).b"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_15() {
        let (item, _) = CallExpression::parse(&mut newparser("a(this)`hi`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_16() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)`${this}`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_17() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0)`${1}`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_18() {
        let (item, _) = CallExpression::parse(&mut newparser("a(0).#b"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("a(b.#valid)" => true; "standard call valid")]
    #[test_case("super(a.#valid)" => true; "supercall valid")]
    #[test_case("import(a.#valid)" => true; "importcall valid")]
    #[test_case("a(b.#valid)(c)" => true; "CallExpression Arguments ce valid")]
    #[test_case("a(b)(c.#valid)" => true; "CallExpression Arguments args valid")]
    #[test_case("a(b.#valid)[c]" => true; "CallExpression [ Expression ] ce valid")]
    #[test_case("a(b)[c.#valid]" => true; "CallExpression [ Expression ] exp valid")]
    #[test_case("a(b.#valid).c" => true; "CallExpression . IdentifierName ce valid")]
    #[test_case("a(b.#valid)`${c}`" => true; "CallExpression TemplateLiteral ce valid")]
    #[test_case("a(b)`${c.#valid}`" => true; "CallExpression TemplateLiteral tl valid")]
    #[test_case("a(b.#invalid)" => false; "standard call invalid")]
    #[test_case("super(a.#invalid)" => false; "supercall invalid")]
    #[test_case("import(a.#invalid)" => false; "importcall invalid")]
    #[test_case("a(b.#invalid)(c)" => false; "CallExpression Arguments ce invalid")]
    #[test_case("a(b)(c.#invalid)" => false; "CallExpression Arguments args invalid")]
    #[test_case("a(b.#invalid)[c]" => false; "CallExpression [ Expression ] ce invalid")]
    #[test_case("a(b)[c.#invalid]" => false; "CallExpression [ Expression ] exp invalid")]
    #[test_case("a(b.#invalid).c" => false; "CallExpression . IdentifierName ce invalid")]
    #[test_case("a(b.#invalid)`${c}`" => false; "CallExpression TemplateLiteral ce invalid")]
    #[test_case("a(b)`${c.#invalid}`" => false; "CallExpression TemplateLiteral tl invalid")]
    #[test_case("a(b.#valid).#other" => true; "CallExpression . PrivateIdentifier both valid")]
    #[test_case("a(b.#invalid).#other" => false; "CallExpression . PrivateIdentifier ce invalid")]
    #[test_case("a(b.#valid).#nonsense" => false; "CallExpression . PrivateIdentifier pi invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = CallExpression::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid"), JSString::from("#other")])
    }

    #[test_case("package(0)", true => sset(&[PACKAGE_NOT_ALLOWED]); "cover exp")]
    #[test_case("super(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "super call")]
    #[test_case("import(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "import call")]
    #[test_case("package(0)(1)", true => sset(&[PACKAGE_NOT_ALLOWED]); "call args; id bad")]
    #[test_case("a(0)(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "call args; args bad")]
    #[test_case("package(0)[a]", true => sset(&[PACKAGE_NOT_ALLOWED]); "call array; id bad")]
    #[test_case("a(0)[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "call array; exp bad")]
    #[test_case("package(0).id", true => sset(&[PACKAGE_NOT_ALLOWED]); "call id")]
    #[test_case("package(0).#id", true => sset(&[PACKAGE_NOT_ALLOWED]); "call private")]
    #[test_case("package(0)`${0}`", true => sset(&[PACKAGE_NOT_ALLOWED]); "call templ; id bad")]
    #[test_case("a(0)`${package}`", true => sset(&[PACKAGE_NOT_ALLOWED]); "call templ; templ bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        CallExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a()" => true; "standard")]
    #[test_case("super()" => true; "super call")]
    #[test_case("import(a)" => true; "import")]
    #[test_case("a()()" => true; "call on call")]
    #[test_case("a()[0]" => true; "exp lookup")]
    #[test_case("a().b" => true; "member")]
    #[test_case("a()`${1}`" => true; "template")]
    #[test_case("a().#u" => false; "private id")]
    fn is_strictly_deletable(src: &str) -> bool {
        CallExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments()" => true; "CallMemberExpression (yes)")]
    #[test_case("super(arguments)" => true; "SuperCall (yes)")]
    #[test_case("import(arguments)" => true; "ImportCall (yes)")]
    #[test_case("arguments()(bob)" => true; "Call Args (left)")]
    #[test_case("bob()(arguments)" => true; "Call Args (right)")]
    #[test_case("arguments()[bob]" => true; "Call [Exp] (left)")]
    #[test_case("bob()[arguments]" => true; "Call [Exp] (right)")]
    #[test_case("arguments().name" => true; "Call . Id (yes)")]
    #[test_case("arguments()`${bob}`" => true; "Call Template (left)")]
    #[test_case("bob()`${arguments}`" => true; "Call Template (right)")]
    #[test_case("arguments().#pid" => true; "Call . Private (yes)")]
    #[test_case("xyzzy()" => false; "CallMemberExpression (no)")]
    #[test_case("super(xyzzy)" => false; "SuperCall (no)")]
    #[test_case("import(xyzzy)" => false; "ImportCall (no)")]
    #[test_case("xyzzy()(bob)" => false; "Call Args (no)")]
    #[test_case("xyzzy()[bob]" => false; "Call [Exp] (no)")]
    #[test_case("xyzzy().name" => false; "Call . Id (no)")]
    #[test_case("xyzzy()`${bob}`" => false; "Call Template (no)")]
    #[test_case("xyzzy().#pid" => false; "Call . Private (no)")]
    fn contains_arguments(src: &str) -> bool {
        CallExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("a()" => ATTKind::Invalid; "CallMemberExpression")]
    #[test_case("super()" => ATTKind::Invalid; "SuperCall")]
    #[test_case("import(a)" => ATTKind::Invalid; "ImportCall")]
    #[test_case("a()()" => ATTKind::Invalid; "CallExpression Arguments")]
    #[test_case("a()[0]" => ATTKind::Simple; "CallExpression [ Expression ]")]
    #[test_case("a().a" => ATTKind::Simple; "CallExpression . IdentifierName")]
    #[test_case("a()`${b}`" => ATTKind::Invalid; "CallExpression TemplateLiteral")]
    #[test_case("a().#a" => ATTKind::Simple; "CallExpression . PrivateIdentifier")]
    fn assignment_target_type(src: &str) -> ATTKind {
        Maker::new(src).call_expression().assignment_target_type()
    }

    #[test_case("  a()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "CallMemberExpression")]
    #[test_case("  super()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "SuperCall")]
    #[test_case("  import(a)" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 9 }}; "ImportCall")]
    #[test_case("  a()()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "CallExpression Arguments")]
    #[test_case("  a()[0]" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "CallExpression [ Expression ]")]
    #[test_case("  a().a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "CallExpression . IdentifierName")]
    #[test_case("  a()`${b}`" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 9 }}; "CallExpression TemplateLiteral")]
    #[test_case("  a().#a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "CallExpression . PrivateIdentifier")]
    fn location(src: &str) -> Location {
        Maker::new(src).call_expression().location()
    }
}

// OPTIONAL EXPRESSION
mod optional_expression {
    use super::*;
    use test_case::test_case;
    #[test]
    fn parse_01() {
        let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*lhs, OptionalExpression::Member(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalExpression: a ?. b", &["MemberExpression: a", "OptionalChain: ?. b"]);
        concise_check(&*lhs, "OptionalExpression: a ?. b", &["IdentifierName: a", "OptionalChain: ?. b"]);
    }
    #[test]
    fn parse_02() {
        let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(*lhs, OptionalExpression::Call(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalExpression: a ( ) ?. b", &["CallExpression: a ( )", "OptionalChain: ?. b"]);
        concise_check(&*lhs, "OptionalExpression: a ( ) ?. b", &["CallMemberExpression: a ( )", "OptionalChain: ?. b"]);
    }
    #[test]
    fn parse_03() {
        let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(*lhs, OptionalExpression::Opt(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalExpression: a ?. b ?. c", &["OptionalExpression: a ?. b", "OptionalChain: ?. c"]);
        concise_check(&*lhs, "OptionalExpression: a ?. b ?. c", &["OptionalExpression: a ?. b", "OptionalChain: ?. c"]);
    }
    #[test]
    fn parse_04() {
        check_err(
            OptionalExpression::parse(&mut newparser(""), Scanner::new(), false, false),
            "OptionalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_05() {
        check_err(OptionalExpression::parse(&mut newparser("10"), Scanner::new(), false, false), "‘?.’ expected", 1, 3);
    }
    #[test]
    fn parse_06() {
        check_err(
            OptionalExpression::parse(&mut newparser("u()"), Scanner::new(), false, false),
            "‘?.’ expected",
            1,
            4,
        );
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = OptionalExpression::parse(&mut newparser("this?.(a)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_03() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a?.(b)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            OptionalExpression::parse(&mut newparser("a(this)?.(b)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            OptionalExpression::parse(&mut newparser("a(0)?.(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_06() {
        let (item, _) = OptionalExpression::parse(&mut newparser("a(0)?.(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_07() {
        let (item, _) =
            OptionalExpression::parse(&mut newparser("a(this)?.(0)?.(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) =
            OptionalExpression::parse(&mut newparser("a(0)?.(0)?.(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_09() {
        let (item, _) =
            OptionalExpression::parse(&mut newparser("a(0)?.(0)?.(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("a.#valid?.(b)" => true; "MemberExpression OptionalChain me valid")]
    #[test_case("a?.(b.#valid)" => true; "MemberExpression OptionalChain oc valid")]
    #[test_case("a(b.#valid)?.(c)" => true; "CallExpression OptionalChain ce valid")]
    #[test_case("a()?.(b.#valid)" => true; "CallExpression OptionalChain oc valid")]
    #[test_case("a.#valid?.(b)?.(c)" => true; "OptionalExpression OptionalChain oe valid")]
    #[test_case("a?.(b)?.(c.#valid)" => true; "OptionalExpression OptionalChain oc valid")]
    #[test_case("a.#invalid?.(b)" => false; "MemberExpression OptionalChain me invalid")]
    #[test_case("a?.(b.#invalid)" => false; "MemberExpression OptionalChain oc invalid")]
    #[test_case("a(b.#invalid)?.(c)" => false; "CallExpression OptionalChain ce invalid")]
    #[test_case("a()?.(b.#invalid)" => false; "CallExpression OptionalChain oc invalid")]
    #[test_case("a.#invalid?.(b)?.(c)" => false; "OptionalExpression OptionalChain oe invalid")]
    #[test_case("a?.(b)?.(c.#invalid)" => false; "OptionalExpression OptionalChain oc invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = OptionalExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid"), JSString::from("#other")])
    }

    #[test_case("package?.a", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp opt; exp bad")]
    #[test_case("a?.(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp opt; opt bad")]
    #[test_case("package()?.a", true => sset(&[PACKAGE_NOT_ALLOWED]); "call opt; call bad")]
    #[test_case("a()?.(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "call opt; opt bad")]
    #[test_case("package?.a?.(0)", true => sset(&[PACKAGE_NOT_ALLOWED]); "opt opt; head bad")]
    #[test_case("a?.b?.(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "opt opt; tail bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        OptionalExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a?.b" => true; "member exp")]
    #[test_case("a?.#b" => false; "private member exp")]
    #[test_case("a()?.b" => true; "call exp")]
    #[test_case("a()?.#b" => false; "private call exp")]
    #[test_case("a?.b?.c" => true; "optional exp")]
    #[test_case("a?.b?.#c" => false; "private optional exp")]
    fn is_strictly_deletable(src: &str) -> bool {
        OptionalExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments?.[bob]" => true; "ME OC (left)")]
    #[test_case("bob?.[arguments]" => true; "ME OC (right)")]
    #[test_case("alice(arguments)?.[bob]" => true; "Call OC (left)")]
    #[test_case("alice(bob)?.[arguments]" => true; "Call OC (right)")]
    #[test_case("arguments?.[alice]?.[bob]" => true; "Opt OC (left)")]
    #[test_case("bob?.[alice]?.[arguments]" => true; "Opt OC (right)")]
    #[test_case("xyzzy?.[bob]" => false; "ME OC (no)")]
    #[test_case("alice(xyzzy)?.[bob]" => false; "Call OC (no)")]
    #[test_case("xyzzy?.[alice]?.[bob]" => false; "Opt OC (no)")]
    fn contains_arguments(src: &str) -> bool {
        OptionalExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  a?.b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "member exp")]
    #[test_case("  a?.#b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "private member exp")]
    #[test_case("  a()?.b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "call exp")]
    #[test_case("  a()?.#b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "private call exp")]
    #[test_case("  a?.b?.c" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "optional exp")]
    #[test_case("  a?.b?.#c" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "private optional exp")]
    fn location(src: &str) -> Location {
        Maker::new(src).optional_expression().location()
    }
}

// OPTIONAL CHAIN
mod optional_chain {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*lhs, OptionalChain::Args(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. ( )", &["Arguments: ( )"]);
        concise_check(&*lhs, "OptionalChain: ?. ( )", &["Punctuator: ?.", "Arguments: ( )"]);
    }
    #[test]
    fn parse_02() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.[1 in a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*lhs, OptionalChain::Exp(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. [ 1 in a ]", &["Expression: 1 in a"]);
        concise_check(
            &*lhs,
            "OptionalChain: ?. [ 1 in a ]",
            &["Punctuator: ?.", "Punctuator: [", "RelationalExpression: 1 in a", "Punctuator: ]"],
        );
    }
    #[test]
    fn parse_03() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*lhs, OptionalChain::Ident(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a", &["IdentifierName: a"]);
        concise_check(&*lhs, "OptionalChain: ?. a", &["Punctuator: ?.", "IdentifierName: a"]);
    }
    #[test]
    fn parse_04() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(*lhs, OptionalChain::Template(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. `a`", &["TemplateLiteral: `a`"]);
        concise_check(&*lhs, "OptionalChain: ?. `a`", &["Punctuator: ?.", "NoSubTemplate: `a`"]);
    }
    #[test]
    fn parse_05() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(*lhs, OptionalChain::PlusArgs(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a ( )", &["OptionalChain: ?. a", "Arguments: ( )"]);
        concise_check(&*lhs, "OptionalChain: ?. a ( )", &["OptionalChain: ?. a", "Arguments: ( )"]);
    }
    #[test]
    fn parse_06() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a[0 in b]"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        assert!(matches!(*lhs, OptionalChain::PlusExp(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a [ 0 in b ]", &["OptionalChain: ?. a", "Expression: 0 in b"]);
        concise_check(
            &*lhs,
            "OptionalChain: ?. a [ 0 in b ]",
            &["OptionalChain: ?. a", "Punctuator: [", "RelationalExpression: 0 in b", "Punctuator: ]"],
        );
    }
    #[test]
    fn parse_07() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(*lhs, OptionalChain::PlusIdent(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a . b", &["OptionalChain: ?. a", "IdentifierName: b"]);
        concise_check(&*lhs, "OptionalChain: ?. a . b", &["OptionalChain: ?. a", "Punctuator: .", "IdentifierName: b"]);
    }
    #[test]
    fn parse_08() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(*lhs, OptionalChain::PlusTemplate(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a `b`", &["OptionalChain: ?. a", "TemplateLiteral: `b`"]);
        concise_check(&*lhs, "OptionalChain: ?. a `b`", &["OptionalChain: ?. a", "NoSubTemplate: `b`"]);
    }
    #[test]
    fn parse_09() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*lhs, OptionalChain::PrivateId(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. #a", &["PrivateIdentifier: #a"]);
        concise_check(&*lhs, "OptionalChain: ?. #a", &["Punctuator: ?.", "PrivateIdentifier: #a"]);
    }
    #[test]
    fn parse_10() {
        let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(*lhs, OptionalChain::PlusPrivateId(..)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "OptionalChain: ?. a . #b", &["OptionalChain: ?. a", "PrivateIdentifier: #b"]);
        concise_check(
            &*lhs,
            "OptionalChain: ?. a . #b",
            &["OptionalChain: ?. a", "Punctuator: .", "PrivateIdentifier: #b"],
        );
    }
    #[test]
    fn err_1() {
        check_err(OptionalChain::parse(&mut newparser(""), Scanner::new(), false, false), "‘?.’ expected", 1, 1);
    }
    #[test]
    fn err_2() {
        check_err(
            OptionalChain::parse(&mut newparser("?."), Scanner::new(), false, false),
            "‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)",
            1,
            3,
        );
    }
    #[test]
    fn err_3() {
        let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a."), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*oc, OptionalChain::Ident(..)));
    }
    #[test]
    fn err_4() {
        let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a["), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*oc, OptionalChain::Ident(..)));
    }
    #[test]
    fn err_5() {
        let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a[0"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*oc, OptionalChain::Ident(..)));
    }
    #[test]
    fn err_6() {
        check_err(
            OptionalChain::parse(&mut newparser("?.["), Scanner::new(), false, false),
            "Expression expected",
            1,
            4,
        );
    }
    #[test]
    fn err_7() {
        check_err(OptionalChain::parse(&mut newparser("?.[0"), Scanner::new(), false, false), "‘]’ expected", 1, 5);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_4() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_5() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_6() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a[0]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_7() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_8() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_9() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_10() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_4() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_5() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_6() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a[0]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_7() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_8() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_9() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_10() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.[this]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_05() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_06() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.`${this}`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_07() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.`${0}`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_08() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this)(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_09() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_10() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_11() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this)[0]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_12() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)[this]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_13() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)[0]"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_14() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this).a"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_15() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0).this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_16() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this)`a`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_17() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)`${this}`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_18() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0)`a`"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_19() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.#this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_20() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(this).#a"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_21() {
        let (item, _) = OptionalChain::parse(&mut newparser("?.(0).#this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("?.a" => true; "?. IdentifierName")]
    #[test_case("?.(a.#valid)" => true; "?. Arguments valid")]
    #[test_case("?.[a.#valid]" => true; "?. [ Expression ] valid")]
    #[test_case("?.`${a.#valid}`" => true; "?. TemplateLiteral valid")]
    #[test_case("?.#valid" => true; "?. PrivateIdentifier valid")]
    #[test_case("?.[a.#valid](b)" => true; "OptionalChain Arguments oc valid")]
    #[test_case("?.[a](b.#valid)" => true; "OptionalChain Arguments args valid")]
    #[test_case("?.[a.#valid][b]" => true; "OptionalChain [ Expression ] oc valid")]
    #[test_case("?.[a][b.#valid]" => true; "OptionalChain [ Expression ] expr valid")]
    #[test_case("?.[a.#valid].b" => true; "OptionalChain . IdentifierName valid")]
    #[test_case("?.[a.#valid]`${b}`" => true; "OptionalChain TemplateLiteral oc valid")]
    #[test_case("?.[a]`${b.#valid}`" => true; "OptionalChain TemplateLiteral tl valid")]
    #[test_case("?.(a.#invalid)" => false; "?. Arguments invalid")]
    #[test_case("?.[a.#invalid]" => false; "?. [ Expression ] invalid")]
    #[test_case("?.`${a.#invalid}`" => false; "?. TemplateLiteral invalid")]
    #[test_case("?.#invalid" => false; "?. PrivateIdentifier invalid")]
    #[test_case("?.[a.#invalid](b)" => false; "OptionalChain Arguments oc invalid")]
    #[test_case("?.[a](b.#invalid)" => false; "OptionalChain Arguments args invalid")]
    #[test_case("?.[a.#invalid][b]" => false; "OptionalChain [ Expression ] oc invalid")]
    #[test_case("?.[a][b.#invalid]" => false; "OptionalChain [ Expression ] expr invalid")]
    #[test_case("?.[a.#invalid].b" => false; "OptionalChain . IdentifierName invalid")]
    #[test_case("?.[a.#invalid]`${b}`" => false; "OptionalChain TemplateLiteral oc invalid")]
    #[test_case("?.[a]`${b.#invalid}`" => false; "OptionalChain TemplateLiteral tl invalid")]
    #[test_case("?.[a.#valid].#alsovalid" => true; "OptionalChain . PrivateIdentifier both valid")]
    #[test_case("?.[a.#invalid].#alsovalid" => false; "OptionalChain . PrivateIdentifier oc invalid")]
    #[test_case("?.[a.#valid].#invalid" => false; "OptionalChain . PrivateIdentifier pi invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = OptionalChain::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid"), JSString::from("#alsovalid")])
    }

    const TEMPLATE_NOT_ALLOWED: &str = "Template literal not allowed here";

    #[test_case("?.(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "args")]
    #[test_case("?.[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "expression")]
    #[test_case("?.package", true => AHashSet::<String>::new(); "ident")]
    #[test_case("?.`${package}`", true => sset(&[PACKAGE_NOT_ALLOWED, TEMPLATE_NOT_ALLOWED]); "template lit")]
    #[test_case("?.#package", true => AHashSet::<String>::new(); "private")]
    #[test_case("?.(package)(interface)", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "chain args")]
    #[test_case("?.(package)[interface]", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "chain expression")]
    #[test_case("?.(package).interface", true => sset(&[PACKAGE_NOT_ALLOWED]); "chain ident")]
    #[test_case("?.(package)`${interface}`", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, TEMPLATE_NOT_ALLOWED]); "chain template lit")]
    #[test_case("?.(package).#interface", true => sset(&[PACKAGE_NOT_ALLOWED]); "chain private")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        OptionalChain::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("?.()" => true; "args")]
    #[test_case("?.[0]" => true; "expression")]
    #[test_case("?.a" => true; "member")]
    #[test_case("?.`${0}`" => true; "template")]
    #[test_case("?.#a" => false; "private")]
    #[test_case("?.z()" => true; "chain args")]
    #[test_case("?.z[0]" => true; "chain expression")]
    #[test_case("?.z.a" => true; "chain member")]
    #[test_case("?.z`${0}`" => true; "chain template")]
    #[test_case("?.z.#a" => false; "chain private")]
    fn is_strictly_deletable(src: &str) -> bool {
        OptionalChain::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("?.arguments" => false; "ident")]
    #[test_case("?.#arguments" => false; "private")]
    #[test_case("?.(arguments)" => true; "Args (yes)")]
    #[test_case("?.[arguments]" => true; "Exp (yes)")]
    #[test_case("?.`${arguments}`" => true; "Template (yes)")]
    #[test_case("?.(arguments)(bob)" => true; "OC Args (left)")]
    #[test_case("?.(bob)(arguments)" => true; "OC Args (right)")]
    #[test_case("?.(arguments)[bob]" => true; "OC Exp (left)")]
    #[test_case("?.(bob)[arguments]" => true; "OC Exp (right)")]
    #[test_case("?.(arguments).bob" => true; "OC Ident (yes)")]
    #[test_case("?.(arguments)`${bob}`" => true; "OC Template (left)")]
    #[test_case("?.(bob)`${arguments}`" => true; "OC Template (right)")]
    #[test_case("?.(arguments).#bob" => true; "OC Private (yes)")]
    #[test_case("?.(xyzzy)" => false; "Args (no)")]
    #[test_case("?.[xyzzy]" => false; "Exp (no)")]
    #[test_case("?.`${xyzzy}`" => false; "Template (no)")]
    #[test_case("?.(xyzzy)(bob)" => false; "OC Args (no)")]
    #[test_case("?.(xyzzy)[bob]" => false; "OC Exp (no)")]
    #[test_case("?.(xyzzy).bob" => false; "OC Ident (no)")]
    #[test_case("?.(xyzzy)`${bob}`" => false; "OC Template (no)")]
    #[test_case("?.(xyzzy).#bob" => false; "OC Private (no)")]
    fn contains_arguments(src: &str) -> bool {
        OptionalChain::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("  ?.()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "args")]
    #[test_case("  ?.[0]" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "expression")]
    #[test_case("  ?.a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "member")]
    #[test_case("  ?.`${0}`" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "template")]
    #[test_case("  ?.#a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "private")]
    #[test_case("  ?.z()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "chain args")]
    #[test_case("  ?.z[0]" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "chain expression")]
    #[test_case("  ?.z.a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "chain member")]
    #[test_case("  ?.z`${0}`" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 9 }}; "chain template")]
    #[test_case("  ?.z.#a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "chain private")]
    fn location(src: &str) -> Location {
        Maker::new(src).optional_chain().location()
    }
}

// LEFT-HAND-SIDE EXPRESSION
mod left_hand_side_expression {
    use super::*;
    use test_case::test_case;
    #[test]
    fn parse_01() {
        let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*lhs, LeftHandSideExpression::New(_)));
        format!("{lhs:?}");
        pretty_check(&*lhs, "LeftHandSideExpression: a", &["NewExpression: a"]);
        concise_check(&*lhs, "IdentifierName: a", &[]);
        assert_eq!(lhs.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*lhs, LeftHandSideExpression::Call(_)));
        pretty_check(&*lhs, "LeftHandSideExpression: a ( )", &["CallExpression: a ( )"]);
        concise_check(&*lhs, "CallMemberExpression: a ( )", &["IdentifierName: a", "Arguments: ( )"]);
        assert_eq!(lhs.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (lhs, scanner) =
            check(LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(*lhs, LeftHandSideExpression::Optional(_)));
        pretty_check(&*lhs, "LeftHandSideExpression: a ( ) ?. b", &["OptionalExpression: a ( ) ?. b"]);
        concise_check(&*lhs, "OptionalExpression: a ( ) ?. b", &["CallMemberExpression: a ( )", "OptionalChain: ?. b"]);
        assert_eq!(lhs.is_function_definition(), false);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a(this)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a(0)"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_05() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("this?.n"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_06() {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser("a?.n"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("die()" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("a.#valid" => true; "NewExpression valid")]
    #[test_case("a(b.#valid)" => true; "CallExpression valid")]
    #[test_case("a?.[b.#valid]" => true; "OptionalExpression valid")]
    #[test_case("a.#invalid" => false; "NewExpression invalid")]
    #[test_case("a(b.#invalid)" => false; "CallExpression invalid")]
    #[test_case("a?.[b.#invalid]" => false; "OptionalExpression invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("{}" => true; "ObjectLiteral")]
    #[test_case("3" => false; "Other Literal")]
    #[test_case("a()" => false; "CallExpression")]
    #[test_case("blue?.green" => false; "OptionalExpression")]
    fn is_object_or_array_literal(src: &str) -> bool {
        LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_object_or_array_literal()
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "new expression")]
    #[test_case("package()", true => sset(&[PACKAGE_NOT_ALLOWED]); "call expression")]
    #[test_case("package?.()", true => sset(&[PACKAGE_NOT_ALLOWED]); "optional expression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a()" => true; "call expression")]
    #[test_case("a().#u" => false; "call expression with private id")]
    #[test_case("a?.b" => true; "optional expression")]
    #[test_case("a?.#u" => false; "optional with private id")]
    fn is_strictly_deletable(src: &str) -> bool {
        LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "new exp (yes)")]
    #[test_case("arguments()" => true; "call exp (yes)")]
    #[test_case("arguments?.b" => true; "optional (yes)")]
    #[test_case("xyzzy" => false; "new exp (no)")]
    #[test_case("xyzzy()" => false; "call exp (no)")]
    #[test_case("xyzzy?.b" => false; "optional (no)")]
    fn contains_arguments(src: &str) -> bool {
        LeftHandSideExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a()", false => ATTKind::Invalid; "invalid call expression")]
    #[test_case("a().a", false => ATTKind::Simple; "valid call expression")]
    #[test_case("a?.b", false => ATTKind::Invalid; "optional expression")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).left_hand_side_expression().assignment_target_type(strict)
    }

    #[test_case("idref" => true; "Id Ref")]
    #[test_case("this" => false; "this kwd")]
    #[test_case("10" => false; "literal")]
    #[test_case("[10, 11, 12]" => false; "array literal")]
    #[test_case("{ a: 12 }" => false; "object literal")]
    #[test_case("function a(){}" => false; "function expression")]
    #[test_case("function *a(){}" => false; "generator expression")]
    #[test_case("async function () {}" => false; "async func expr")]
    #[test_case("async function *(){}" => false; "async gen expr")]
    #[test_case("/abcd/" => false; "regex literal")]
    #[test_case("`template`" => false; "template literal")]
    #[test_case("(id)" => false; "parentheszied expr")]
    #[test_case("a[0]" => false; "expression member access")]
    #[test_case("a.a" => false; "name member access")]
    #[test_case("a`${b}`" => false; "tagged template")]
    #[test_case("super.a" => false; "super prop")]
    #[test_case("new.target" => false; "meta prop")]
    #[test_case("new a()" => false; "new expr")]
    #[test_case("a.#b" => false; "private member access")]
    #[test_case("new a" => false; "other new expr")]
    #[test_case("a()" => false; "call")]
    #[test_case("a?.b" => false; "optional")]
    fn is_identifier_ref(src: &str) -> bool {
        Maker::new(src).left_hand_side_expression().is_identifier_ref()
    }

    #[test_case("idref" => ssome("idref"); "Id Ref")]
    #[test_case("this" => None; "this kwd")]
    #[test_case("10" => None; "literal")]
    #[test_case("[10, 11, 12]" => None; "array literal")]
    #[test_case("{ a: 12 }" => None; "object literal")]
    #[test_case("function a(){}" => None; "function expression")]
    #[test_case("function *a(){}" => None; "generator expression")]
    #[test_case("async function () {}" => None; "async func expr")]
    #[test_case("async function *(){}" => None; "async gen expr")]
    #[test_case("/abcd/" => None; "regex literal")]
    #[test_case("`template`" => None; "template literal")]
    #[test_case("(id)" => None; "parentheszied expr")]
    #[test_case("a[0]" => None; "expression member access")]
    #[test_case("a.a" => None; "name member access")]
    #[test_case("a`${b}`" => None; "tagged template")]
    #[test_case("super.a" => None; "super prop")]
    #[test_case("new.target" => None; "meta prop")]
    #[test_case("new a()" => None; "new expr")]
    #[test_case("a.#b" => None; "private member access")]
    #[test_case("new a" => None; "other new expr")]
    #[test_case("a()" => None; "call")]
    #[test_case("a?.b" => None; "optional")]
    fn identifier_ref(src: &str) -> Option<String> {
        Maker::new(src).left_hand_side_expression().identifier_ref().map(|id| id.to_string())
    }

    #[test_case("  1" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1 }}; "literal")]
    #[test_case("  a()" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "call expression")]
    #[test_case("  a?.b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "optional expression")]
    fn location(src: &str) -> Location {
        Maker::new(src).left_hand_side_expression().location()
    }

    #[test_case("1" => false; "literal")]
    #[test_case("a()" => false; "call expression")]
    #[test_case("a?.b" => false; "optional expression")]
    #[test_case("function bob(){}" => true; "function")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).left_hand_side_expression().is_named_function()
    }

    #[test_case("1" => false; "literal")]
    #[test_case("[a]" => true; "destructuring")]
    #[test_case("a()" => false; "call expression")]
    #[test_case("a?.b" => false; "optional expression")]
    fn is_destructuring(src: &str) -> bool {
        Maker::new(src).left_hand_side_expression().is_destructuring()
    }
}

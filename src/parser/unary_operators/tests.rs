#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// UNARY EXPRESSION
mod unary_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn update_expression() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("900"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UnaryExpression::UpdateExpression(_)));
        pretty_check(&*ue, "UnaryExpression: 900", &["UpdateExpression: 900"]);
        concise_check(&*ue, "Numeric: 900", &[]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn delete() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("delete bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Delete { .. }));
        pretty_check(&*ue, "UnaryExpression: delete bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: delete bob", &["Keyword: delete", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn void() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("void bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(*ue, UnaryExpression::Void { .. }));
        pretty_check(&*ue, "UnaryExpression: void bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: void bob", &["Keyword: void", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn r#typeof() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("typeof bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Typeof { .. }));
        pretty_check(&*ue, "UnaryExpression: typeof bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: typeof bob", &["Keyword: typeof", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn numberify() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("+bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::NoOp { .. }));
        pretty_check(&*ue, "UnaryExpression: + bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: + bob", &["Punctuator: +", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn negate() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("-bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Negate { .. }));
        pretty_check(&*ue, "UnaryExpression: - bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: - bob", &["Punctuator: -", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn complement() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("~bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Complement { .. }));
        pretty_check(&*ue, "UnaryExpression: ~ bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: ~ bob", &["Punctuator: ~", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn not() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("!bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Not { .. }));
        pretty_check(&*ue, "UnaryExpression: ! bob", &["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: ! bob", &["Punctuator: !", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn await_bob() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
        chk_scan(&scanner, 9);
        assert!(matches!(*ue, UnaryExpression::Await(_)));
        pretty_check(&*ue, "UnaryExpression: await bob", &["AwaitExpression: await bob"]);
        concise_check(&*ue, "AwaitExpression: await bob", &["Keyword: await", "IdentifierName: bob"]);
        assert_ne!(format!("{ue:?}"), "");
    }
    #[test]
    fn nomatch() {
        check_err(
            UnaryExpression::parse(&mut newparser(""), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn incomplete() {
        check_err(
            UnaryExpression::parse(&mut newparser("delete"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            7,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("void"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            5,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("typeof"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            7,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("+"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            2,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("-"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            2,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("~"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            2,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("!"), Scanner::new(), false, false),
            "UnaryExpression expected",
            1,
            2,
        );
        check_err(
            UnaryExpression::parse(&mut newparser("await"), Scanner::new(), false, true),
            "UnaryExpression expected",
            1,
            6,
        );
    }

    #[test]
    fn prettyerrors_1() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_4() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_5() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_6() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_7() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_8() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_4() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_5() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_6() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_7() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_8() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn cache_01() {
        let mut parser = newparser("void blue");
        let (node, scanner) = check(UnaryExpression::parse(&mut parser, Scanner::new(), false, false));
        let (node2, scanner2) = check(UnaryExpression::parse(&mut parser, Scanner::new(), false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn contains_01() {
        let (item, _) = UnaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_02() {
        let (item, _) = UnaryExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_04() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_05() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_06() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_07() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_08() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_09() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_10() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_11() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_12() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_13() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_14() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_15() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_16() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! p"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
    }
    #[test]
    fn contains_17() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await this"), Scanner::new(), false, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), true);
    }
    #[test]
    fn contains_18() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await p"), Scanner::new(), false, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
        assert_eq!(item.contains(ParseNodeKind::AwaitExpression), true);
    }
    #[test_case("\"string\"" => Some(String::from("string")); "String Token")]
    #[test_case("-a" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("item.#valid" => true; "FallThru valid")]
    #[test_case("delete item.#valid" => true; "Delete valid")]
    #[test_case("void item.#valid" => true; "Void valid")]
    #[test_case("typeof item.#valid" => true; "Typeof valid")]
    #[test_case("+item.#valid" => true; "NoOp valid")]
    #[test_case("-item.#valid" => true; "Negate valid")]
    #[test_case("~item.#valid" => true; "Complement valid")]
    #[test_case("!item.#valid" => true; "Not valid")]
    #[test_case("await item.#valid" => true; "Await valid")]
    #[test_case("item.#invalid" => false; "FallThru invalid")]
    #[test_case("delete item.#invalid" => false; "Delete invalid")]
    #[test_case("void item.#invalid" => false; "Void invalid")]
    #[test_case("typeof item.#invalid" => false; "Typeof invalid")]
    #[test_case("+item.#invalid" => false; "NoOp invalid")]
    #[test_case("-item.#invalid" => false; "Negate invalid")]
    #[test_case("~item.#invalid" => false; "Complement invalid")]
    #[test_case("!item.#invalid" => false; "Not invalid")]
    #[test_case("await item.#invalid" => false; "Await invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    const ITEM_NOT_DELETABLE: &str = "Item is not deletable";

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall-thru")]
    #[test_case("delete package", true => sset(&[PACKAGE_NOT_ALLOWED, ITEM_NOT_DELETABLE]); "delete")]
    #[test_case("delete (((foo)))", true => sset(&[ITEM_NOT_DELETABLE]); "nested ref")]
    #[test_case("delete a", false => AHashSet::<String>::new(); "non-strict delete")]
    #[test_case("void package", true => sset(&[PACKAGE_NOT_ALLOWED]); "void")]
    #[test_case("typeof package", true => sset(&[PACKAGE_NOT_ALLOWED]); "typeof_")]
    #[test_case("+ package", true => sset(&[PACKAGE_NOT_ALLOWED]); "noop")]
    #[test_case("- package", true => sset(&[PACKAGE_NOT_ALLOWED]); "negate")]
    #[test_case("~ package", true => sset(&[PACKAGE_NOT_ALLOWED]); "complement")]
    #[test_case("! package", true => sset(&[PACKAGE_NOT_ALLOWED]); "not")]
    #[test_case("await package", true => sset(&[PACKAGE_NOT_ALLOWED]); "await_")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        UnaryExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("delete x" => true; "delete")]
    #[test_case("void x" => true; "void")]
    #[test_case("typeof x" => true; "typeof op")]
    #[test_case("+a" => true; "unary plus")]
    #[test_case("-a" => true; "unary minus")]
    #[test_case("~a" => true; "complement")]
    #[test_case("!a" => true; "not")]
    #[test_case("await a" => true; "await op")]
    fn is_strictly_deletable(src: &str) -> bool {
        UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("delete arguments" => true; "delete (yes)")]
    #[test_case("void arguments" => true; "void (yes)")]
    #[test_case("typeof arguments" => true; "typeof (yes)")]
    #[test_case("+arguments" => true; "plus (yes)")]
    #[test_case("-arguments" => true; "minus (yes)")]
    #[test_case("~arguments" => true; "twiddle (yes)")]
    #[test_case("!arguments" => true; "bang (yes)")]
    #[test_case("await arguments" => true; "await (yes)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("delete xyzzy" => false; "delete (no)")]
    #[test_case("void xyzzy" => false; "void (no)")]
    #[test_case("typeof xyzzy" => false; "typeof (no)")]
    #[test_case("+xyzzy" => false; "plus (no)")]
    #[test_case("-xyzzy" => false; "minus (no)")]
    #[test_case("~xyzzy" => false; "twiddle (no)")]
    #[test_case("!xyzzy" => false; "bang (no)")]
    #[test_case("await xyzzy" => false; "await (no)")]
    fn contains_arguments(src: &str) -> bool {
        UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("delete a", false => ATTKind::Invalid; "delete")]
    #[test_case("void a", false => ATTKind::Invalid; "void")]
    #[test_case("typeof a", false => ATTKind::Invalid; "typeof kwd")]
    #[test_case("+a", false => ATTKind::Invalid; "to-number")]
    #[test_case("-a", false => ATTKind::Invalid; "negate")]
    #[test_case("~a", false => ATTKind::Invalid; "complement")]
    #[test_case("!a", false => ATTKind::Invalid; "not")]
    #[test_case("await a", false => ATTKind::Invalid; "await kwd")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).unary_expression().assignment_target_type(strict)
    }

    //#[test_case("-a" => false; "expr")]
    //#[test_case("function bob(){}" => true; "function fallthru")]
    //#[test_case("1" => false; "literal fallthru")]
    //fn is_named_function(src: &str) -> bool {
    //    Maker::new(src).unary_expression().is_named_function()
    //}

    #[test_case("  delete a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "delete")]
    #[test_case("  void a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "void")]
    #[test_case("  typeof a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "typeof kwd")]
    #[test_case("  +a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "to-number")]
    #[test_case("  -a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "negate")]
    #[test_case("  ~a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "complement")]
    #[test_case("  !a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "not")]
    #[test_case("  await a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "await kwd")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).unary_expression().location()
    }
}

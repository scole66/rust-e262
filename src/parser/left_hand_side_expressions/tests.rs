use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// MEMBER EXPRESSION
#[test]
fn member_expression_test_primary_expression() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(me.kind, MemberExpressionKind::PrimaryExpression(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: a", vec!["PrimaryExpression: a"]);
    concise_check(&*me, "IdentifierName: a", vec![]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn member_expression_test_meta_property() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("new.target"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(me.kind, MemberExpressionKind::MetaProperty(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: new . target", vec!["MetaProperty: new . target"]);
    concise_check(&*me, "MetaProperty: new . target", vec!["Keyword: new", "Punctuator: .", "Keyword: target"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn member_expression_test_super_property() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("super.ior"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(me.kind, MemberExpressionKind::SuperProperty(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: super . ior", vec!["SuperProperty: super . ior"]);
    concise_check(&*me, "SuperProperty: super . ior", vec!["Keyword: super", "Punctuator: .", "IdentifierName: ior"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn member_expression_test_new_me_args() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("new shoes('red', 'leather')"), Scanner::new(), false, false));
    chk_scan(&scanner, 27);
    assert!(matches!(me.kind, MemberExpressionKind::NewArguments(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: new shoes ( \"red\" , \"leather\" )", vec!["MemberExpression: shoes", "Arguments: ( \"red\" , \"leather\" )"]);
    concise_check(&*me, "MemberExpression: new shoes ( \"red\" , \"leather\" )", vec!["Keyword: new", "IdentifierName: shoes", "Arguments: ( \"red\" , \"leather\" )"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn member_expression_test_me_expression() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("bill[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(me.kind, MemberExpressionKind::Expression(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: bill [ a ]", vec!["MemberExpression: bill", "Expression: a"]);
    concise_check(&*me, "MemberExpression: bill [ a ]", vec!["IdentifierName: bill", "Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn member_expression_test_me_ident() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice.name"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(me.kind, MemberExpressionKind::IdentifierName(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: alice . name", vec!["MemberExpression: alice"]);
    concise_check(&*me, "MemberExpression: alice . name", vec!["IdentifierName: alice", "Punctuator: .", "IdentifierName: name"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn member_expression_test_me_private() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice.#name"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(me.kind, MemberExpressionKind::PrivateId(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: alice . #name", vec!["MemberExpression: alice"]);
    concise_check(&*me, "MemberExpression: alice . #name", vec!["IdentifierName: alice", "Punctuator: .", "PrivateIdentifier: #name"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn member_expression_test_me_template() {
    let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice`${a}`"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(me.kind, MemberExpressionKind::TemplateLiteral(..)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", me);
    pretty_check(&*me, "MemberExpression: alice `${ a }`", vec!["MemberExpression: alice", "TemplateLiteral: `${ a }`"]);
    concise_check(&*me, "MemberExpression: alice `${ a }`", vec!["IdentifierName: alice", "SubstitutionTemplate: `${ a }`"]);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn member_expression_test_errs_01() {
    check_err(MemberExpression::parse(&mut newparser(""), Scanner::new(), false, false), "MemberExpression expected", 1, 1);
}
#[test]
fn member_expression_test_errs_02() {
    check_err(MemberExpression::parse(&mut newparser("super"), Scanner::new(), false, false), "One of [‘.’, ‘[’] expected", 1, 6);
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

// SUPER PROPERTY
#[test]
fn super_property_test_expression() {
    let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(sp.kind, SuperPropertyKind::Expression(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", sp);
    pretty_check(&*sp, "SuperProperty: super [ a ]", vec!["Expression: a"]);
    concise_check(&*sp, "SuperProperty: super [ a ]", vec!["Keyword: super", "Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
}
#[test]
fn super_property_test_ident() {
    let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super.bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(sp.kind, SuperPropertyKind::IdentifierName(_)));
    // Excersize the Debug formatter, for code coverage
    format!("{:?}", sp);
    pretty_check(&*sp, "SuperProperty: super . bob", vec![]);
    concise_check(&*sp, "SuperProperty: super . bob", vec!["Keyword: super", "Punctuator: .", "IdentifierName: bob"]);
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
    check_err(SuperProperty::parse(&mut newparser("super duper"), Scanner::new(), false, false), "One of [‘.’, ‘[’] expected", 1, 6);
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

// META PROPERTY
#[test]
fn meta_property_test_newtarget() {
    let (mp, scanner) = check(MetaProperty::parse(&mut newparser("new.target"), Scanner::new()));
    chk_scan(&scanner, 10);
    assert!(matches!(mp.kind, MetaPropertyKind::NewTarget));
    format!("{:?}", mp);
    pretty_check(&*mp, "MetaProperty: new . target", vec![]);
    concise_check(&*mp, "MetaProperty: new . target", vec!["Keyword: new", "Punctuator: .", "Keyword: target"]);
}
#[test]
fn meta_property_test_importmeta() {
    let (mp, scanner) = check(MetaProperty::parse(&mut newparser("import.meta"), Scanner::new()));
    chk_scan(&scanner, 11);
    assert!(matches!(mp.kind, MetaPropertyKind::ImportMeta));
    format!("{:?}", mp);
    pretty_check(&*mp, "MetaProperty: import . meta", vec![]);
    concise_check(&*mp, "MetaProperty: import . meta", vec!["Keyword: import", "Punctuator: .", "Keyword: meta"]);
}
#[test]
fn meta_property_test_nomatch_01() {
    check_err(MetaProperty::parse(&mut newparser("silly"), Scanner::new()), "One of [‘new’, ‘import’] expected", 1, 1);
}
#[test]
fn meta_property_test_nomatch_02() {
    check_err(MetaProperty::parse(&mut newparser("new silly"), Scanner::new()), "‘.’ expected", 1, 4);
}
#[test]
fn meta_property_test_nomatch_03() {
    check_err(MetaProperty::parse(&mut newparser("new.silly"), Scanner::new()), "‘target’ expected", 1, 5);
}
#[test]
fn meta_property_test_nomatch_04() {
    check_err(MetaProperty::parse(&mut newparser("import silly"), Scanner::new()), "‘.’ expected", 1, 7);
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

// ARGUMENTS
#[test]
fn arguments_test_onlyparens() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("()"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(args.kind, ArgumentsKind::Empty));
    format!("{:?}", args);
    pretty_check(&*args, "Arguments: ( )", vec![]);
    concise_check(&*args, "Arguments: ( )", vec!["Punctuator: (", "Punctuator: )"]);
}
#[test]
fn arguments_test_trailing_comma() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("(a,)"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(args.kind, ArgumentsKind::ArgumentListComma(_)));
    format!("{:?}", args);
    pretty_check(&*args, "Arguments: ( a , )", vec!["ArgumentList: a"]);
    concise_check(&*args, "Arguments: ( a , )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: )"]);
}
#[test]
fn arguments_test_arglist() {
    let (args, scanner) = check(Arguments::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(args.kind, ArgumentsKind::ArgumentList(_)));
    format!("{:?}", args);
    pretty_check(&*args, "Arguments: ( a , b )", vec!["ArgumentList: a , b"]);
    concise_check(&*args, "Arguments: ( a , b )", vec!["Punctuator: (", "ArgumentList: a , b", "Punctuator: )"]);
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
    check_err(Arguments::parse(&mut newparser("(88"), Scanner::new(), false, false), "One of [‘,’, ‘)’] expected", 1, 4);
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

// ARGUMENT LIST
#[test]
fn argument_list_test_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(al.kind, ArgumentListKind::FallThru(_)));
    format!("{:?}", al);
    pretty_check(&*al, "ArgumentList: aba", vec!["AssignmentExpression: aba"]);
    concise_check(&*al, "IdentifierName: aba", vec![]);
}
#[test]
fn argument_list_test_dots_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("...aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(al.kind, ArgumentListKind::Dots(_)));
    format!("{:?}", al);
    pretty_check(&*al, "ArgumentList: ... aba", vec!["AssignmentExpression: aba"]);
    concise_check(&*al, "ArgumentList: ... aba", vec!["Punctuator: ...", "IdentifierName: aba"]);
}
#[test]
fn argument_list_test_al_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(al.kind, ArgumentListKind::ArgumentList(..)));
    format!("{:?}", al);
    pretty_check(&*al, "ArgumentList: ab , aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    concise_check(&*al, "ArgumentList: ab , aba", vec!["IdentifierName: ab", "Punctuator: ,", "IdentifierName: aba"]);
}
#[test]
fn argument_list_test_al_dots_ae() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,...aba"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(al.kind, ArgumentListKind::ArgumentListDots(..)));
    format!("{:?}", al);
    pretty_check(&*al, "ArgumentList: ab , ... aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    concise_check(&*al, "ArgumentList: ab , ... aba", vec!["IdentifierName: ab", "Punctuator: ,", "Punctuator: ...", "IdentifierName: aba"]);
}
#[test]
fn argument_list_test_nomatch() {
    check_err(ArgumentList::parse(&mut newparser("*"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 1);
}
#[test]
fn argument_list_test_dotsonly() {
    check_err(ArgumentList::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
}
#[test]
fn argument_list_test_dots_term() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,..."), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(al.kind, ArgumentListKind::FallThru(_)));
}
#[test]
fn argument_list_test_commas() {
    let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,,10"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(al.kind, ArgumentListKind::FallThru(_)));
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

// NEW EXPRESSION
#[test]
fn new_expression_test_me() {
    let (ne, scanner) = check(NewExpression::parse(&mut newparser("true"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(ne.kind, NewExpressionKind::MemberExpression(_)));
    format!("{:?}", ne);
    pretty_check(&*ne, "NewExpression: true", vec!["MemberExpression: true"]);
    concise_check(&*ne, "Keyword: true", vec![]);
    assert_eq!(ne.is_function_definition(), false);
    assert_eq!(ne.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn new_expression_test_new() {
    let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(ne.kind, NewExpressionKind::NewExpression(_)));
    format!("{:?}", ne);
    pretty_check(&*ne, "NewExpression: new bob", vec!["NewExpression: bob"]);
    concise_check(&*ne, "NewExpression: new bob", vec!["Keyword: new", "IdentifierName: bob"]);
    assert_eq!(ne.is_function_definition(), false);
    assert_eq!(ne.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn new_expression_test_new_me() {
    let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob()"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(ne.kind, NewExpressionKind::MemberExpression(_)));
    format!("{:?}", ne);
    pretty_check(&*ne, "NewExpression: new bob ( )", vec!["MemberExpression: new bob ( )"]);
    concise_check(&*ne, "MemberExpression: new bob ( )", vec!["Keyword: new", "IdentifierName: bob", "Arguments: ( )"]);
    assert_eq!(ne.is_function_definition(), false);
    assert_eq!(ne.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn new_expression_test_nomatch() {
    check_err(NewExpression::parse(&mut newparser("*"), Scanner::new(), false, false), "‘new’ or MemberExpression expected", 1, 1);
}
#[test]
fn new_expression_test_chopped() {
    check_err(NewExpression::parse(&mut newparser("new"), Scanner::new(), false, false), "‘.’ expected", 1, 4);
}
#[test]
fn new_expression_test_prettyerrors_1() {
    let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn new_expression_test_prettyerrors_2() {
    let (item, _) = NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn new_expression_test_conciseerrors_1() {
    let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn new_expression_test_conciseerrors_2() {
    let (item, _) = NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn new_expression_test_contains_01() {
    let (item, _) = NewExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn new_expression_test_contains_02() {
    let (item, _) = NewExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn new_expression_test_contains_03() {
    let (item, _) = NewExpression::parse(&mut newparser("new this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn new_expression_test_contains_04() {
    let (item, _) = NewExpression::parse(&mut newparser("new 0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// CALL MEMBER EXPRESSION
#[test]
fn call_member_expression_test_me_args() {
    let (cme, scanner) = check(CallMemberExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    format!("{:?}", cme);
    pretty_check(&*cme, "CallMemberExpression: a ( )", vec!["MemberExpression: a", "Arguments: ( )"]);
    concise_check(&*cme, "CallMemberExpression: a ( )", vec!["IdentifierName: a", "Arguments: ( )"]);
}
#[test]
fn call_member_expression_test_nomatch() {
    check_err(CallMemberExpression::parse(&mut newparser("++"), Scanner::new(), false, false), "MemberExpression expected", 1, 1);
}
#[test]
fn call_member_expression_test_incomplete() {
    check_err(CallMemberExpression::parse(&mut newparser("pop"), Scanner::new(), false, false), "‘(’ expected", 1, 4);
}
#[test]
fn call_member_expression_test_prettyerrors_1() {
    let (item, _) = CallMemberExpression::parse(&mut newparser("o(0)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_member_expression_test_conciseerrors_1() {
    let (item, _) = CallMemberExpression::parse(&mut newparser("o(0)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_member_expression_test_contains_01() {
    let (item, _) = CallMemberExpression::parse(&mut newparser("this.fcn()"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_member_expression_test_contains_02() {
    let (item, _) = CallMemberExpression::parse(&mut newparser("Math.sin(this.angle)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_member_expression_test_contains_03() {
    let (item, _) = CallMemberExpression::parse(&mut newparser("Math.sin(angle)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// SUPER CALL
#[test]
fn super_call_test_args() {
    let (sc, scanner) = check(SuperCall::parse(&mut newparser("super()"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(sc.arguments.kind, ArgumentsKind::Empty));
    format!("{:?}", sc);
    pretty_check(&*sc, "SuperCall: super ( )", vec!["Arguments: ( )"]);
    concise_check(&*sc, "SuperCall: super ( )", vec!["Keyword: super", "Arguments: ( )"]);
}
#[test]
fn super_call_test_nomatch() {
    check_err(SuperCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘super’ expected", 1, 1);
}
#[test]
fn super_call_test_incomplete() {
    check_err(SuperCall::parse(&mut newparser("super"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
}
#[test]
fn super_call_test_prettyerrors_1() {
    let (item, _) = SuperCall::parse(&mut newparser("super(0)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn super_call_test_conciseerrors_1() {
    let (item, _) = SuperCall::parse(&mut newparser("super(0)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn super_call_test_contains_01() {
    let (item, _) = SuperCall::parse(&mut newparser("super(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn super_call_test_contains_02() {
    let (item, _) = SuperCall::parse(&mut newparser("super(zyz)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}

// IMPORT CALL
#[test]
fn import_call_test_ae() {
    let (ic, scanner) = check(ImportCall::parse(&mut newparser("import(bob)"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    format!("{:?}", ic);
    pretty_check(&*ic, "ImportCall: import ( bob )", vec!["AssignmentExpression: bob"]);
    concise_check(&*ic, "ImportCall: import ( bob )", vec!["Keyword: import", "Punctuator: (", "IdentifierName: bob", "Punctuator: )"]);
}
#[test]
fn import_call_test_nomatch() {
    check_err(ImportCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘import’ expected", 1, 1);
}
#[test]
fn import_call_test_incomplete() {
    check_err(ImportCall::parse(&mut newparser("import"), Scanner::new(), false, false), "‘(’ expected", 1, 7);
}
#[test]
fn import_call_test_incomplete2() {
    check_err(ImportCall::parse(&mut newparser("import("), Scanner::new(), false, false), "AssignmentExpression expected", 1, 8);
}
#[test]
fn import_call_test_incomplete3() {
    check_err(ImportCall::parse(&mut newparser("import(bob"), Scanner::new(), false, false), "‘)’ expected", 1, 11);
}
#[test]
fn import_call_test_prettyerrors_1() {
    let (item, _) = ImportCall::parse(&mut newparser("import(blue)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn import_call_test_conciseerrors_1() {
    let (item, _) = ImportCall::parse(&mut newparser("import(blue)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn import_call_test_contains_01() {
    let (item, _) = ImportCall::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn import_call_test_contains_02() {
    let (item, _) = ImportCall::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}

// CALL EXPRESSION
#[test]
fn call_expression_test_me_args() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: a ( )", vec!["CallMemberExpression: a ( )"]);
    concise_check(&*ce, "CallMemberExpression: a ( )", vec!["IdentifierName: a", "Arguments: ( )"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn call_expression_test_super() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("super()"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(ce.kind, CallExpressionKind::SuperCall(_)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: super ( )", vec!["SuperCall: super ( )"]);
    concise_check(&*ce, "SuperCall: super ( )", vec!["Keyword: super", "Arguments: ( )"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn call_expression_test_import() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("import(pop)"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(ce.kind, CallExpressionKind::ImportCall(_)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: import ( pop )", vec!["ImportCall: import ( pop )"]);
    concise_check(&*ce, "ImportCall: import ( pop )", vec!["Keyword: import", "Punctuator: (", "IdentifierName: pop", "Punctuator: )"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn call_expression_test_ce_args() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(20)"), Scanner::new(), false, false));
    chk_scan(&scanner, 23);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(..)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )", vec!["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"]);
    concise_check(&*ce, "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )", vec!["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn call_expression_test_ce_args2() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(++)"), Scanner::new(), false, false));
    chk_scan(&scanner, 19);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(..)));
    format!("{:?}", ce);
}
#[test]
fn call_expression_test_ce_exp() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[snap]"), Scanner::new(), false, false));
    chk_scan(&scanner, 15);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionExpression(..)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: blue ( pop ) [ snap ]", vec!["CallExpression: blue ( pop )", "Expression: snap"]);
    concise_check(&*ce, "CallExpression: blue ( pop ) [ snap ]", vec!["CallMemberExpression: blue ( pop )", "Punctuator: [", "IdentifierName: snap", "Punctuator: ]"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn call_expression_test_ce_ident() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop).snap"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionIdentifierName(..)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: blue ( pop ) . snap", vec!["CallExpression: blue ( pop )"]);
    concise_check(&*ce, "CallExpression: blue ( pop ) . snap", vec!["CallMemberExpression: blue ( pop )", "Punctuator: .", "IdentifierName: snap"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn call_expression_test_ce_pid() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop).#snap"), Scanner::new(), false, false));
    chk_scan(&scanner, 15);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionPrivateId(..)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: blue ( pop ) . #snap", vec!["CallExpression: blue ( pop )"]);
    concise_check(&*ce, "CallExpression: blue ( pop ) . #snap", vec!["CallMemberExpression: blue ( pop )", "Punctuator: .", "PrivateIdentifier: #snap"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn call_expression_test_ce_template() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)`snap`"), Scanner::new(), false, false));
    chk_scan(&scanner, 15);
    assert!(matches!(ce.kind, CallExpressionKind::CallExpressionTemplateLiteral(..)));
    format!("{:?}", ce);
    pretty_check(&*ce, "CallExpression: blue ( pop ) `snap`", vec!["CallExpression: blue ( pop )", "TemplateLiteral: `snap`"]);
    concise_check(&*ce, "CallExpression: blue ( pop ) `snap`", vec!["CallMemberExpression: blue ( pop )", "NoSubTemplate: `snap`"]);
    assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn call_expression_test_nomatch() {
    check_err(CallExpression::parse(&mut newparser(""), Scanner::new(), false, false), "CallExpression expected", 1, 1);
}
#[test]
fn call_expression_test_incomplete_01() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)["), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
}
#[test]
fn call_expression_test_incomplete_02() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[99"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
}
#[test]
fn call_expression_test_incomplete_03() {
    let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)."), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
}
#[test]
fn call_expression_test_prettyerrors_1() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_2() {
    let (item, _) = CallExpression::parse(&mut newparser("super(b)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_3() {
    let (item, _) = CallExpression::parse(&mut newparser("import(b)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_4() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)(c)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_5() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)[c]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_6() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b).c"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_7() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)`c`"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_prettyerrors_8() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b).#c"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_1() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_2() {
    let (item, _) = CallExpression::parse(&mut newparser("super(b)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_3() {
    let (item, _) = CallExpression::parse(&mut newparser("import(b)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_4() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)(c)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_5() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)[c]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_6() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b).c"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_7() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)`c`"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_conciseerrors_8() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b).#c"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn call_expression_test_contains_01() {
    let (item, _) = CallExpression::parse(&mut newparser("a(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_02() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_03() {
    let (item, _) = CallExpression::parse(&mut newparser("super(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn call_expression_test_contains_04() {
    let (item, _) = CallExpression::parse(&mut newparser("super(a)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn call_expression_test_contains_05() {
    let (item, _) = CallExpression::parse(&mut newparser("import(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_06() {
    let (item, _) = CallExpression::parse(&mut newparser("import(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_07() {
    let (item, _) = CallExpression::parse(&mut newparser("a(b)(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_08() {
    let (item, _) = CallExpression::parse(&mut newparser("a(this)(b)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_09() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)(b)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_10() {
    let (item, _) = CallExpression::parse(&mut newparser("a(this)[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_11() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_12() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_13() {
    let (item, _) = CallExpression::parse(&mut newparser("a(this).b"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_14() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0).b"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_15() {
    let (item, _) = CallExpression::parse(&mut newparser("a(this)`hi`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_16() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)`${this}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn call_expression_test_contains_17() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0)`${1}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn call_expression_test_contains_18() {
    let (item, _) = CallExpression::parse(&mut newparser("a(0).#b"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// OPTIONAL EXPRESSION
#[test]
fn optional_expression_test_01() {
    let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*lhs, OptionalExpression::Member(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalExpression: a ?. b", vec!["MemberExpression: a", "OptionalChain: ?. b"]);
    concise_check(&*lhs, "OptionalExpression: a ?. b", vec!["IdentifierName: a", "OptionalChain: ?. b"]);
}
#[test]
fn optional_expression_test_02() {
    let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*lhs, OptionalExpression::Call(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalExpression: a ( ) ?. b", vec!["CallExpression: a ( )", "OptionalChain: ?. b"]);
    concise_check(&*lhs, "OptionalExpression: a ( ) ?. b", vec!["CallMemberExpression: a ( )", "OptionalChain: ?. b"]);
}
#[test]
fn optional_expression_test_03() {
    let (lhs, scanner) = check(OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(*lhs, OptionalExpression::Opt(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalExpression: a ?. b ?. c", vec!["OptionalExpression: a ?. b", "OptionalChain: ?. c"]);
    concise_check(&*lhs, "OptionalExpression: a ?. b ?. c", vec!["OptionalExpression: a ?. b", "OptionalChain: ?. c"]);
}
#[test]
fn optional_expression_test_04() {
    check_err(OptionalExpression::parse(&mut newparser(""), Scanner::new(), false, false), "OptionalExpression expected", 1, 1);
}
#[test]
fn optional_expression_test_05() {
    check_err(OptionalExpression::parse(&mut newparser("10"), Scanner::new(), false, false), "‘?.’ expected", 1, 3);
}
#[test]
fn optional_expression_test_06() {
    check_err(OptionalExpression::parse(&mut newparser("u()"), Scanner::new(), false, false), "‘?.’ expected", 1, 4);
}
#[test]
fn optional_expression_test_prettyerrors_1() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_expression_test_prettyerrors_2() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_expression_test_prettyerrors_3() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_expression_test_conciseerrors_1() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_expression_test_conciseerrors_2() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_expression_test_conciseerrors_3() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.b?.c"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_expression_test_contains_01() {
    let (item, _) = OptionalExpression::parse(&mut newparser("this?.(a)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_02() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_03() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a?.(b)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_expression_test_contains_04() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(this)?.(b)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_05() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(0)?.(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_06() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(0)?.(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_expression_test_contains_07() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(this)?.(0)?.(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_08() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(0)?.(0)?.(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_expression_test_contains_09() {
    let (item, _) = OptionalExpression::parse(&mut newparser("a(0)?.(0)?.(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// OPTIONAL CHAIN
#[test]
fn optional_chain_test_01() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*lhs, OptionalChain::Args(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. ( )", vec!["Arguments: ( )"]);
    concise_check(&*lhs, "OptionalChain: ?. ( )", vec!["Punctuator: ?.", "Arguments: ( )"]);
}
#[test]
fn optional_chain_test_02() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.[1 in a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(*lhs, OptionalChain::Exp(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. [ 1 in a ]", vec!["Expression: 1 in a"]);
    concise_check(&*lhs, "OptionalChain: ?. [ 1 in a ]", vec!["Punctuator: ?.", "Punctuator: [", "RelationalExpression: 1 in a", "Punctuator: ]"]);
}
#[test]
fn optional_chain_test_03() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*lhs, OptionalChain::Ident(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a", vec!["IdentifierName: a"]);
    concise_check(&*lhs, "OptionalChain: ?. a", vec!["Punctuator: ?.", "IdentifierName: a"]);
}
#[test]
fn optional_chain_test_04() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(*lhs, OptionalChain::Template(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. `a`", vec!["TemplateLiteral: `a`"]);
    concise_check(&*lhs, "OptionalChain: ?. `a`", vec!["Punctuator: ?.", "NoSubTemplate: `a`"]);
}
#[test]
fn optional_chain_test_05() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(*lhs, OptionalChain::PlusArgs(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a ( )", vec!["OptionalChain: ?. a", "Arguments: ( )"]);
    concise_check(&*lhs, "OptionalChain: ?. a ( )", vec!["OptionalChain: ?. a", "Arguments: ( )"]);
}
#[test]
fn optional_chain_test_06() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a[0 in b]"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    assert!(matches!(*lhs, OptionalChain::PlusExp(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a [ 0 in b ]", vec!["OptionalChain: ?. a", "Expression: 0 in b"]);
    concise_check(&*lhs, "OptionalChain: ?. a [ 0 in b ]", vec!["OptionalChain: ?. a", "Punctuator: [", "RelationalExpression: 0 in b", "Punctuator: ]"]);
}
#[test]
fn optional_chain_test_07() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(*lhs, OptionalChain::PlusIdent(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a . b", vec!["OptionalChain: ?. a", "IdentifierName: b"]);
    concise_check(&*lhs, "OptionalChain: ?. a . b", vec!["OptionalChain: ?. a", "Punctuator: .", "IdentifierName: b"]);
}
#[test]
fn optional_chain_test_08() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*lhs, OptionalChain::PlusTemplate(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a `b`", vec!["OptionalChain: ?. a", "TemplateLiteral: `b`"]);
    concise_check(&*lhs, "OptionalChain: ?. a `b`", vec!["OptionalChain: ?. a", "NoSubTemplate: `b`"]);
}
#[test]
fn optional_chain_test_09() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*lhs, OptionalChain::PrivateId(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. #a", vec!["PrivateIdentifier: #a"]);
    concise_check(&*lhs, "OptionalChain: ?. #a", vec!["Punctuator: ?.", "PrivateIdentifier: #a"]);
}
#[test]
fn optional_chain_test_10() {
    let (lhs, scanner) = check(OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*lhs, OptionalChain::PlusPrivateId(..)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "OptionalChain: ?. a . #b", vec!["OptionalChain: ?. a", "PrivateIdentifier: #b"]);
    concise_check(&*lhs, "OptionalChain: ?. a . #b", vec!["OptionalChain: ?. a", "Punctuator: .", "PrivateIdentifier: #b"]);
}
#[test]
fn optional_chain_test_err_1() {
    check_err(OptionalChain::parse(&mut newparser(""), Scanner::new(), false, false), "‘?.’ expected", 1, 1);
}
#[test]
fn optional_chain_test_err_2() {
    check_err(OptionalChain::parse(&mut newparser("?."), Scanner::new(), false, false), "‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)", 1, 3);
}
#[test]
fn optional_chain_test_err_3() {
    let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a."), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*oc, OptionalChain::Ident(..)));
}
#[test]
fn optional_chain_test_err_4() {
    let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a["), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*oc, OptionalChain::Ident(..)));
}
#[test]
fn optional_chain_test_err_5() {
    let (oc, scanner) = check(OptionalChain::parse(&mut newparser("?.a[0"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*oc, OptionalChain::Ident(..)));
}
#[test]
fn optional_chain_test_err_6() {
    check_err(OptionalChain::parse(&mut newparser("?.["), Scanner::new(), false, false), "Expression expected", 1, 4);
}
#[test]
fn optional_chain_test_err_7() {
    check_err(OptionalChain::parse(&mut newparser("?.[0"), Scanner::new(), false, false), "‘]’ expected", 1, 5);
}
#[test]
fn optional_chain_test_prettyerrors_1() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_2() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_3() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_4() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_5() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_6() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a[0]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_7() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_8() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_9() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_prettyerrors_10() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_1() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_2() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_3() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_4() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.`a`"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_5() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_6() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a[0]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_7() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a.b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_8() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a`b`"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_9() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.#a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_conciseerrors_10() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.a.#b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn optional_chain_test_contains_01() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_02() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_03() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_04() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_05() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_06() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.`${this}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_07() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.`${0}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_08() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this)(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_09() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_10() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_11() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this)[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_12() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_13() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_14() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this).a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_15() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0).this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_16() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this)`a`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_17() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)`${this}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_18() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0)`a`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_19() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.#this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn optional_chain_test_contains_20() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(this).#a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn optional_chain_test_contains_21() {
    let (item, _) = OptionalChain::parse(&mut newparser("?.(0).#this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}


// LEFT-HAND-SIDE EXPRESSION
#[test]
fn left_hand_side_expression_test_01() {
    let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*lhs, LeftHandSideExpression::New(_)));
    format!("{:?}", lhs);
    pretty_check(&*lhs, "LeftHandSideExpression: a", vec!["NewExpression: a"]);
    concise_check(&*lhs, "IdentifierName: a", vec![]);
    assert_eq!(lhs.is_function_definition(), false);
    assert_eq!(lhs.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn left_hand_side_expression_test_02() {
    let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*lhs, LeftHandSideExpression::Call(_)));
    pretty_check(&*lhs, "LeftHandSideExpression: a ( )", vec!["CallExpression: a ( )"]);
    concise_check(&*lhs, "CallMemberExpression: a ( )", vec!["IdentifierName: a", "Arguments: ( )"]);
    assert_eq!(lhs.is_function_definition(), false);
    assert_eq!(lhs.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn left_hand_side_expression_test_03() {
    let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*lhs, LeftHandSideExpression::Optional(_)));
    pretty_check(&*lhs, "LeftHandSideExpression: a ( ) ?. b", vec!["OptionalExpression: a ( ) ?. b"]);
    concise_check(&*lhs, "OptionalExpression: a ( ) ?. b", vec!["CallMemberExpression: a ( )", "OptionalChain: ?. b"]);
    assert_eq!(lhs.is_function_definition(), false);
    assert_eq!(lhs.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn left_hand_side_expression_test_prettyerrors_1() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_prettyerrors_2() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_prettyerrors_3() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_conciseerrors_1() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_conciseerrors_2() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_conciseerrors_3() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a()?.b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn left_hand_side_expression_test_contains_01() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn left_hand_side_expression_test_contains_02() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn left_hand_side_expression_test_contains_03() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn left_hand_side_expression_test_contains_04() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn left_hand_side_expression_test_contains_05() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("this?.n"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn left_hand_side_expression_test_contains_06() {
    let (item, _) = LeftHandSideExpression::parse(&mut newparser("a?.n"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

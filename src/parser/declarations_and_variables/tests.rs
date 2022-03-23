use super::testhelp::{check, check_err, chk_scan, newparser, set, strictparser, IMPLEMENTS_NOT_ALLOWED, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

const MISSING_INITIALIZER: &str = "Missing initializer in const declaration";

// LEXICAL DECLARATION
#[test]
fn lexical_declaration_test_01() {
    let (node, scanner) = check(LexicalDeclaration::parse(&mut newparser("let a;"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "LexicalDeclaration: let a ;", vec!["LetOrConst: let", "BindingList: a"]);
    concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn lexical_declaration_test_02() {
    let (node, scanner) = check(LexicalDeclaration::parse(&mut newparser("const a=0;"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "LexicalDeclaration: const a = 0 ;", vec!["LetOrConst: const", "BindingList: a = 0"]);
    concise_check(&*node, "LexicalDeclaration: const a = 0 ;", vec!["Keyword: const", "LexicalBinding: a = 0", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn lexical_declaration_test_cache_01() {
    let mut parser = newparser("let i=0;");
    let (node, scanner) = check(LexicalDeclaration::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(LexicalDeclaration::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn lexical_declaration_test_asi_01() {
    let (node, scanner) = check(LexicalDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "LexicalDeclaration: let a ;", vec!["LetOrConst: let", "BindingList: a"]);
    concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn lexical_declaration_test_err_01() {
    check_err(LexicalDeclaration::parse(&mut newparser(""), Scanner::new(), true, false, false), "one of [‘let’, ‘const’] expected", 1, 1);
}
#[test]
fn lexical_declaration_test_err_02() {
    check_err(LexicalDeclaration::parse(&mut newparser("let"), Scanner::new(), true, false, false), "LexicalBinding expected", 1, 4);
}
#[test]
fn lexical_declaration_test_err_03() {
    check_err(LexicalDeclaration::parse(&mut newparser("let a for"), Scanner::new(), true, false, false), "‘;’ expected", 1, 6);
}
#[test]
fn lexical_declaration_test_prettyerrors_1() {
    let (item, _) = LexicalDeclaration::parse(&mut newparser("let a;"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn lexical_declaration_test_conciseerrors_1() {
    let (item, _) = LexicalDeclaration::parse(&mut newparser("let a;"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn lexical_declaration_test_bound_names_01() {
    let (item, _) = LexicalDeclaration::parse(&mut newparser("let a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn lexical_declaration_test_contains_01() {
    let (item, _) = LexicalDeclaration::parse(&mut newparser("let a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("let a=item.#valid;" => true; "valid")]
#[test_case("let a=item.#invalid;" => false; "invalid")]
fn lexical_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LexicalDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod lexical_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("let a;" => false; "kwd let")]
    #[test_case("const a=0;" => true; "kwd const")]
    fn is_constant_declaration(src: &str) -> bool {
        LexicalDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_constant_declaration()
    }

    const LET_NOT_LEGAL: &str = "‘let’ is not a valid binding identifier";
    const DUPLICATE_LEX_A: &str = "Duplicate binding identifiers: ‘a’";
    const DUPLICATE_LEX_ABC: &str = "Duplicate binding identifiers: ‘a’, ‘b’, ‘c’";

    #[test_case("let let=0;", false => set(&[LET_NOT_LEGAL]); "let let")]
    #[test_case("let a=1,b=2,c=3,a=6;", true => set(&[DUPLICATE_LEX_A]); "duplicate names")]
    #[test_case("let a=1,a=2,b=3,b=4,c=5,c=6;", true => set(&[DUPLICATE_LEX_ABC]); "many duplicates")]
    #[test_case("let package;", true => set(&[PACKAGE_NOT_ALLOWED]); "sub-productions")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        LexicalDeclaration::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// LET OR CONST
#[test]
fn let_or_const_test_01() {
    let item = LetOrConst::Const;
    assert!(format!("{}", item) == "const");
    pretty_check(&item, "LetOrConst: const", vec![]);
    concise_check(&item, "Keyword: const", vec![]);
    pretty_error_validate(&item);
    concise_error_validate(&item);
}
#[test]
fn let_or_const_test_02() {
    let item = LetOrConst::Let;
    assert!(format!("{}", item) == "let");
    pretty_check(&item, "LetOrConst: let", vec![]);
    concise_check(&item, "Keyword: let", vec![]);
    pretty_error_validate(&item);
    concise_error_validate(&item);
}

mod let_or_const {
    use super::*;
    use test_case::test_case;

    #[test]
    fn contains() {
        let item = LetOrConst::Let;
        assert_eq!(item.contains(ParseNodeKind::Literal), false);
    }

    #[test_case(LetOrConst::Let => false; "kwd let")]
    #[test_case(LetOrConst::Const => true; "kwd const")]
    fn is_constant_declaration(which: LetOrConst) -> bool {
        which.is_constant_declaration()
    }
}

// BINDING LIST
#[test]
fn binding_list_test_01() {
    let (node, scanner) = check(BindingList::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingList::Item(..)));
    pretty_check(&*node, "BindingList: a", vec!["LexicalBinding: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_list_test_02() {
    let (node, scanner) = check(BindingList::parse(&mut newparser("a,b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingList::List(..)));
    pretty_check(&*node, "BindingList: a , b", vec!["BindingList: a", "LexicalBinding: b"]);
    concise_check(&*node, "BindingList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_list_test_err_01() {
    check_err(BindingList::parse(&mut newparser(""), Scanner::new(), true, false, false), "LexicalBinding expected", 1, 1);
}
#[test]
fn binding_list_test_bound_names_01() {
    let (item, _) = BindingList::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_list_test_bound_names_02() {
    let (item, _) = BindingList::parse(&mut newparser("a,b"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn binding_list_test_contains_01() {
    let (item, _) = BindingList::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_list_test_contains_02() {
    let (item, _) = BindingList::parse(&mut newparser("a,b"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "single valid")]
#[test_case("a=item.#valid, b" => true; "multi first valid")]
#[test_case("a, b=item.#valid" => true; "multi second valid")]
#[test_case("a=item.#invalid" => false; "single invalid")]
#[test_case("a=item.#invalid, b" => false; "multi first invalid")]
#[test_case("a, b=item.#invalid" => false; "multi second invalid")]
fn binding_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true, true => set(&[PACKAGE_NOT_ALLOWED, MISSING_INITIALIZER]); "LexicalBinding")]
    #[test_case("package,implements", true, false => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingList , LexicalBinding")]
    fn early_errors(src: &str, strict: bool, is_constant_declaration: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingList::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, is_constant_declaration);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// LEXICAL BINDING
#[test]
fn lexical_binding_test_01() {
    let (node, scanner) = check(LexicalBinding::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, LexicalBinding::Identifier(_, None)));
    pretty_check(&*node, "LexicalBinding: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn lexical_binding_test_02() {
    let (node, scanner) = check(LexicalBinding::parse(&mut newparser("a=0"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, LexicalBinding::Identifier(_, Some(_))));
    pretty_check(&*node, "LexicalBinding: a = 0", vec!["BindingIdentifier: a", "Initializer: = 0"]);
    concise_check(&*node, "LexicalBinding: a = 0", vec!["IdentifierName: a", "Initializer: = 0"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn lexical_binding_test_03() {
    let (node, scanner) = check(LexicalBinding::parse(&mut newparser("{a}=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, LexicalBinding::Pattern(..)));
    pretty_check(&*node, "LexicalBinding: { a } = b", vec!["BindingPattern: { a }", "Initializer: = b"]);
    concise_check(&*node, "LexicalBinding: { a } = b", vec!["ObjectBindingPattern: { a }", "Initializer: = b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn lexical_binding_test_err_01() {
    check_err(LexicalBinding::parse(&mut newparser(""), Scanner::new(), true, false, false), "LexicalBinding expected", 1, 1);
}
#[test]
fn lexical_binding_test_err_02() {
    check_err(LexicalBinding::parse(&mut newparser("{a}"), Scanner::new(), true, false, false), "‘=’ expected", 1, 4);
}
#[test]
fn lexical_binding_test_bound_names_01() {
    let (item, _) = LexicalBinding::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn lexical_binding_test_bound_names_02() {
    let (item, _) = LexicalBinding::parse(&mut newparser("{a}={b}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn lexical_binding_test_contains_01() {
    let (item, _) = LexicalBinding::parse(&mut newparser("a=1"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn lexical_binding_test_contains_02() {
    let (item, _) = LexicalBinding::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn lexical_binding_test_contains_03() {
    let (item, _) = LexicalBinding::parse(&mut newparser("[a=0]=[z]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn lexical_binding_test_contains_04() {
    let (item, _) = LexicalBinding::parse(&mut newparser("[a]=[0]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn lexical_binding_test_contains_05() {
    let (item, _) = LexicalBinding::parse(&mut newparser("[a]=[b]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a" => true; "Identifer only")]
#[test_case("a=item.#valid" => true; "Initializer valid")]
#[test_case("[a=item.#valid]=[0]" => true; "Pattern valid")]
#[test_case("[a]=[item.#valid]" => true; "pattern init valid")]
#[test_case("a=item.#invalid" => false; "Initializer invalid")]
#[test_case("[a=item.#invalid]=[0]" => false; "Pattern invalid")]
#[test_case("[a]=[item.#invalid]" => false; "pattern init invalid")]
fn lexical_binding_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LexicalBinding::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod lexical_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("a=0", true, true => set(&[]); "valid constant decl")]
    #[test_case("a=0", true, false => set(&[]); "valid mutable decl, with initializer")]
    #[test_case("a", true, false => set(&[]); "valid mutable decl, without initializer")]
    #[test_case("a", true, true => set(&[MISSING_INITIALIZER]); "invalid constant decl")]
    #[test_case("package", true, false => set(&[PACKAGE_NOT_ALLOWED]); "BindingIdentifier")]
    #[test_case("package=implements", true, true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingIdentifier Initializer")]
    #[test_case("[package]=implements", true, false => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingPattern Initializer")]
    fn early_errors(src: &str, strict: bool, is_constant_declaration: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        LexicalBinding::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, is_constant_declaration);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// VARIABLE STATMENT
#[test]
fn variable_statement_test_01() {
    let (node, scanner) = check(VariableStatement::parse(&mut newparser("var a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "VariableStatement: var a ;", vec!["VariableDeclarationList: a"]);
    concise_check(&*node, "VariableStatement: var a ;", vec!["Keyword: var", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_statement_test_asi_01() {
    let (node, scanner) = check(VariableStatement::parse(&mut newparser("var a"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "VariableStatement: var a ;", vec!["VariableDeclarationList: a"]);
    concise_check(&*node, "VariableStatement: var a ;", vec!["Keyword: var", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_statement_test_err_01() {
    check_err(VariableStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘var’ expected", 1, 1);
}
#[test]
fn variable_statement_test_err_02() {
    check_err(VariableStatement::parse(&mut newparser("var"), Scanner::new(), false, false), "VariableDeclaration expected", 1, 4);
}
#[test]
fn variable_statement_test_err_03() {
    check_err(VariableStatement::parse(&mut newparser("var a 4"), Scanner::new(), false, false), "‘;’ expected", 1, 6);
}
#[test]
fn variable_statement_test_var_declared_names_01() {
    let (item, _) = VariableStatement::parse(&mut newparser("var a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn variable_statement_test_contains_01() {
    let (item, _) = VariableStatement::parse(&mut newparser("var a=0;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_statement_test_contains_02() {
    let (item, _) = VariableStatement::parse(&mut newparser("var a=b;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("var a=item.#valid;" => true; "valid")]
#[test_case("var a=item.#invalid;" => false; "invalid")]
fn variable_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = VariableStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod variable_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("var package;", true => set(&[PACKAGE_NOT_ALLOWED]); "var VariableDeclarationList ;")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        VariableStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// VARIABLE DECLARATION LIST
#[test]
fn variable_declaration_list_test_01() {
    let (node, scanner) = check(VariableDeclarationList::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, VariableDeclarationList::Item(..)));
    pretty_check(&*node, "VariableDeclarationList: a", vec!["VariableDeclaration: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_declaration_list_test_02() {
    let (node, scanner) = check(VariableDeclarationList::parse(&mut newparser("a,b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, VariableDeclarationList::List(..)));
    pretty_check(&*node, "VariableDeclarationList: a , b", vec!["VariableDeclarationList: a", "VariableDeclaration: b"]);
    concise_check(&*node, "VariableDeclarationList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_declaration_list_test_cache_01() {
    let mut parser = newparser("a,b,c,d,10");
    let (node, scanner) = check(VariableDeclarationList::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(VariableDeclarationList::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn variable_declaration_list_test_err_01() {
    check_err(VariableDeclarationList::parse(&mut newparser(""), Scanner::new(), true, false, false), "VariableDeclaration expected", 1, 1);
}
#[test]
fn variable_declaration_list_test_bound_names_01() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn variable_declaration_list_test_bound_names_02() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=0,b=x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn variable_declaration_list_test_contains_01() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_list_test_contains_02() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn variable_declaration_list_test_contains_03() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=0,b=x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_list_test_contains_04() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=x,b=0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_list_test_contains_05() {
    let (item, _) = VariableDeclarationList::parse(&mut newparser("a=x,b=y"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "Item valid")]
#[test_case("a=item.#valid, b" => true; "multi first valid")]
#[test_case("a,b=item.#valid" => true; "multi last valid")]
#[test_case("a=item.#invalid" => false; "Item invalid")]
#[test_case("a=item.#invalid, b" => false; "multi first invalid")]
#[test_case("a,b=item.#invalid" => false; "multi last invalid")]
fn variable_declaration_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = VariableDeclarationList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod variable_declaration_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "VariableDeclaration")]
    #[test_case("package,implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "VariableDeclarationList , VariableDeclaration")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        VariableDeclarationList::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// VARIABLE DECLARATION
#[test]
fn variable_declaration_test_01() {
    let (node, scanner) = check(VariableDeclaration::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, VariableDeclaration::Identifier(_, None)));
    pretty_check(&*node, "VariableDeclaration: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_declaration_test_02() {
    let (node, scanner) = check(VariableDeclaration::parse(&mut newparser("a=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, VariableDeclaration::Identifier(_, Some(_))));
    pretty_check(&*node, "VariableDeclaration: a = b", vec!["BindingIdentifier: a", "Initializer: = b"]);
    concise_check(&*node, "VariableDeclaration: a = b", vec!["IdentifierName: a", "Initializer: = b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_declaration_test_03() {
    let (node, scanner) = check(VariableDeclaration::parse(&mut newparser("{a}=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, VariableDeclaration::Pattern(..)));
    pretty_check(&*node, "VariableDeclaration: { a } = b", vec!["BindingPattern: { a }", "Initializer: = b"]);
    concise_check(&*node, "VariableDeclaration: { a } = b", vec!["ObjectBindingPattern: { a }", "Initializer: = b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn variable_declaration_test_err_01() {
    check_err(VariableDeclaration::parse(&mut newparser(""), Scanner::new(), true, false, false), "VariableDeclaration expected", 1, 1);
}
#[test]
fn variable_declaration_test_err_02() {
    check_err(VariableDeclaration::parse(&mut newparser("{a}"), Scanner::new(), true, false, false), "‘=’ expected", 1, 4)
}
#[test]
fn variable_declaration_test_bound_names_01() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn variable_declaration_test_bound_names_02() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("{a}={a:1}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn variable_declaration_test_contains_01() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("a=0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_test_contains_02() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("a=x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn variable_declaration_test_contains_03() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn variable_declaration_test_contains_04() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("[a=0]=[x]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_test_contains_05() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("[a]=[0]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn variable_declaration_test_contains_06() {
    let (item, _) = VariableDeclaration::parse(&mut newparser("[a]=[x]"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a" => true; "ident only")]
#[test_case("a=item.#valid" => true; "izer valid")]
#[test_case("[a=item.#valid]=[b]" => true; "pattern valid")]
#[test_case("[a]=[item.#valid]" => true; "pattern izer valid")]
#[test_case("a=item.#invalid" => false; "izer invalid")]
#[test_case("[a=item.#invalid]=[b]" => false; "pattern invalid")]
#[test_case("[a]=[item.#invalid]" => false; "pattern izer invalid")]
fn variable_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = VariableDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod variable_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingIdentifier")]
    #[test_case("package=implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingIdentifier Initializer")]
    #[test_case("[package]=implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingPattern Initializer")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        VariableDeclaration::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING PATTERN
#[test]
fn binding_pattern_test_01() {
    let (node, scanner) = check(BindingPattern::parse(&mut newparser("{a}"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingPattern::Object(..)));
    pretty_check(&*node, "BindingPattern: { a }", vec!["ObjectBindingPattern: { a }"]);
    concise_check(&*node, "ObjectBindingPattern: { a }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_pattern_test_02() {
    let (node, scanner) = check(BindingPattern::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingPattern::Array(..)));
    pretty_check(&*node, "BindingPattern: [ a ]", vec!["ArrayBindingPattern: [ a ]"]);
    concise_check(&*node, "ArrayBindingPattern: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_pattern_test_cache_01() {
    let mut parser = newparser("{one, two, three}");
    let (node, scanner) = check(BindingPattern::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(BindingPattern::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn binding_pattern_test_err_01() {
    check_err(BindingPattern::parse(&mut newparser(""), Scanner::new(), false, false), "BindingPattern expected", 1, 1);
}
#[test]
fn binding_pattern_test_bound_names_01() {
    let (item, _) = BindingPattern::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_pattern_test_bound_names_02() {
    let (item, _) = BindingPattern::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_pattern_test_contains_01() {
    let (item, _) = BindingPattern::parse(&mut newparser("{a=0}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_pattern_test_contains_02() {
    let (item, _) = BindingPattern::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_pattern_test_contains_03() {
    let (item, _) = BindingPattern::parse(&mut newparser("[a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_pattern_test_contains_04() {
    let (item, _) = BindingPattern::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("{a=item.#valid}" => true; "OBP valid")]
#[test_case("[a=item.#valid]" => true; "ABP valid")]
#[test_case("{a=item.#invalid}" => false; "OBP invalid")]
#[test_case("[a=item.#invalid]" => false; "ABP invalid")]
fn binding_pattern_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{package}", true => set(&[PACKAGE_NOT_ALLOWED]); "ObjectBindingPattern")]
    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "ArrayBindingPattern")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingPattern::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// OBJECT BINDING PATTERN
#[test]
fn object_binding_pattern_test_01() {
    let (node, scanner) = check(ObjectBindingPattern::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*node, ObjectBindingPattern::Empty));
    pretty_check(&*node, "ObjectBindingPattern: { }", vec![]);
    concise_check(&*node, "ObjectBindingPattern: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn object_binding_pattern_test_02() {
    let (node, scanner) = check(ObjectBindingPattern::parse(&mut newparser("{...a}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*node, ObjectBindingPattern::RestOnly(..)));
    pretty_check(&*node, "ObjectBindingPattern: { ... a }", vec!["BindingRestProperty: ... a"]);
    concise_check(&*node, "ObjectBindingPattern: { ... a }", vec!["Punctuator: {", "BindingRestProperty: ... a", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn object_binding_pattern_test_03() {
    let (node, scanner) = check(ObjectBindingPattern::parse(&mut newparser("{a}"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, ObjectBindingPattern::ListOnly(..)));
    pretty_check(&*node, "ObjectBindingPattern: { a }", vec!["BindingPropertyList: a"]);
    concise_check(&*node, "ObjectBindingPattern: { a }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn object_binding_pattern_test_04() {
    let (node, scanner) = check(ObjectBindingPattern::parse(&mut newparser("{a,...b}"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*node, ObjectBindingPattern::ListRest(_, Some(_))));
    pretty_check(&*node, "ObjectBindingPattern: { a , ... b }", vec!["BindingPropertyList: a", "BindingRestProperty: ... b"]);
    concise_check(&*node, "ObjectBindingPattern: { a , ... b }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: ,", "BindingRestProperty: ... b", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn object_binding_pattern_test_05() {
    let (node, scanner) = check(ObjectBindingPattern::parse(&mut newparser("{a,}"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, ObjectBindingPattern::ListRest(_, None)));
    pretty_check(&*node, "ObjectBindingPattern: { a , }", vec!["BindingPropertyList: a"]);
    concise_check(&*node, "ObjectBindingPattern: { a , }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: ,", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn object_binding_pattern_test_err_01() {
    check_err(ObjectBindingPattern::parse(&mut newparser(""), Scanner::new(), false, false), "ObjectBindingPattern expected", 1, 1);
}
#[test]
fn object_binding_pattern_test_err_02() {
    check_err(ObjectBindingPattern::parse(&mut newparser("{"), Scanner::new(), false, false), "‘}’ expected", 1, 2);
}
#[test]
fn object_binding_pattern_test_err_03() {
    check_err(ObjectBindingPattern::parse(&mut newparser("{"), Scanner::new(), false, false), "‘}’ expected", 1, 2);
}
#[test]
fn object_binding_pattern_test_err_04() {
    check_err(ObjectBindingPattern::parse(&mut newparser("{a"), Scanner::new(), false, false), "‘}’ expected", 1, 3);
}
#[test]
fn object_binding_pattern_test_err_05() {
    check_err(ObjectBindingPattern::parse(&mut newparser("{...a"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
}
#[test]
fn object_binding_pattern_test_err_06() {
    check_err(ObjectBindingPattern::parse(&mut newparser("{b,...a"), Scanner::new(), false, false), "‘}’ expected", 1, 8);
}
#[test]
fn object_binding_pattern_test_bound_names_01() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &[] as &[JSString]);
}
#[test]
fn object_binding_pattern_test_bound_names_02() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{...a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn object_binding_pattern_test_bound_names_03() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn object_binding_pattern_test_bound_names_04() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a,}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn object_binding_pattern_test_bound_names_05() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a,...b}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn object_binding_pattern_test_contains_01() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn object_binding_pattern_test_contains_02() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{...a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn object_binding_pattern_test_contains_03() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a=0}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn object_binding_pattern_test_contains_04() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn object_binding_pattern_test_contains_05() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a=0,}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn object_binding_pattern_test_contains_06() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a,}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn object_binding_pattern_test_contains_07() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a=0,...b}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn object_binding_pattern_test_contains_08() {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser("{a,...b}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("{}" => true; "Empty")]
#[test_case("{...a}" => true; "BindingRestProperty")]
#[test_case("{a=item.#valid}" => true; "BindingList valid")]
#[test_case("{a=item.#valid,}" => true; "BindingListComma valid")]
#[test_case("{a=item.#valid,...b}" => true; "BindingListRest valid")]
#[test_case("{a=item.#invalid}" => false; "BindingList invalid")]
#[test_case("{a=item.#invalid,}" => false; "BindingListComma invalid")]
#[test_case("{a=item.#invalid,...b}" => false; "BindingListRest invalid")]
fn object_binding_pattern_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ObjectBindingPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod object_binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true => set(&[]); "Empty")]
    #[test_case("{...package}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ BindingRestProperty }")]
    #[test_case("{package}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ BindingPropertyList }")]
    #[test_case("{package,}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ BindingPropertyList , } (trailing comma)")]
    #[test_case("{package,...implements}", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "{ BindingPropertyList , BindingRestProperty }")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ObjectBindingPattern::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// ARRAY BINDING PATTERN
#[test]
fn array_binding_pattern_test_01() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[]"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*node, ArrayBindingPattern::RestOnly(None, None)));
    pretty_check(&*node, "ArrayBindingPattern: [ ]", vec![]);
    concise_check(&*node, "ArrayBindingPattern: [ ]", vec!["Punctuator: [", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_02() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, ArrayBindingPattern::RestOnly(Some(_), None)));
    pretty_check(&*node, "ArrayBindingPattern: [ , ]", vec!["Elisions: ,"]);
    concise_check(&*node, "ArrayBindingPattern: [ , ]", vec!["Punctuator: [", "Elisions: ,", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_03() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[...a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*node, ArrayBindingPattern::RestOnly(None, Some(_))));
    pretty_check(&*node, "ArrayBindingPattern: [ ... a ]", vec!["BindingRestElement: ... a"]);
    concise_check(&*node, "ArrayBindingPattern: [ ... a ]", vec!["Punctuator: [", "BindingRestElement: ... a", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_04() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[,...a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*node, ArrayBindingPattern::RestOnly(Some(_), Some(_))));
    pretty_check(&*node, "ArrayBindingPattern: [ , ... a ]", vec!["Elisions: ,", "BindingRestElement: ... a"]);
    concise_check(&*node, "ArrayBindingPattern: [ , ... a ]", vec!["Punctuator: [", "Elisions: ,", "BindingRestElement: ... a", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_05() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, ArrayBindingPattern::ListOnly(..)));
    pretty_check(&*node, "ArrayBindingPattern: [ a ]", vec!["BindingElementList: a"]);
    concise_check(&*node, "ArrayBindingPattern: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_06() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[a,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, ArrayBindingPattern::ListRest(_, None, None)));
    pretty_check(&*node, "ArrayBindingPattern: [ a , ]", vec!["BindingElementList: a"]);
    concise_check(&*node, "ArrayBindingPattern: [ a , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_07() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[a,,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, ArrayBindingPattern::ListRest(_, Some(_), None)));
    pretty_check(&*node, "ArrayBindingPattern: [ a , , ]", vec!["BindingElementList: a", "Elisions: ,"]);
    concise_check(&*node, "ArrayBindingPattern: [ a , , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_08() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[a,...b]"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*node, ArrayBindingPattern::ListRest(_, None, Some(_))));
    pretty_check(&*node, "ArrayBindingPattern: [ a , ... b ]", vec!["BindingElementList: a", "BindingRestElement: ... b"]);
    concise_check(&*node, "ArrayBindingPattern: [ a , ... b ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "BindingRestElement: ... b", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_09() {
    let (node, scanner) = check(ArrayBindingPattern::parse(&mut newparser("[a,,...b]"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(&*node, ArrayBindingPattern::ListRest(_, Some(_), Some(_))));
    pretty_check(&*node, "ArrayBindingPattern: [ a , , ... b ]", vec!["BindingElementList: a", "Elisions: ,", "BindingRestElement: ... b"]);
    concise_check(&*node, "ArrayBindingPattern: [ a , , ... b ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "BindingRestElement: ... b", "Punctuator: ]"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn array_binding_pattern_test_err_01() {
    check_err(ArrayBindingPattern::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
}
#[test]
fn array_binding_pattern_test_err_02() {
    check_err(ArrayBindingPattern::parse(&mut newparser("["), Scanner::new(), false, false), "BindingElement expected", 1, 2);
}
#[test]
fn array_binding_pattern_test_err_03() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[a"), Scanner::new(), false, false), "one of [‘]’, ‘,’] expected", 1, 3);
}
#[test]
fn array_binding_pattern_test_err_04() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[,"), Scanner::new(), false, false), "BindingElement expected", 1, 3);
}
#[test]
fn array_binding_pattern_test_err_05() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[,...h"), Scanner::new(), false, false), "‘]’ expected", 1, 7);
}
#[test]
fn array_binding_pattern_test_err_06() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[,,,,,,,,,,,,,..."), Scanner::new(), false, false), "‘[’, ‘{’, or an identifier expected", 1, 18);
}
#[test]
fn array_binding_pattern_test_err_07() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[abc,def,,,,..."), Scanner::new(), false, false), "‘[’, ‘{’, or an identifier expected", 1, 16);
}
#[test]
fn array_binding_pattern_test_err_08() {
    check_err(ArrayBindingPattern::parse(&mut newparser("[abc,def,,,,...ddd"), Scanner::new(), false, false), "‘]’ expected", 1, 19);
}
#[test]
fn array_binding_pattern_test_bound_names_01() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[...a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn array_binding_pattern_test_bound_names_02() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &[] as &[JSString]);
}
#[test]
fn array_binding_pattern_test_bound_names_03() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn array_binding_pattern_test_bound_names_04() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,...b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn array_binding_pattern_test_bound_names_05() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn array_binding_pattern_test_contains_01() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_02() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_03() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[...{a=10}]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_04() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[...a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_05() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[,...{a=0}]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_06() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[,...a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_07() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_08() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_09() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a=0,,...b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_10() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,,...{b=0}]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_11() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,,...b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_12() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a=0,...b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_13() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,...{b=0}]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_14() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,...b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_15() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a=0,,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_16() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn array_binding_pattern_test_contains_17() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a=0,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn array_binding_pattern_test_contains_18() {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser("[a,]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("[]" => true; "Empty")]
#[test_case("[,,]" => true; "Commas")]
#[test_case("[...[a=item.#valid]]" => true; "RestOnly valid")]
#[test_case("[,,...[a=item.#valid]]" => true; "Commas + Rest valid")]
#[test_case("[a=item.#valid]" => true; "Element valid")]
#[test_case("[a=item.#valid,]" => true; "ElementComma valid")]
#[test_case("[a=item.#valid,,]" => true; "ElementElision valid")]
#[test_case("[a=item.#valid,...b]" => true; "ElementRest valid")]
#[test_case("[a=item.#valid,,...b]" => true; "ElementERest valid")]
#[test_case("[a,...[b=item.#valid]]" => true; "ElementRest rest valid")]
#[test_case("[a,,...[b=item.#valid]]" => true; "ElementERest rest valid")]
#[test_case("[...[a=item.#invalid]]" => false; "RestOnly invalid")]
#[test_case("[,,...[a=item.#invalid]]" => false; "Commas + Rest invalid")]
#[test_case("[a=item.#invalid]" => false; "Element invalid")]
#[test_case("[a=item.#invalid,]" => false; "ElementComma invalid")]
#[test_case("[a=item.#invalid,,]" => false; "ElementElision invalid")]
#[test_case("[a=item.#invalid,...b]" => false; "ElementRest invalid")]
#[test_case("[a=item.#invalid,,...b]" => false; "ElementERest invalid")]
#[test_case("[a,...[b=item.#invalid]]" => false; "ElementRest rest invalid")]
#[test_case("[a,,...[b=item.#invalid]]" => false; "ElementERest rest invalid")]
fn array_binding_pattern_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrayBindingPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod array_binding_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("[]", true => set(&[]); "Empty")]
    #[test_case("[,]", true => set(&[]); "[ Elision ]")]
    #[test_case("[...package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ BindingRestElement ]")]
    #[test_case("[,...package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ Elision BindingRestElement ]")]
    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ BindingElementList ]")]
    #[test_case("[package,]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ BindingElementList , ] (trailing comma)")]
    #[test_case("[package,,]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ BindingElementList , Elision ]")]
    #[test_case("[package,...implements]", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "[ BindingElementList , BindingRestElement ]")]
    #[test_case("[package,,...implements]", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "[ BindingElementList , Elision BindingRestElement ]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ArrayBindingPattern::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING REST PROPERTY
#[test]
fn binding_rest_property_test_01() {
    let (node, scanner) = check(BindingRestProperty::parse(&mut newparser("...b"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "BindingRestProperty: ... b", vec!["BindingIdentifier: b"]);
    concise_check(&*node, "BindingRestProperty: ... b", vec!["Punctuator: ...", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_rest_property_test_cache_01() {
    let mut parser = newparser("...xyz");
    let (node, scanner) = check(BindingRestProperty::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(BindingRestProperty::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn binding_rest_property_test_err_01() {
    check_err(BindingRestProperty::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
}
#[test]
fn binding_rest_property_test_err_02() {
    check_err(BindingRestProperty::parse(&mut newparser("..."), Scanner::new(), false, false), "not an identifier", 1, 4);
}
#[test]
fn binding_rest_property_test_bound_names_01() {
    let (item, _) = BindingRestProperty::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_rest_property_test_contains_01() {
    let (item, _) = BindingRestProperty::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
mod binding_rest_property {
    use super::*;
    use test_case::test_case;

    #[test_case("...package", true => set(&[PACKAGE_NOT_ALLOWED]); "... BindingIdentifier")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingRestProperty::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING PROPERTY LIST
#[test]
fn binding_property_list_test_01() {
    let (node, scanner) = check(BindingPropertyList::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingPropertyList::Item(..)));
    pretty_check(&*node, "BindingPropertyList: a", vec!["BindingProperty: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_list_test_02() {
    let (node, scanner) = check(BindingPropertyList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingPropertyList::List(..)));
    pretty_check(&*node, "BindingPropertyList: a , b", vec!["BindingPropertyList: a", "BindingProperty: b"]);
    concise_check(&*node, "BindingPropertyList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_list_test_03() {
    let (node, scanner) = check(BindingPropertyList::parse(&mut newparser("a,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingPropertyList::Item(..)));
    pretty_check(&*node, "BindingPropertyList: a", vec!["BindingProperty: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_list_test_err_01() {
    check_err(BindingPropertyList::parse(&mut newparser(""), Scanner::new(), false, false), "BindingProperty expected", 1, 1);
}
#[test]
fn binding_property_list_test_bound_names_01() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_property_list_test_bound_names_02() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a1, a2, a3"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a1", "a2", "a3"]);
}
#[test]
fn binding_property_list_test_contains_01() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_list_test_contains_02() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_property_list_test_contains_03() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a=0,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_list_test_contains_04() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a,b=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_list_test_contains_05() {
    let (item, _) = BindingPropertyList::parse(&mut newparser("a,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "Single valid")]
#[test_case("a=item.#valid,b" => true; "Multi first valid")]
#[test_case("a,b=item.#valid" => true; "Multi second valid")]
#[test_case("a=item.#invalid" => false; "Single invalid")]
#[test_case("a=item.#invalid,b" => false; "Multi first invalid")]
#[test_case("a,b=item.#invalid" => false; "Multi second invalid")]
fn binding_property_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingPropertyList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_property_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingProperty")]
    #[test_case("package,implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingPropertyList , BindingProperty")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingPropertyList::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING ELEMENT LIST
#[test]
fn binding_element_list_test_01() {
    let (node, scanner) = check(BindingElementList::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingElementList::Item(..)));
    pretty_check(&*node, "BindingElementList: a", vec!["BindingElisionElement: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_list_test_02() {
    let (node, scanner) = check(BindingElementList::parse(&mut newparser("a,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingElementList::Item(..)));
    pretty_check(&*node, "BindingElementList: a", vec!["BindingElisionElement: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_list_test_03() {
    let (node, scanner) = check(BindingElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingElementList::List(..)));
    pretty_check(&*node, "BindingElementList: a , b", vec!["BindingElementList: a", "BindingElisionElement: b"]);
    concise_check(&*node, "BindingElementList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_list_test_err_01() {
    check_err(BindingElementList::parse(&mut newparser(""), Scanner::new(), false, false), "BindingElement expected", 1, 1);
}
#[test]
fn binding_element_list_test_bound_names_01() {
    let (item, _) = BindingElementList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_element_list_test_bound_names_02() {
    let (item, _) = BindingElementList::parse(&mut newparser("a,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn binding_element_list_test_contains_01() {
    let (item, _) = BindingElementList::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_list_test_contains_02() {
    let (item, _) = BindingElementList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_element_list_test_contains_03() {
    let (item, _) = BindingElementList::parse(&mut newparser("a=0,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_list_test_contains_04() {
    let (item, _) = BindingElementList::parse(&mut newparser("a,b=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_list_test_contains_05() {
    let (item, _) = BindingElementList::parse(&mut newparser("a,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "Single valid")]
#[test_case("a=item.#valid,b" => true; "Multi first valid")]
#[test_case("a,b=item.#valid" => true; "Multi second valid")]
#[test_case("a=item.#invalid" => false; "Single invalid")]
#[test_case("a=item.#invalid,b" => false; "Multi first invalid")]
#[test_case("a,b=item.#invalid" => false; "Multi second invalid")]
fn binding_element_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingElementList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingElisionElement")]
    #[test_case("package,implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingElementList , BindingElisionElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingElementList::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING ELISION ELEMENT
#[test]
fn binding_elision_element_test_01() {
    let (node, scanner) = check(BindingElisionElement::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingElisionElement::Element(None, _)));
    pretty_check(&*node, "BindingElisionElement: a", vec!["BindingElement: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_elision_element_test_02() {
    let (node, scanner) = check(BindingElisionElement::parse(&mut newparser(",a"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*node, BindingElisionElement::Element(Some(_), _)));
    pretty_check(&*node, "BindingElisionElement: , a", vec!["Elisions: ,", "BindingElement: a"]);
    concise_check(&*node, "BindingElisionElement: , a", vec!["Elisions: ,", "IdentifierName: a"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_elision_element_test_err_01() {
    check_err(BindingElisionElement::parse(&mut newparser(""), Scanner::new(), false, false), "BindingElement expected", 1, 1);
}
#[test]
fn binding_elision_element_test_bound_names_01() {
    let (item, _) = BindingElisionElement::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_elision_element_test_contains_01() {
    let (item, _) = BindingElisionElement::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_elision_element_test_contains_02() {
    let (item, _) = BindingElisionElement::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_elision_element_test_contains_03() {
    let (item, _) = BindingElisionElement::parse(&mut newparser(",a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_elision_element_test_contains_04() {
    let (item, _) = BindingElisionElement::parse(&mut newparser(",a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "No commas valid")]
#[test_case(",a=item.#valid" => true; "Commas valid")]
#[test_case("a=item.#invalid" => false; "No commas invalid")]
#[test_case(",a=item.#invalid" => false; "Commas invalid")]
fn binding_elision_element_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingElisionElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_elision_element {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingElement")]
    #[test_case(",package", true => set(&[PACKAGE_NOT_ALLOWED]); "Elision BindingElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingElisionElement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING PROPERTY
#[test]
fn binding_property_test_01() {
    let (node, scanner) = check(BindingProperty::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingProperty::Single(..)));
    pretty_check(&*node, "BindingProperty: a", vec!["SingleNameBinding: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_test_02() {
    let (node, scanner) = check(BindingProperty::parse(&mut newparser("a:b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingProperty::Property(..)));
    pretty_check(&*node, "BindingProperty: a : b", vec!["PropertyName: a", "BindingElement: b"]);
    concise_check(&*node, "BindingProperty: a : b", vec!["IdentifierName: a", "Punctuator: :", "IdentifierName: b"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_test_03() {
    let (node, scanner) = check(BindingProperty::parse(&mut newparser("a:"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingProperty::Single(..)));
    pretty_check(&*node, "BindingProperty: a", vec!["SingleNameBinding: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_property_test_err_01() {
    check_err(BindingProperty::parse(&mut newparser(""), Scanner::new(), false, false), "BindingProperty expected", 1, 1);
}
#[test]
fn binding_property_test_bound_names_01() {
    let (item, _) = BindingProperty::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_property_test_bound_names_02() {
    let (item, _) = BindingProperty::parse(&mut newparser("a:b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["b"]);
}
#[test]
fn binding_property_test_contains_01() {
    let (item, _) = BindingProperty::parse(&mut newparser("a=1"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_test_contains_02() {
    let (item, _) = BindingProperty::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_property_test_contains_03() {
    let (item, _) = BindingProperty::parse(&mut newparser("[0]:a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_test_contains_04() {
    let (item, _) = BindingProperty::parse(&mut newparser("a:b=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_property_test_contains_05() {
    let (item, _) = BindingProperty::parse(&mut newparser("a:b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "Single valid")]
#[test_case("a:b=item.#valid" => true; "Element valid")]
#[test_case("[item.#valid]:b" => true; "Name valid")]
#[test_case("a=item.#invalid" => false; "Single invalid")]
#[test_case("a:b=item.#invalid" => false; "Element invalid")]
#[test_case("[item.#invalid]:b" => false; "Name invalid")]
fn binding_property_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_property {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "SingleNameBinding")]
    #[test_case("[package]:implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "PropertyName : BindingElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingProperty::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING ELEMENT
#[test]
fn binding_element_test_01() {
    let (node, scanner) = check(BindingElement::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, BindingElement::Single(..)));
    pretty_check(&*node, "BindingElement: a", vec!["SingleNameBinding: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_test_02() {
    let (node, scanner) = check(BindingElement::parse(&mut newparser("{a}"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, BindingElement::Pattern(_, None)));
    pretty_check(&*node, "BindingElement: { a }", vec!["BindingPattern: { a }"]);
    concise_check(&*node, "ObjectBindingPattern: { a }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_test_03() {
    let (node, scanner) = check(BindingElement::parse(&mut newparser("{a}=n"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, BindingElement::Pattern(_, Some(_))));
    pretty_check(&*node, "BindingElement: { a } = n", vec!["BindingPattern: { a }", "Initializer: = n"]);
    concise_check(&*node, "BindingElement: { a } = n", vec!["ObjectBindingPattern: { a }", "Initializer: = n"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_element_test_cache_01() {
    let mut parser = newparser("{xyz}=test_object");
    let (node, scanner) = check(BindingElement::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(BindingElement::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn binding_element_test_err_01() {
    check_err(BindingElement::parse(&mut newparser(""), Scanner::new(), false, false), "BindingElement expected", 1, 1);
}
#[test]
fn binding_element_test_bound_names_01() {
    let (item, _) = BindingElement::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_element_test_bound_names_02() {
    let (item, _) = BindingElement::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_element_test_contains_01() {
    let (item, _) = BindingElement::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_test_contains_02() {
    let (item, _) = BindingElement::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_element_test_contains_03() {
    let (item, _) = BindingElement::parse(&mut newparser("{a:c=0}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_test_contains_04() {
    let (item, _) = BindingElement::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_element_test_contains_05() {
    let (item, _) = BindingElement::parse(&mut newparser("{a:c=0}={a:x}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_test_contains_06() {
    let (item, _) = BindingElement::parse(&mut newparser("{a}={a:0}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_element_test_contains_07() {
    let (item, _) = BindingElement::parse(&mut newparser("{a}={a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=item.#valid" => true; "Single valid")]
#[test_case("[a=item.#valid]" => true; "PatternOnly valid")]
#[test_case("[a=item.#valid]=[0]" => true; "Pattern valid")]
#[test_case("[a]=[item.#valid]" => true; "Izer valid")]
#[test_case("a=item.#invalid" => false; "Single invalid")]
#[test_case("[a=item.#invalid]" => false; "PatternOnly invalid")]
#[test_case("[a=item.#invalid]=[0]" => false; "Pattern invalid")]
#[test_case("[a]=[item.#invalid]" => false; "Izer invalid")]
fn binding_element_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
#[test_case("a" => true; "Single simple")]
#[test_case("a=3" => false; "Single complex")]
#[test_case("[a]" => false; "Pattern only")]
#[test_case("[a]=[0]" => false; "Pattern izer")]
fn binding_element_test_is_simple_parameter_list(src: &str) -> bool {
    let (item, _) = BindingElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.is_simple_parameter_list()
}
mod binding_element {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "SingleNameBinding")]
    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingPattern")]
    #[test_case("[package]=implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingPattern Initializer")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingElement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// SINGLE NAME BINDING
#[test]
fn single_name_binding_test_01() {
    let (node, scanner) = check(SingleNameBinding::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, SingleNameBinding::Id(_, None)));
    pretty_check(&*node, "SingleNameBinding: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn single_name_binding_test_02() {
    let (node, scanner) = check(SingleNameBinding::parse(&mut newparser("a=0"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, SingleNameBinding::Id(_, Some(_))));
    pretty_check(&*node, "SingleNameBinding: a = 0", vec!["BindingIdentifier: a", "Initializer: = 0"]);
    concise_check(&*node, "SingleNameBinding: a = 0", vec!["IdentifierName: a", "Initializer: = 0"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn single_name_binding_test_cache_01() {
    let mut parser = newparser("xyz=green");
    let (node, scanner) = check(SingleNameBinding::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(SingleNameBinding::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn single_name_binding_test_err_01() {
    check_err(SingleNameBinding::parse(&mut newparser(""), Scanner::new(), false, false), "not an identifier", 1, 1);
}
#[test]
fn single_name_binding_test_bound_names_01() {
    let (item, _) = SingleNameBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn single_name_binding_test_bound_names_02() {
    let (item, _) = SingleNameBinding::parse(&mut newparser("a=x"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn single_name_binding_test_contains_01() {
    let (item, _) = SingleNameBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn single_name_binding_test_contains_02() {
    let (item, _) = SingleNameBinding::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn single_name_binding_test_contains_03() {
    let (item, _) = SingleNameBinding::parse(&mut newparser("a=x"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a" => true; "Name Only")]
#[test_case("a=item.#valid" => true; "Izer valid")]
#[test_case("a=item.#invalid" => false; "Izer invalid")]
fn single_name_binding_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = SingleNameBinding::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
#[test_case("a" => true; "Name only")]
#[test_case("a=0" => false; "Has Initializer")]
fn single_name_binding_test_is_simple_parameter_list(src: &str) -> bool {
    let (item, _) = SingleNameBinding::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.is_simple_parameter_list()
}
mod single_name_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingIdentifier")]
    #[test_case("package=implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "BindingIdentifier Initializer")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        SingleNameBinding::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// BINDING REST ELEMENT
#[test]
fn binding_rest_element_test_01() {
    let (node, scanner) = check(BindingRestElement::parse(&mut newparser("...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, BindingRestElement::Identifier(..)));
    pretty_check(&*node, "BindingRestElement: ... a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "BindingRestElement: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_rest_element_test_02() {
    let (node, scanner) = check(BindingRestElement::parse(&mut newparser("...{a}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*node, BindingRestElement::Pattern(..)));
    pretty_check(&*node, "BindingRestElement: ... { a }", vec!["BindingPattern: { a }"]);
    concise_check(&*node, "BindingRestElement: ... { a }", vec!["Punctuator: ...", "ObjectBindingPattern: { a }"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn binding_rest_element_test_cache_01() {
    let mut parser = newparser("...{xyz}");
    let (node, scanner) = check(BindingRestElement::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(BindingRestElement::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn binding_rest_element_test_err_01() {
    check_err(BindingRestElement::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
}
#[test]
fn binding_rest_element_test_err_02() {
    check_err(BindingRestElement::parse(&mut newparser("..."), Scanner::new(), false, false), "‘[’, ‘{’, or an identifier expected", 1, 4);
}
#[test]
fn binding_rest_element_test_bound_names_01() {
    let (item, _) = BindingRestElement::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn binding_rest_element_test_bound_names_02() {
    let (item, _) = BindingRestElement::parse(&mut newparser("...[a,b]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a", "b"]);
}
#[test]
fn binding_rest_element_test_contains_01() {
    let (item, _) = BindingRestElement::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn binding_rest_element_test_contains_02() {
    let (item, _) = BindingRestElement::parse(&mut newparser("...[a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn binding_rest_element_test_contains_03() {
    let (item, _) = BindingRestElement::parse(&mut newparser("...[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("...a" => true; "Name only")]
#[test_case("...[a=item.#valid]" => true; "Pattern valid")]
#[test_case("...[a=item.#invalid]" => false; "Pattern invalid")]
fn binding_rest_element_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BindingRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod binding_rest_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...package", true => set(&[PACKAGE_NOT_ALLOWED]); "... BindingIdentifier")]
    #[test_case("...[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "... BindingPattern")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BindingRestElement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

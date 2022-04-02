use super::testhelp::{check, expected_scan, newparser, set, strictparser, sv, INTERFACE_NOT_ALLOWED, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_data, concise_error_validate, pretty_data, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;

mod assignment_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let node = AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), false, true, false).unwrap().0;
        assert_ne!(format!("{:?}", node), "");
    }

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentExpression: a", "ConditionalExpression: a"]),
        sv(&["IdentifierName: a"])
    )); "Fall-thru (identifier)")]
    #[test_case("yield a" => Ok((
        expected_scan(7),
        sv(&["AssignmentExpression: yield a", "YieldExpression: yield a"]),
        sv(&["YieldExpression: yield a", "Keyword: yield", "IdentifierName: a"])
    )); "YieldExpression")]
    #[test_case("a=>a" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a => a", "ArrowFunction: a => a"]),
        sv(&["ArrowFunction: a => a", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]),
    )); "ArrowFunction")]
    #[test_case("async a=>a" => Ok((
        expected_scan(10),
        sv(&["AssignmentExpression: async a => a", "AsyncArrowFunction: async a => a"]),
        sv(&["AsyncArrowFunction: async a => a", "Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"])
    )); "AsyncArrowFunction")]
    #[test_case("a=b" => Ok((
        expected_scan(3),
        sv(&["AssignmentExpression: a = b", "LeftHandSideExpression: a", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a = b", "IdentifierName: a", "Punctuator: =", "IdentifierName: b"])
    )); "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a *= b", "LeftHandSideExpression: a", "AssignmentOperator: *=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a *= b", "IdentifierName: a", "Punctuator: *=", "IdentifierName: b"])
    )); "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("a/=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a /= b", "LeftHandSideExpression: a", "AssignmentOperator: /=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a /= b", "IdentifierName: a", "Punctuator: /=", "IdentifierName: b"])
    )); "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("a%=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a %= b", "LeftHandSideExpression: a", "AssignmentOperator: %=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a %= b", "IdentifierName: a", "Punctuator: %=", "IdentifierName: b"])
    )); "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("a+=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a += b", "LeftHandSideExpression: a", "AssignmentOperator: +=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a += b", "IdentifierName: a", "Punctuator: +=", "IdentifierName: b"])
    )); "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("a-=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a -= b", "LeftHandSideExpression: a", "AssignmentOperator: -=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a -= b", "IdentifierName: a", "Punctuator: -=", "IdentifierName: b"])
    )); "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("a<<=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a <<= b", "LeftHandSideExpression: a", "AssignmentOperator: <<=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a <<= b", "IdentifierName: a", "Punctuator: <<=", "IdentifierName: b"])
    )); "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("a>>=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a >>= b", "LeftHandSideExpression: a", "AssignmentOperator: >>=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a >>= b", "IdentifierName: a", "Punctuator: >>=", "IdentifierName: b"])
    )); "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("a>>>=b" => Ok((
        expected_scan(6),
        sv(&["AssignmentExpression: a >>>= b", "LeftHandSideExpression: a", "AssignmentOperator: >>>=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a >>>= b", "IdentifierName: a", "Punctuator: >>>=", "IdentifierName: b"])
    )); "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("a&=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a &= b", "LeftHandSideExpression: a", "AssignmentOperator: &=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a &= b", "IdentifierName: a", "Punctuator: &=", "IdentifierName: b"])
    )); "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("a^=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a ^= b", "LeftHandSideExpression: a", "AssignmentOperator: ^=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a ^= b", "IdentifierName: a", "Punctuator: ^=", "IdentifierName: b"])
    )); "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("a|=b" => Ok((
        expected_scan(4),
        sv(&["AssignmentExpression: a |= b", "LeftHandSideExpression: a", "AssignmentOperator: |=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a |= b", "IdentifierName: a", "Punctuator: |=", "IdentifierName: b"])
    )); "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("a**=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a **= b", "LeftHandSideExpression: a", "AssignmentOperator: **=", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a **= b", "IdentifierName: a", "Punctuator: **=", "IdentifierName: b"])
    )); "LeftHandSideExpression **= AssignmentExpression (exponentiation)")]
    #[test_case("a&&=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a &&= b", "LeftHandSideExpression: a", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a &&= b", "IdentifierName: a", "Punctuator: &&=", "IdentifierName: b"])
    )); "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a ||= b", "LeftHandSideExpression: a", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a ||= b", "IdentifierName: a", "Punctuator: ||=", "IdentifierName: b"])
    )); "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b" => Ok((
        expected_scan(5),
        sv(&["AssignmentExpression: a ??= b", "LeftHandSideExpression: a", "AssignmentExpression: b"]),
        sv(&["AssignmentExpression: a ??= b", "IdentifierName: a", "Punctuator: ??=", "IdentifierName: b"])
    )); "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[a]=[1]" => Ok((
        expected_scan(7),
        sv(&["AssignmentExpression: [ a ] = [ 1 ]", "AssignmentPattern: [ a ]", "AssignmentExpression: [ 1 ]"]),
        sv(&["AssignmentExpression: [ a ] = [ 1 ]", "ArrayAssignmentPattern: [ a ]", "Punctuator: =", "ArrayLiteral: [ 1 ]"])
    )); "AssignmentPattern = AssignmentExpression")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), 1)); "empty")]
    #[test_case("[a+1]=[1]" => Err(ParseError::new(PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBracket]), 3)); "bad destructuring")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), false, true, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test_case("a" => false; "Fall-thru (identifier)")]
    #[test_case("function (){}" => true; "Fall-thru (function exp)")]
    #[test_case("yield a" => false; "YieldExpression")]
    #[test_case("a=>a" => true; "ArrowFunction")]
    #[test_case("async a=>a" => true; "AsyncArrowFunction")]
    #[test_case("a=b" => false; "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b" => false; "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("a/=b" => false; "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("a%=b" => false; "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("a+=b" => false; "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("a-=b" => false; "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("a<<=b" => false; "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("a>>=b" => false; "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("a>>>=b" => false; "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("a&=b" => false; "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("a^=b" => false; "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("a|=b" => false; "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("a&&=b" => false; "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b" => false; "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b" => false; "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[a]=[1]" => false; "AssignmentPattern = AssignmentExpression")]
    fn is_function_definition(src: &str) -> bool {
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_function_definition()
    }

    #[test_case("a" => ATTKind::Simple; "Fall-thru (identifier)")]
    #[test_case("function (){}" => ATTKind::Invalid; "Fall-thru (function exp)")]
    #[test_case("yield a" => ATTKind::Invalid; "YieldExpression")]
    #[test_case("a=>a" => ATTKind::Invalid; "ArrowFunction")]
    #[test_case("async a=>a" => ATTKind::Invalid; "AsyncArrowFunction")]
    #[test_case("a=b" => ATTKind::Invalid; "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b" => ATTKind::Invalid; "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("a/=b" => ATTKind::Invalid; "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("a%=b" => ATTKind::Invalid; "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("a+=b" => ATTKind::Invalid; "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("a-=b" => ATTKind::Invalid; "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("a<<=b" => ATTKind::Invalid; "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("a>>=b" => ATTKind::Invalid; "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("a>>>=b" => ATTKind::Invalid; "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("a&=b" => ATTKind::Invalid; "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("a^=b" => ATTKind::Invalid; "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("a|=b" => ATTKind::Invalid; "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("a**=b" => ATTKind::Invalid; "LeftHandSideExpression **= AssignmentExpression (exponentiation)")]
    #[test_case("a&&=b" => ATTKind::Invalid; "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b" => ATTKind::Invalid; "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b" => ATTKind::Invalid; "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[a]=[1]" => ATTKind::Invalid; "AssignmentPattern = AssignmentExpression")]
    fn assignment_target_type(src: &str) -> ATTKind {
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.assignment_target_type()
    }

    #[test]
    fn cache() {
        let mut parser = newparser("a+=b+c+d+e");
        let (node, scanner) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
        let (node2, scanner2) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn assignment_expression_test_prettyerrors_1() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_2() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_3() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_31() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_4() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_5() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_6() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_7() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_8() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_9() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_10() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_11() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_12() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_13() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_14() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_15() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_16() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_17() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_18() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_19() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_prettyerrors_20() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("[a]=[1]"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_1() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_2() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_3() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_31() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_4() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_5() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_6() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_7() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_8() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_9() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_10() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_11() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_12() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_13() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_14() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_15() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_16() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_17() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_18() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_19() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_conciseerrors_20() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("[a]=[1]"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn assignment_expression_test_contains_01() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_02() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_03() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("yield this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_04() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("yield 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_05() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("x => { this; }"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_06() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("x => { 0; }"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_07() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("async x => { this; }"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_08() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("async x => { 0; }"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_09() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this.a = 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_10() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a = this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_11() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a = 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_12() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this.a *= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_13() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a *= this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_14() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a *= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_15() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this.a &&= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_16() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a &&= this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_17() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a &&= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_18() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this.a ||= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_19() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a ||= this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_20() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a ||= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_21() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("this.a ??= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_22() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a ??= this"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_23() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("a ??= 0"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn assignment_expression_test_contains_24() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("[a]=[this]"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_25() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("[a=this]=[0]"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn assignment_expression_test_contains_26() {
        let (item, _) = AssignmentExpression::parse(&mut newparser("[a]=[0]"), Scanner::new(), true, true, true).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a=b" => None; "Not token")]
    fn assignment_expression_test_as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("item.#valid" => true; "Fallthru valid")]
    #[test_case("yield item.#valid" => true; "Yield valid")]
    #[test_case("x => x.#valid" => true; "ArrowFunction valid")]
    #[test_case("async x => x.#valid" => true; "AsyncArrowFunction valid")]
    #[test_case("a.#valid = 0" => true; "Assignment/Left valid")]
    #[test_case("a = item.#valid" => true; "Assignment/Right valid")]
    #[test_case("a.#valid += 0" => true; "AssignmentOp/Left valid")]
    #[test_case("a += item.#valid" => true; "AssignmentOp/Right valid")]
    #[test_case("a.#valid &&= 0" => true; "AssignmentLand/Left valid")]
    #[test_case("a &&= item.#valid" => true; "AssignmentLand/Right valid")]
    #[test_case("a.#valid ||= 0" => true; "AssignmentLor/Left valid")]
    #[test_case("a ||= item.#valid" => true; "AssignmentLor/Right valid")]
    #[test_case("a.#valid ??= 0" => true; "AssignmentCoal/Left valid")]
    #[test_case("a ??= item.#valid" => true; "AssignmentCoal/Right valid")]
    #[test_case("[a]=[item.#valid]" => true; "Destructuring/Right valid")]
    #[test_case("[a=item.#valid]=[0]" => true; "Destructuring/Left valid")]
    #[test_case("item.#invalid" => false; "Fallthru invalid")]
    #[test_case("yield item.#invalid" => false; "Yield invalid")]
    #[test_case("x => x.#invalid" => false; "ArrowFunction invalid")]
    #[test_case("async x => x.#invalid" => false; "AsyncArrowFunction invalid")]
    #[test_case("a.#invalid = 0" => false; "Assignment/Left invalid")]
    #[test_case("a = item.#invalid" => false; "Assignment/Right invalid")]
    #[test_case("a.#invalid += 0" => false; "AssignmentOp/Left invalid")]
    #[test_case("a += item.#invalid" => false; "AssignmentOp/Right invalid")]
    #[test_case("a.#invalid &&= 0" => false; "AssignmentLand/Left invalid")]
    #[test_case("a &&= item.#invalid" => false; "AssignmentLand/Right invalid")]
    #[test_case("a.#invalid ||= 0" => false; "AssignmentLor/Left invalid")]
    #[test_case("a ||= item.#invalid" => false; "AssignmentLor/Right invalid")]
    #[test_case("a.#invalid ??= 0" => false; "AssignmentCoal/Left invalid")]
    #[test_case("a ??= item.#invalid" => false; "AssignmentCoal/Right invalid")]
    #[test_case("[a]=[item.#invalid]" => false; "Destructuring/Right invalid")]
    #[test_case("[a=item.#invalid]=[0]" => false; "Destructuring/Left invalid")]
    fn assignment_expression_test_all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "Fall-thru (identifier)")]
    #[test_case("yield package", true => panics "not yet implemented"; "YieldExpression")]
    #[test_case("package=>interface", true => panics "not yet implemented"; "ArrowFunction")]
    #[test_case("async package=>interface", true => panics "not yet implemented"; "AsyncArrowFunction")]
    #[test_case("package=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("package*=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("package/=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("package%=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("package+=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("package-=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("package<<=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("package>>=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("package>>>=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("package&=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("package^=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("package|=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("package**=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression **= AssignmentExpression (exponentiation)")]
    #[test_case("package&&=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("package||=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("package??=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[package]=[interface]", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentPattern = AssignmentExpression")]
    #[test_case("(a=>a)=b", true => panics "not yet implemented"; "LHS must be simple (assignment)")]
    #[test_case("(a=>a)*=b", true => panics "not yet implemented"; "LHS must be simple (multiplication)")]
    #[test_case("(a=>a)/=b", true => panics "not yet implemented"; "LHS must be simple (division)")]
    #[test_case("(a=>a)%=b", true => panics "not yet implemented"; "LHS must be simple (modulo)")]
    #[test_case("(a=>a)+=b", true => panics "not yet implemented"; "LHS must be simple (add)")]
    #[test_case("(a=>a)-=b", true => panics "not yet implemented"; "LHS must be simple (subtract)")]
    #[test_case("(a=>a)<<=b", true => panics "not yet implemented"; "LHS must be simple (lsh)")]
    #[test_case("(a=>a)>>=b", true => panics "not yet implemented"; "LHS must be simple (rsh)")]
    #[test_case("(a=>a)>>>=b", true => panics "not yet implemented"; "LHS must be simple (ursh)")]
    #[test_case("(a=>a)&=b", true => panics "not yet implemented"; "LHS must be simple (bitwise and)")]
    #[test_case("(a=>a)^=b", true => panics "not yet implemented"; "LHS must be simple (xor)")]
    #[test_case("(a=>a)|=b", true => panics "not yet implemented"; "LHS must be simple (bitwise or)")]
    #[test_case("(a=>a)**=b", true => panics "not yet implemented"; "LHS must be simple (exponentiation)")]
    #[test_case("(a=>a)&&=b", true => panics "not yet implemented"; "LHS must be simple (logical and)")]
    #[test_case("(a=>a)||=b", true => panics "not yet implemented"; "LHS must be simple (logical or)")]
    #[test_case("(a=>a)??=b", true => panics "not yet implemented"; "LHS must be simple (coalesce)")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a=3" => true; "assignment op")]
    fn is_strictly_deletable(src: &str) -> bool {
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
    }
}

#[test]
fn assignment_operator_test_contains_01() {
    assert_eq!(AssignmentOperator::Multiply.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_02() {
    assert_eq!(AssignmentOperator::Divide.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_03() {
    assert_eq!(AssignmentOperator::Modulo.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_04() {
    assert_eq!(AssignmentOperator::Add.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_05() {
    assert_eq!(AssignmentOperator::Subtract.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_06() {
    assert_eq!(AssignmentOperator::LeftShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_07() {
    assert_eq!(AssignmentOperator::SignedRightShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_08() {
    assert_eq!(AssignmentOperator::UnsignedRightShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_09() {
    assert_eq!(AssignmentOperator::BitwiseAnd.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_10() {
    assert_eq!(AssignmentOperator::BitwiseXor.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_11() {
    assert_eq!(AssignmentOperator::BitwiseOr.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_12() {
    assert_eq!(AssignmentOperator::Exponentiate.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_debug() {
    assert_ne!(format!("{:?}", AssignmentOperator::BitwiseOr), "");
}

mod assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{}" => Ok((
        expected_scan(2),
        sv(&["AssignmentPattern: { }", "ObjectAssignmentPattern: { }"]),
        sv(&["ObjectAssignmentPattern: { }", "Punctuator: {", "Punctuator: }"])
    )); "ObjectAssignmentPattern")]
    #[test_case("[]" => Ok((
        expected_scan(2),
        sv(&["AssignmentPattern: [ ]", "ArrayAssignmentPattern: [ ]"]),
        sv(&["ArrayAssignmentPattern: [ ]", "Punctuator: [", "Punctuator: ]"])
    )); "ArrayAssignmentPattern")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentPattern), 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = AssignmentPattern::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("{}"; "ObjectAssignmentPattern")]
    #[test_case("[]"; "ArrayAssignmentPattern")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("{}"; "ObjectAssignmentPattern")]
    #[test_case("[]"; "ArrayAssignmentPattern")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("{[this]:a}" => true; "ObjectAssignmentPattern: present")]
    #[test_case("{[p]:a}" => false; "ObjectAssignmentPattern: not present")]
    #[test_case("[a=this]" => true; "ArrayAssignmentPattern: present")]
    #[test_case("[a=3]" => false; "ArrayAssignmentPattern: not present")]
    fn contains(src: &str) -> bool {
        let (node, _) = AssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("{a:b.#valid}" => true; "ObjectAssignmentPattern: valid")]
    #[test_case("[a=b.#valid]" => true; "ArrayAssignmentPattern: valid")]
    #[test_case("{a:b.#invalid}" => false; "ObjectAssignmentPattern: invalid")]
    #[test_case("[a=b.#invalid]" => false; "ArrayAssignmentPattern: invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("{package}", true => set(&[PACKAGE_NOT_ALLOWED]); "ObjectAssignmentPattern")]
    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "ArrayAssignmentPattern")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod object_assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("{}" => Ok((
        expected_scan(2),
        sv(&["ObjectAssignmentPattern: { }"]),
        sv(&["ObjectAssignmentPattern: { }", "Punctuator: {", "Punctuator: }"])
    )); "{ } (empty)")]
    #[test_case("{...a}" => Ok((
        expected_scan(6),
        sv(&["ObjectAssignmentPattern: { ... a }", "AssignmentRestProperty: ... a"]),
        sv(&["ObjectAssignmentPattern: { ... a }", "Punctuator: {", "AssignmentRestProperty: ... a", "Punctuator: }"]),
    )); "{ AssignmentRestProperty }")]
    #[test_case("{a}" => Ok((
        expected_scan(3),
        sv(&["ObjectAssignmentPattern: { a }", "AssignmentPropertyList: a"]),
        sv(&["ObjectAssignmentPattern: { a }", "Punctuator: {", "IdentifierName: a", "Punctuator: }"])
    )); "{ AssignmentPropertyList }")]
    #[test_case("{a,}" => Ok((
        expected_scan(4),
        sv(&["ObjectAssignmentPattern: { a , }", "AssignmentPropertyList: a"]),
        sv(&["ObjectAssignmentPattern: { a , }", "Punctuator: {", "IdentifierName: a", "Punctuator: ,", "Punctuator: }"])
    )); "{ AssignmentPropertyList , } (trailing comma)")]
    #[test_case("{a,...b}" => Ok((
        expected_scan(8),
        sv(&["ObjectAssignmentPattern: { a , ... b }", "AssignmentPropertyList: a", "AssignmentRestProperty: ... b"]),
        sv(&["ObjectAssignmentPattern: { a , ... b }", "Punctuator: {", "IdentifierName: a", "Punctuator: ,", "AssignmentRestProperty: ... b", "Punctuator: }"])
    )); "{ AssignmentPropertyList , AssignmentRestProperty }")]
    #[test_case("" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::LeftBrace), 1)); "empty")]
    #[test_case("{" => Err(ParseError::new(PECode::ObjectAssignmentPatternEndFailure, 2)); "open brace alone")]
    #[test_case("{a" => Err(ParseError::new(PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBrace]), 3)); "err after list")]
    #[test_case("{a," => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBrace), 4)); "err after list+comma")]
    #[test_case("{...a" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBrace), 6)); "err after rest")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = ObjectAssignmentPattern::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("{}"; "{ } (empty)")]
    #[test_case("{...a}"; "{ AssignmentRestProperty }")]
    #[test_case("{a}"; "{ AssignmentPropertyList }")]
    #[test_case("{a,}"; "{ AssignmentPropertyList , } (trailing comma)")]
    #[test_case("{a,...b}"; "{ AssignmentPropertyList , AssignmentRestProperty }")]
    fn pretty_errors(src: &str) {
        let (item, _) = ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }

    #[test_case("{}"; "{ } (empty)")]
    #[test_case("{...a}"; "{ AssignmentRestProperty }")]
    #[test_case("{a}"; "{ AssignmentPropertyList }")]
    #[test_case("{a,}"; "{ AssignmentPropertyList , } (trailing comma)")]
    #[test_case("{a,...b}"; "{ AssignmentPropertyList , AssignmentRestProperty }")]
    fn concise_errors(src: &str) {
        let (item, _) = ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("{}" => false; "{ } (empty): not present")]
    #[test_case("{...a}" => false; "{ AssignmentRestProperty }: not present")]
    #[test_case("{...this}" => true; "{ AssignmentRestProperty }: present")]
    #[test_case("{a}" => false; "{ AssignmentPropertyList }: not present")]
    #[test_case("{[this]:a}" => true; "{ AssignmentPropertyList }: present")]
    #[test_case("{a,}" => false; "{ AssignmentPropertyList , } (trailing comma): not present")]
    #[test_case("{[this]:a,}" => true; "{ AssignmentPropertyList , } (trailing comma): present")]
    #[test_case("{a,...b}" => false; "{ AssignmentPropertyList , AssignmentRestProperty }: not present")]
    #[test_case("{[this]:a,...b}" => true; "{ AssignmentPropertyList , AssignmentRestProperty }: present in List")]
    #[test_case("{a,...this}" => true; "{ AssignmentPropertyList , AssignmentRestProperty }: present in Rest")]
    fn contains(src: &str) -> bool {
        let (node, _) = ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("{}" => true; "{ } (empty)")]
    #[test_case("{...a.#valid}" => true; "{ AssignmentRestProperty }: valid")]
    #[test_case("{[a.#valid]:b}" => true; "{ AssignmentPropertyList }: valid")]
    #[test_case("{[a.#valid]:b,}" => true; "{ AssignmentPropertyList , } (trailing comma): valid")]
    #[test_case("{[a.#valid]:b,...c}" => true; "{ AssignmentPropertyList , AssignmentRestProperty }: valid List")]
    #[test_case("{[a]:b,...c.#valid}" => true; "{ AssignmentPropertyList , AssignmentRestProperty }: valid Rest")]
    #[test_case("{...a.#invalid}" => false; "{ AssignmentRestProperty }: invalid")]
    #[test_case("{[a.#invalid]:b}" => false; "{ AssignmentPropertyList }: invalid")]
    #[test_case("{[a.#invalid]:b,}" => false; "{ AssignmentPropertyList , } (trailing comma): invalid")]
    #[test_case("{[a.#invalid]:b,...c}" => false; "{ AssignmentPropertyList , AssignmentRestProperty }: invalid List")]
    #[test_case("{[a]:b,...c.#invalid}" => false; "{ AssignmentPropertyList , AssignmentRestProperty }: invalid Rest")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("{}", true => AHashSet::<String>::new(); "empty")]
    #[test_case("{...package}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentRestProperty }")]
    #[test_case("{package}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentPropertyList }")]
    #[test_case("{package,}", true => set(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentPropertyList , } (trailing comma)")]
    #[test_case("{package,...interface}", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "{ AssignmentPropertyList , AssignmentRestProperty }")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod array_assignment_pattern {
    use super::*;
    use test_case::test_case;

    #[test_case("[]" => Ok((
        expected_scan(2),
        sv(&["ArrayAssignmentPattern: [ ]"]),
        sv(&["ArrayAssignmentPattern: [ ]", "Punctuator: [", "Punctuator: ]"])
    )); "[ ] (nothing)")]
    #[test_case("[,]" => Ok((
        expected_scan(3),
        sv(&["ArrayAssignmentPattern: [ , ]", "Elisions: ,"]),
        sv(&["ArrayAssignmentPattern: [ , ]", "Punctuator: [", "Elisions: ,", "Punctuator: ]"])
    )); "[ Elision ]")]
    #[test_case("[...a]" => Ok((
        expected_scan(6),
        sv(&["ArrayAssignmentPattern: [ ... a ]", "AssignmentRestElement: ... a"]),
        sv(&["ArrayAssignmentPattern: [ ... a ]", "Punctuator: [", "AssignmentRestElement: ... a", "Punctuator: ]"])
    )); "[ AssignmentRestElement ]")]
    #[test_case("[,...a]" => Ok((
        expected_scan(7),
        sv(&["ArrayAssignmentPattern: [ , ... a ]", "Elisions: ,", "AssignmentRestElement: ... a"]),
        sv(&["ArrayAssignmentPattern: [ , ... a ]", "Punctuator: [", "Elisions: ,", "AssignmentRestElement: ... a", "Punctuator: ]"])
    )); "[ Elision AssignmentRestElement ]")]
    #[test_case("[a]" => Ok((
        expected_scan(3),
        sv(&["ArrayAssignmentPattern: [ a ]", "AssignmentElementList: a"]),
        sv(&["ArrayAssignmentPattern: [ a ]", "Punctuator: [", "IdentifierName: a", "Punctuator: ]"])
    )); "[ AssignmentElementList ]")]
    #[test_case("[a,]" => Ok((
        expected_scan(4),
        sv(&["ArrayAssignmentPattern: [ a , ]", "AssignmentElementList: a"]),
        sv(&["ArrayAssignmentPattern: [ a , ]", "Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Punctuator: ]"])
    )); "[ AssignmentElementList , ] (trailing comma)")]
    #[test_case("[a,,]" => Ok((
        expected_scan(5),
        sv(&["ArrayAssignmentPattern: [ a , , ]", "AssignmentElementList: a", "Elisions: ,"]),
        sv(&["ArrayAssignmentPattern: [ a , , ]", "Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "Punctuator: ]"])
    )); "[ AssignmentElementList , Elision ]")]
    #[test_case("[a,...b]" => Ok((
        expected_scan(8),
        sv(&["ArrayAssignmentPattern: [ a , ... b ]", "AssignmentElementList: a", "AssignmentRestElement: ... b"]),
        sv(&["ArrayAssignmentPattern: [ a , ... b ]", "Punctuator: [", "IdentifierName: a", "Punctuator: ,", "AssignmentRestElement: ... b", "Punctuator: ]"])
    )); "[ AssignmentELementList , AssignmentRestElement ]")]
    #[test_case("[a,,...b]" => Ok((
        expected_scan(9),
        sv(&["ArrayAssignmentPattern: [ a , , ... b ]", "AssignmentElementList: a", "Elisions: ,", "AssignmentRestElement: ... b"]),
        sv(&["ArrayAssignmentPattern: [ a , , ... b ]", "Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "AssignmentRestElement: ... b", "Punctuator: ]"])
    )); "[ AssignmentElementList , Elision AssignmentRestElement ]")]
    #[test_case("" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::LeftBracket), 1)); "empty")]
    #[test_case("[" => Err(ParseError::new(PECode::ArrayAssignmentPatternEndFailure, 2)); "open bracket alone")]
    #[test_case("[a" => Err(ParseError::new(PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBracket]), 3)); "err after list")]
    #[test_case("[a," => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBracket), 4)); "err after list+elision")]
    #[test_case("[...a" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBracket), 6)); "err after rest")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = ArrayAssignmentPattern::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("[]"; "[ ] (nothing)")]
    #[test_case("[,]"; "[ Elision ]")]
    #[test_case("[...a]"; "[ AssignmentRestElement ]")]
    #[test_case("[,...a]"; "[ Elision AssignmentRestElement ]")]
    #[test_case("[a]"; "[ AssignmentElementList ]")]
    #[test_case("[a,]"; "[ AssignmentElementList , ] (trailing comma)")]
    #[test_case("[a,,]"; "[ AssignmentElementList , Elision ]")]
    #[test_case("[a,...b]"; "[ AssignmentELementList , AssignmentRestElement ]")]
    #[test_case("[a,,...b]"; "[ AssignmentElementList , Elision AssignmentRestElement ]")]
    fn pretty_errors(src: &str) {
        let (item, _) = ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("[]"; "[ ] (nothing)")]
    #[test_case("[,]"; "[ Elision ]")]
    #[test_case("[...a]"; "[ AssignmentRestElement ]")]
    #[test_case("[,...a]"; "[ Elision AssignmentRestElement ]")]
    #[test_case("[a]"; "[ AssignmentElementList ]")]
    #[test_case("[a,]"; "[ AssignmentElementList , ] (trailing comma)")]
    #[test_case("[a,,]"; "[ AssignmentElementList , Elision ]")]
    #[test_case("[a,...b]"; "[ AssignmentELementList , AssignmentRestElement ]")]
    #[test_case("[a,,...b]"; "[ AssignmentElementList , Elision AssignmentRestElement ]")]
    fn concise_errors(src: &str) {
        let (item, _) = ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("[]" => false; "[ ] (nothing)")]
    #[test_case("[,]" => false; "[ Elision ]")]
    #[test_case("[...a]" => false; "[ AssignmentRestElement ]: not present")]
    #[test_case("[...this]" => true; "[ AssignmentRestElement ]: present")]
    #[test_case("[,...a]" => false; "[ Elision AssignmentRestElement ]: not present")]
    #[test_case("[,...this]" => true; "[ Elision AssignmentRestElement ]: present")]
    #[test_case("[a]" => false; "[ AssignmentElementList ]: not present")]
    #[test_case("[this]" => true; "[ AssignmentElementList ]: present")]
    #[test_case("[a,]" => false; "[ AssignmentElementList , ] (trailing comma): not present")]
    #[test_case("[this,]" => true; "[ AssignmentElementList , ] (trailing comma): present")]
    #[test_case("[a,,]" => false; "[ AssignmentElementList , Elision ]: not present")]
    #[test_case("[this,,]" => true; "[ AssignmentElementList , Elision ]: present")]
    #[test_case("[a,...b]" => false; "[ AssignmentELementList , AssignmentRestElement ]: not present")]
    #[test_case("[this,...b]" => true; "[ AssignmentELementList , AssignmentRestElement ]: present in List")]
    #[test_case("[a,...this]" => true; "[ AssignmentELementList , AssignmentRestElement ]: present in Rest")]
    #[test_case("[a,,...b]"=> false; "[ AssignmentElementList , Elision AssignmentRestElement ]: not present")]
    #[test_case("[this,,...b]" => true; "[ AssignmentElementList , Elision AssignmentRestElement ]: present in List")]
    #[test_case("[a,,...this]" => true; "[ AssignmentElementList , Elision AssignmentRestElement ]: present in Rest")]
    fn contains(src: &str) -> bool {
        let (node, _) = ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("[]" => true; "[ ] (nothing)")]
    #[test_case("[,]" => true; "[ Elision ]")]
    #[test_case("[...a.#valid]" => true; "[ AssignmentRestElement ]: valid")]
    #[test_case("[,...a.#valid]" => true; "[ Elision AssignmentRestElement ]: valid")]
    #[test_case("[a.#valid]" => true; "[ AssignmentElementList ]: valid")]
    #[test_case("[a.#valid,]" => true; "[ AssignmentElementList , ] (trailing comma): valid")]
    #[test_case("[a.#valid,,]" => true; "[ AssignmentElementList , Elision ]: valid")]
    #[test_case("[a.#valid,...b]" => true; "[ AssignmentELementList , AssignmentRestElement ]: list valid")]
    #[test_case("[a,...b.#valid]" => true; "[ AssignmentELementList , AssignmentRestElement ]: rest valid")]
    #[test_case("[a.#valid,,...b]" => true; "[ AssignmentElementList , Elision AssignmentRestElement ]: list valid")]
    #[test_case("[a,,...b.#valid]" => true; "[ AssignmentElementList , Elision AssignmentRestElement ]: rest valid")]
    #[test_case("[...a.#invalid]" => false; "[ AssignmentRestElement ]: invalid")]
    #[test_case("[,...a.#invalid]" => false; "[ Elision AssignmentRestElement ]: invalid")]
    #[test_case("[a.#invalid]" => false; "[ AssignmentElementList ]: invalid")]
    #[test_case("[a.#invalid,]" => false; "[ AssignmentElementList , ] (trailing comma): invalid")]
    #[test_case("[a.#invalid,,]" => false; "[ AssignmentElementList , Elision ]: invalid")]
    #[test_case("[a.#invalid,...b]" => false; "[ AssignmentELementList , AssignmentRestElement ]: list invalid")]
    #[test_case("[a,...b.#invalid]" => false; "[ AssignmentELementList , AssignmentRestElement ]: rest invalid")]
    #[test_case("[a.#invalid,,...b]" => false; "[ AssignmentElementList , Elision AssignmentRestElement ]: list invalid")]
    #[test_case("[a,,...b.#invalid]" => false; "[ AssignmentElementList , Elision AssignmentRestElement ]: rest invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("[]", true => AHashSet::<String>::new(); "empty")]
    #[test_case("[,]", true => AHashSet::<String>::new(); "[ Elision ]")]
    #[test_case("[...package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentRestElement ]")]
    #[test_case("[,...package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ Elision AssignmentRestElement ]")]
    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList ]")]
    #[test_case("[package,]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList , ] (trailing comma)")]
    #[test_case("[package,,]", true => set(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList , Elision ]")]
    #[test_case("[package,...interface]", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "[ AssignmentElementList , AssignmentRestElement ]")]
    #[test_case("[package,,...interface]", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "[ AssignmentElementList , Elision AssignmentRestElement ]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_rest_property {
    use super::*;
    use test_case::test_case;

    #[test_case("...a" => Ok((
        expected_scan(4),
        sv(&["AssignmentRestProperty: ... a", "DestructuringAssignmentTarget: a"]),
        sv(&["AssignmentRestProperty: ... a", "Punctuator: ...", "IdentifierName: a"])
    )); "... DestructuringAssignmentTarget")]
    #[test_case("" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Ellipsis), 1)); "empty")]
    #[test_case("..." => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 4)); "dots")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = AssignmentRestProperty::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("...a"; "... DestructuringAssignmentTarget")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("...a"; "... DestructuringAssignmentTarget")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("...this" => true; "has this")]
    #[test_case("...3" => false; "missing this")]
    fn contains(src: &str) -> bool {
        let (node, _) = AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("...item.#valid" => true; "valid")]
    #[test_case("...item.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("...package", true => set(&[PACKAGE_NOT_ALLOWED]); "... DestructuringAssignmentTarget")]
    #[test_case("...{a}", true => set(&["`...` must be followed by an assignable reference in assignment contexts"]); "assignable refs")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_property_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentPropertyList: a", "AssignmentProperty: a"]),
        sv(&["IdentifierName: a"])
    )); "AssignmentProperty")]
    #[test_case("a,b" => Ok((
        expected_scan(3),
        sv(&["AssignmentPropertyList: a , b", "AssignmentPropertyList: a", "AssignmentProperty: b"]),
        sv(&["AssignmentPropertyList: a , b", "IdentifierName: a", "Punctuator: ,", "IdentifierName: b"])
    )); "AssignmentPropertyList , AssignmentProperty")]
    #[test_case("" => Err(ParseError::new(PECode::IdRefOrPropertyNameExpected, 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = AssignmentPropertyList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("a"; "AssignmentProperty")]
    #[test_case("a,b"; "AssignmentPropertyList AssignmentProperty")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a"; "AssignmentProperty")]
    #[test_case("a,b"; "AssignmentPropertyList AssignmentProperty")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("a" => false; "AssignmentProperty: not present")]
    #[test_case("[this]:a" => true; "AssignmentProperty: present")]
    #[test_case("a,b" => false; "AssignmentPropertyList , AssignmentProperty: not present")]
    #[test_case("[this]:a,b" => true; "AssignmentPropertyList , AssignmentProperty: present in AssignmentPropertyList")]
    #[test_case("a,[this]:b" => true; "AssignmentPropertyList , AssignmentProperty: present in AssignmentProperty")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("[a.#valid]:q" => true; "AssignmentProperty: valid")]
    #[test_case("[a.#valid]:q,b" => true; "AssignmentPropertyList , AssignmentProperty: AssignmentPropertyList valid")]
    #[test_case("a,[b.#valid]:q" => true; "AssignmentPropertyList , AssignmentProperty: AssignmentProperty valid")]
    #[test_case("[a.#invalid]:q" => false; "AssignmentProperty: invalid")]
    #[test_case("[a.#invalid]:q,b" => false; "AssignmentPropertyList , AssignmentProperty: AssignmentPropertyList invalid")]
    #[test_case("a,[b.#invalid]:q" => false; "AssignmentPropertyList , AssignmentProperty: AssignmentProperty invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "AssignmentProperty")]
    #[test_case("package,interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentPropertyList , AssignmentProperty")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentElementList: a", "AssignmentElisionElement: a"]),
        sv(&["IdentifierName: a"])
    )); "AssignmentElisionElement")]
    #[test_case("a,b" => Ok((
        expected_scan(3),
        sv(&["AssignmentElementList: a , b", "AssignmentElementList: a", "AssignmentElisionElement: b"]),
        sv(&["AssignmentElementList: a , b", "IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]),
    )); "AssignmentElementList , AssignmentElisionElement")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentElementList::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = AssignmentElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("a"; "AssignmentElisionElement")]
    #[test_case("a,b"; "AssignmentElementList AssignmentElisionElement")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a"; "AssignmentElisionElement")]
    #[test_case("a,b"; "AssignmentElementList AssignmentElisionElement")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("a" => false; "AssignmentElisionElement: not present")]
    #[test_case("a[this]" => true; "AssignmentElisionElement: present")]
    #[test_case("a,b" => false; "AssignmentElementList , AssignmentElement: not present")]
    #[test_case("a[this],b" => true; "AssignmentElementList , AssignmentElement: present in AssignmentElementList")]
    #[test_case("a,b[this]" => true; "AssignmentElementList , AssignmentElement: present in AssignmentElement")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("a.#valid" => true; "AssignmentElisionElement: valid")]
    #[test_case("a.#valid,b" => true; "AssignmentElementList , AssignmentElisionElement: AssignmentElementList valid")]
    #[test_case("a,b.#valid" => true; "AssignmentElementList , AssignmentElisionElement: AssignmentElisionElement valid")]
    #[test_case("a.#invalid" => false; "AssignmentElisionElement: invalid")]
    #[test_case("a.#invalid,b" => false; "AssignmentElementList , AssignmentElisionElement: AssignmentElementList invalid")]
    #[test_case("a,b.#invalid" => false; "AssignmentElementList , AssignmentElisionElement: AssignmentElisionElement invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "AssignmentElisionElement")]
    #[test_case("package,interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentElementList , AssignmentElisionElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentElementList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_elision_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentElisionElement: a", "AssignmentElement: a"]),
        sv(&["IdentifierName: a"])
    )); "AssignmentElement")]
    #[test_case(",a" => Ok((
        expected_scan(2),
        sv(&["AssignmentElisionElement: , a", "Elisions: ,", "AssignmentElement: a"]),
        sv(&["AssignmentElisionElement: , a", "Elisions: ,", "IdentifierName: a"]),
    )); "Elision AssignmentElement")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (node, _) = AssignmentElisionElement::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", node));
    }

    #[test_case("a"; "AssignmentElement")]
    #[test_case(",a"; "Elision AssignmentElement")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a"; "AssignmentElement")]
    #[test_case(",a"; "Elision AssignmentElement")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("a" => false; "AssignmentElement: not present")]
    #[test_case("a[this]" => true; "AssignmentElement: present")]
    #[test_case(",a" => false; "Elision AssignmentElement: not present")]
    #[test_case(",a[this]" => true; "Elision AssignmentElement: present in AssignmentElement")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("a.#valid" => true; "AssignmentElement: valid")]
    #[test_case(",a.#valid" => true; "Elision AssignmentElement: AssignmentElement valid")]
    #[test_case("a.#invalid" => false; "AssignmentElement: invalid")]
    #[test_case(",a.#invalid" => false; "Elision AssignmentElement: AssignmentElement invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "AssignmentElement")]
    #[test_case(",package", true => set(&[PACKAGE_NOT_ALLOWED]); "Elision AssignmentElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_property {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentProperty: a", "IdentifierReference: a"]),
        sv(&["IdentifierName: a"]),
    )); "IdentifierReference")]
    #[test_case("a=0" => Ok((
        expected_scan(3),
        sv(&["AssignmentProperty: a = 0", "IdentifierReference: a", "Initializer: = 0"]),
        sv(&["AssignmentProperty: a = 0", "IdentifierName: a", "Initializer: = 0"])
    )); "IdentifierReference Initializer")]
    #[test_case("a:b" => Ok((
        expected_scan(3),
        sv(&["AssignmentProperty: a : b", "PropertyName: a", "AssignmentElement: b"]),
        sv(&["AssignmentProperty: a : b", "IdentifierName: a", "Punctuator: :", "IdentifierName: b"])
    )); "PropertyName : AssignmentElement")]
    #[test_case("" => Err(ParseError::new(PECode::IdRefOrPropertyNameExpected, 1)); "empty")]
    #[test_case("0" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Colon), 2)); "Error after PropertyName")]
    #[test_case("0:" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 3)); "Error after colon")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentProperty::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        let (item, _) = AssignmentProperty::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        assert_ne!(format!("{:?}", item), "");
    }

    #[test_case("a")]
    #[test_case("a=0")]
    #[test_case("a:b")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a")]
    #[test_case("a=0")]
    #[test_case("a:b")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("a" => false; "IdentifierReference: not present")]
    #[test_case("a=0" => false; "IdentifierReference Initializer: not present")]
    #[test_case("a=this" => true; "IdentifierReference Initializer: present in initialier")]
    #[test_case("[this]:a" => true; "PropertyName : AssignmentElement: present in PropertyName")]
    #[test_case("a:this" => true; "PropertyName : AssignmentElement: present in AssignmentElement")]
    #[test_case("a:b" => false; "PropertyName : AssignmentElement: not present")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("a" => true; "IdentifierReference")]
    #[test_case("a=b.#valid" => true; "IdentifierReference Initialier: Initializer valid")]
    #[test_case("[a.#valid]:0" => true; "PropertyName : AssignmentElement: PropertyName valid")]
    #[test_case("a:b.#valid" => true; "PropertyName : AssignmentElement: AssignmentElement valid")]
    #[test_case("a=b.#invalid" => false; "IdentifierReference Initialier: Initializer invalid")]
    #[test_case("[a.#invalid]:0" => false; "PropertyName : AssignmentElement: PropertyName invalid")]
    #[test_case("a:b.#invalid" => false; "PropertyName : AssignmentElement: AssignmentElement invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentProperty::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("eval", true => set(&["Identifier eval is an invalid left-hand-side"]); "IdentifierReference (eval)")]
    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "IdentifierReference (package)")]
    #[test_case("eval=interface", true => set(&["Identifier eval is an invalid left-hand-side", INTERFACE_NOT_ALLOWED]); "IdentifierReference Initializer (eval)")]
    #[test_case("package=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "IdentifierReference Initializer")]
    #[test_case("[package]:interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "PropertyName : AssignmentElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentProperty::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentElement: a", "DestructuringAssignmentTarget: a"]),
        sv(&["IdentifierName: a"]),
    )); "alone")]
    #[test_case("a=0" => Ok((
        expected_scan(3),
        sv(&["AssignmentElement: a = 0", "DestructuringAssignmentTarget: a", "Initializer: = 0"]),
        sv(&["AssignmentElement: a = 0", "IdentifierName: a", "Initializer: = 0"]),
    )); "with initializer")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        assert_ne!("", format!("{:?}", AssignmentElement::parse(&mut newparser("A"), Scanner::new(), false, false).unwrap()));
    }

    #[test_case("a")]
    #[test_case("a=0")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a")]
    #[test_case("a=0")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("this" => true; "no init; present")]
    #[test_case("a" => false; "no init; not present")]
    #[test_case("this=0" => true; "init; present in target")]
    #[test_case("a=this" => true; "init; present in init")]
    #[test_case("a=0" => false; "init; not present")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("item.#valid" => true; "no init; valid")]
    #[test_case("item.#valid=0" => true; "init; target valid")]
    #[test_case("a=item.#valid" => true; "init; init valid")]
    #[test_case("item.#invalid" => false; "no init; invalid")]
    #[test_case("item.#invalid=0" => false; "init; target invalid")]
    #[test_case("a=item.#invalid" => false; "init; init invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "DestructuringAssignmentTarget")]
    #[test_case("package=interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "DestructuringAssignmentTarget Initializer")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentElement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod assignment_rest_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a" => Ok((
        expected_scan(4),
        sv(&["AssignmentRestElement: ... a", "DestructuringAssignmentTarget: a"]),
        sv(&["AssignmentRestElement: ... a", "Punctuator: ...", "IdentifierName: a"])
    )); "normal")]
    #[test_case("" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Ellipsis), 1)); "empty")]
    #[test_case("..." => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 4)); "dots")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }
    #[test]
    fn debug() {
        let (item, _) = AssignmentRestElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", item));
    }
    #[test_case("...blue")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("...blue")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test_case("...this" => true; "has this")]
    #[test_case("...3" => false; "missing this")]
    fn contains(src: &str) -> bool {
        let (node, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("...item.#valid" => true; "valid")]
    #[test_case("...item.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("...package", true => set(&[PACKAGE_NOT_ALLOWED]); "... DestructuringAssignmentTarget")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AssignmentRestElement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod destructuring_assignment_target {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["DestructuringAssignmentTarget: a", "LeftHandSideExpression: a"]),
        sv(&["IdentifierName: a"])
    )); "LeftHandSideExpression")]
    #[test_case("{}" => Ok((
        expected_scan(2),
        sv(&["DestructuringAssignmentTarget: { }", "AssignmentPattern: { }"]),
        sv(&["ObjectAssignmentPattern: { }", "Punctuator: {", "Punctuator: }"])
    )); "AssignmentPattern")]
    #[test_case("" => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), 1)); "empty")]
    #[test_case("{...{},...{}}" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBrace), 7)); "ObjectLiteral but not AssignmentPattern")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }
    #[test]
    fn debug() {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser("k"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", item));
    }

    #[test_case("blue")]
    #[test_case("{}"; "ObjectLiteral")]
    fn pretty_errors(src: &str) {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("blue")]
    #[test_case("{}"; "ObjectLiteral")]
    fn concise_errors(src: &str) {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("this" => true; "has this")]
    #[test_case("3" => false; "missing this")]
    #[test_case("{[this]:a}" => true; "pattern with this")]
    #[test_case("{}" => false; "pattern without this")]
    fn contains(src: &str) -> bool {
        let (node, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("item.#valid" => true; "valid")]
    #[test_case("item.#invalid" => false; "invalid")]
    #[test_case("{[a.#valid]:b}" => true; "pattern valid")]
    #[test_case("{[a.#invalid]:b}" => false; "pattern invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("[package]", true => set(&[PACKAGE_NOT_ALLOWED]); "AssignmentPattern")]
    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "lhs")]
    #[test_case("(a=>a)", true => panics "not yet implemented"; "not simple")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        DestructuringAssignmentTarget::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

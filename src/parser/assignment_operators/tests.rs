use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

const INVALID_LHS: &str = "Invalid left-hand side in assignment";

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
    #[test_case("[a+1]=[1]" => Err(ParseError::new(PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBracket]), (1, 3, 1))); "bad destructuring")]
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
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_function_definition()
    }

    #[test_case("a", true => ATTKind::Simple; "Fall-thru (identifier)")]
    #[test_case("function (){}", true => ATTKind::Invalid; "Fall-thru (function exp)")]
    #[test_case("yield a", true => ATTKind::Invalid; "YieldExpression")]
    #[test_case("a=>a", true => ATTKind::Invalid; "ArrowFunction")]
    #[test_case("async a=>a", true => ATTKind::Invalid; "AsyncArrowFunction")]
    #[test_case("a=b", true => ATTKind::Invalid; "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b", true => ATTKind::Invalid; "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("a/=b", true => ATTKind::Invalid; "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("a%=b", true => ATTKind::Invalid; "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("a+=b", true => ATTKind::Invalid; "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("a-=b", true => ATTKind::Invalid; "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("a<<=b", true => ATTKind::Invalid; "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("a>>=b", true => ATTKind::Invalid; "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("a>>>=b", true => ATTKind::Invalid; "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("a&=b", true => ATTKind::Invalid; "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("a^=b", true => ATTKind::Invalid; "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("a|=b", true => ATTKind::Invalid; "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("a**=b", true => ATTKind::Invalid; "LeftHandSideExpression **= AssignmentExpression (exponentiation)")]
    #[test_case("a&&=b", true => ATTKind::Invalid; "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b", true => ATTKind::Invalid; "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b", true => ATTKind::Invalid; "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[a]=[1]", true => ATTKind::Invalid; "AssignmentPattern = AssignmentExpression")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("eval", false => ATTKind::Simple; "non-strict eval")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).assignment_expression().assignment_target_type(strict)
    }

    #[test]
    fn cache() {
        let mut parser = newparser("a+=b+c+d+e");
        let (node, scanner) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
        let (node2, scanner2) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }

    #[test_case("a"; "ConditionalExpression")]
    #[test_case("yield a"; "YieldExpression")]
    #[test_case("a=>a"; "ArrowFunction")]
    #[test_case("async a=>a"; "AsyncArrowFunction")]
    #[test_case("a=b"; "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (multiply)")]
    #[test_case("a/=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (divide)")]
    #[test_case("a%=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (modulo)")]
    #[test_case("a+=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (add)")]
    #[test_case("a-=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (subtract)")]
    #[test_case("a<<=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (left shift)")]
    #[test_case("a>>=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (right shift)")]
    #[test_case("a>>>=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (unsigned right shift)")]
    #[test_case("a&=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary and)")]
    #[test_case("a^=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary xor)")]
    #[test_case("a|=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary or)")]
    #[test_case("a**=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (exponentiate)")]
    #[test_case("a&&=b"; "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b"; "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b"; "LeftHandSideExpression ??= AssignmentExpression (coalese)")]
    #[test_case("[a]=[1]"; "AssignmentPattern = AssignmentExpression")]
    fn pretty_errors(src: &str) {
        let item = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0;
        pretty_error_validate(&*item);
    }

    #[test_case("a"; "ConditionalExpression")]
    #[test_case("yield a"; "YieldExpression")]
    #[test_case("a=>a"; "ArrowFunction")]
    #[test_case("async a=>a"; "AsyncArrowFunction")]
    #[test_case("a=b"; "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("a*=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (multiply)")]
    #[test_case("a/=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (divide)")]
    #[test_case("a%=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (modulo)")]
    #[test_case("a+=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (add)")]
    #[test_case("a-=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (subtract)")]
    #[test_case("a<<=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (left shift)")]
    #[test_case("a>>=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (right shift)")]
    #[test_case("a>>>=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (unsigned right shift)")]
    #[test_case("a&=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary and)")]
    #[test_case("a^=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary xor)")]
    #[test_case("a|=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (binary or)")]
    #[test_case("a**=b"; "LeftHandSideExpression AssignmentOperator AssignmentExpression (exponentiate)")]
    #[test_case("a&&=b"; "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("a||=b"; "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("a??=b"; "LeftHandSideExpression ??= AssignmentExpression (coalese)")]
    #[test_case("[a]=[1]"; "AssignmentPattern = AssignmentExpression")]
    fn concise_errors(src: &str) {
        let item = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0;
        concise_error_validate(&*item);
    }

    #[test_case("this", ParseNodeKind::This => true; "ConditionalExpression (present)")]
    #[test_case("0", ParseNodeKind::This => false; "ConditionalExpression (missing)")]
    #[test_case("yield this", ParseNodeKind::This => true; "YieldExpression (present)")]
    #[test_case("yield 0", ParseNodeKind::This => false; "YieldExpression (missing)")]
    #[test_case("x => { this; }", ParseNodeKind::This => true; "ArrowFunction (present)")]
    #[test_case("x => { 0; }", ParseNodeKind::This => false; "ArrowFunction (missing)")]
    #[test_case("async x => { this; }", ParseNodeKind::This => true; "AsyncArrowFunction (present)")]
    #[test_case("async x => { 0; }", ParseNodeKind::This => false; "AsyncArrowFunction (missing)")]
    #[test_case("this.a = 0", ParseNodeKind::This => true; "LHS = AE (left)")]
    #[test_case("a = this", ParseNodeKind::This => true; "LHS = AE (right)")]
    #[test_case("a = 0", ParseNodeKind::This => false; "LHS = AE (nowhere)")]
    #[test_case("this.a *= 0", ParseNodeKind::This => true; "LHS op AE (left)")]
    #[test_case("a *= this", ParseNodeKind::This => true; "LHS op AE (right)")]
    #[test_case("a *= 0", ParseNodeKind::This => false; "LHS op AE (nowhere)")]
    #[test_case("this.a &&= 0", ParseNodeKind::This => true; "LHS land AE (left)")]
    #[test_case("a &&= this", ParseNodeKind::This => true; "LHS land AE (right)")]
    #[test_case("a &&= 0", ParseNodeKind::This => false; "LHS land AE (nowhere)")]
    #[test_case("this.a ||= 0", ParseNodeKind::This => true; "LHS lor AE (left)")]
    #[test_case("a ||= this", ParseNodeKind::This => true; "LHS lor AE (right)")]
    #[test_case("a ||= 0", ParseNodeKind::This => false; "LHS lor AE (nowhere)")]
    #[test_case("this.a ??= 0", ParseNodeKind::This => true; "LHS maybe AE (left)")]
    #[test_case("a ??= this", ParseNodeKind::This => true; "LHS maybe AE (right)")]
    #[test_case("a ??= 0", ParseNodeKind::This => false; "LHS maybe AE (nowhere)")]
    #[test_case("[a]=[this]", ParseNodeKind::This => true; "Pattern = AE (right)")]
    #[test_case("[a=this]=[0]", ParseNodeKind::This => true; "Pattern = AE (left)")]
    #[test_case("[a]=[0]", ParseNodeKind::This => false; "Pattern = AE (nowhere)")]
    #[test_case("a ? b : c", ParseNodeKind::ConditionalExpression => true; "Conditional")]
    #[test_case("yield x", ParseNodeKind::YieldExpression => true; "Yield exp")]
    #[test_case("a => a", ParseNodeKind::ArrowFunction => true; "Arrow fcn")]
    #[test_case("async a => z", ParseNodeKind::AsyncArrowFunction => true; "Async Arrow")]
    #[test_case("a=b", ParseNodeKind::LeftHandSideExpression => true; "normal lhs")]
    #[test_case("a=b", ParseNodeKind::AssignmentExpression => true; "normal ae")]
    #[test_case("a*=b", ParseNodeKind::LeftHandSideExpression => true; "assop lhs")]
    #[test_case("a*=b", ParseNodeKind::AssignmentExpression => true; "assop ae")]
    #[test_case("a*=b", ParseNodeKind::AssignmentOperator => true; "assop op")]
    #[test_case("a&&=b", ParseNodeKind::LeftHandSideExpression => true; "land lhs")]
    #[test_case("a&&=b", ParseNodeKind::AssignmentExpression => true; "land ae")]
    #[test_case("a||=b", ParseNodeKind::LeftHandSideExpression => true; "lor lhs")]
    #[test_case("a||=b", ParseNodeKind::AssignmentExpression => true; "lor ae")]
    #[test_case("a??=b", ParseNodeKind::LeftHandSideExpression => true; "coal lhs")]
    #[test_case("a??=b", ParseNodeKind::AssignmentExpression => true; "coal ae")]
    #[test_case("[a]=b", ParseNodeKind::AssignmentPattern => true; "dest pat")]
    #[test_case("[a]=b", ParseNodeKind::AssignmentExpression => true; "dest ae")]
    fn contains(src: &str, target: ParseNodeKind) -> bool {
        Maker::new(src).assignment_expression().contains(target)
    }

    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a=b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
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
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "Fall-thru (identifier)")]
    #[test_case("yield package", true => sset(&[PACKAGE_NOT_ALLOWED]); "YieldExpression")]
    #[test_case("package=>interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "ArrowFunction")]
    #[test_case("async package=>interface", true => panics "not yet implemented"; "AsyncArrowFunction")]
    #[test_case("package=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression = AssignmentExpression (assignment)")]
    #[test_case("package*=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression *= AssignmentExpression (multiply)")]
    #[test_case("package/=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression /= AssignmentExpression (divide)")]
    #[test_case("package%=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression %= AssignmentExpression (modulo)")]
    #[test_case("package+=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression += AssignmentExpression (add)")]
    #[test_case("package-=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression -= AssignmentExpression (subtract)")]
    #[test_case("package<<=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression <<= AssignmentExpression (lshift)")]
    #[test_case("package>>=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression >>= AssignmentExpression (rshift)")]
    #[test_case("package>>>=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression >>>= AssignmentExpression (urshift)")]
    #[test_case("package&=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression &= AssignmentExpression (bitwise and)")]
    #[test_case("package^=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ^= AssignmentExpression (bitwise xor)")]
    #[test_case("package|=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression |= AssignmentExpression (bitwise or)")]
    #[test_case("package**=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression **= AssignmentExpression (exponentiation)")]
    #[test_case("package&&=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression &&= AssignmentExpression (logical and)")]
    #[test_case("package||=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ||= AssignmentExpression (logical or)")]
    #[test_case("package??=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "LeftHandSideExpression ??= AssignmentExpression (coalesce)")]
    #[test_case("[package]=[interface]", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentPattern = AssignmentExpression")]
    #[test_case("(a=>a)=b", true => sset(&[INVALID_LHS]); "LHS must be simple (assignment)")]
    #[test_case("(a=>a)*=b", true => sset(&[INVALID_LHS]); "LHS must be simple (multiplication)")]
    #[test_case("(a=>a)/=b", true => sset(&[INVALID_LHS]); "LHS must be simple (division)")]
    #[test_case("(a=>a)%=b", true => sset(&[INVALID_LHS]); "LHS must be simple (modulo)")]
    #[test_case("(a=>a)+=b", true => sset(&[INVALID_LHS]); "LHS must be simple (add)")]
    #[test_case("(a=>a)-=b", true => sset(&[INVALID_LHS]); "LHS must be simple (subtract)")]
    #[test_case("(a=>a)<<=b", true => sset(&[INVALID_LHS]); "LHS must be simple (lsh)")]
    #[test_case("(a=>a)>>=b", true => sset(&[INVALID_LHS]); "LHS must be simple (rsh)")]
    #[test_case("(a=>a)>>>=b", true => sset(&[INVALID_LHS]); "LHS must be simple (ursh)")]
    #[test_case("(a=>a)&=b", true => sset(&[INVALID_LHS]); "LHS must be simple (bitwise and)")]
    #[test_case("(a=>a)^=b", true => sset(&[INVALID_LHS]); "LHS must be simple (xor)")]
    #[test_case("(a=>a)|=b", true => sset(&[INVALID_LHS]); "LHS must be simple (bitwise or)")]
    #[test_case("(a=>a)**=b", true => sset(&[INVALID_LHS]); "LHS must be simple (exponentiation)")]
    #[test_case("(a=>a)&&=b", true => sset(&[INVALID_LHS]); "LHS must be simple (logical and)")]
    #[test_case("(a=>a)||=b", true => sset(&[INVALID_LHS]); "LHS must be simple (logical or)")]
    #[test_case("(a=>a)??=b", true => sset(&[INVALID_LHS]); "LHS must be simple (coalesce)")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a=3" => true; "assignment op")]
    fn is_strictly_deletable(src: &str) -> bool {
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("yield arguments" => true; "Yield (yes)")]
    #[test_case("x => x+arguments" => true; "Arrow (yes)")]
    #[test_case("async x => x + arguments" => true; "AsyncArrow (yes)")]
    #[test_case("arguments = bob" => true; "Assignment (left)")]
    #[test_case("bob = arguments" => true; "Assignment (right)")]
    #[test_case("arguments += bob" => true; "Assignment op (left)")]
    #[test_case("bob += arguments" => true; "Assignment op (right)")]
    #[test_case("arguments &&= bob" => true; "LAND (left)")]
    #[test_case("bob &&= arguments" => true; "LAND (right)")]
    #[test_case("arguments ||= bob" => true; "LOR (left)")]
    #[test_case("bob ||= arguments" => true; "LOR (right)")]
    #[test_case("arguments ??= bob" => true; "Coal (left)")]
    #[test_case("bob ??= arguments" => true; "Coal (right)")]
    #[test_case("{arguments} = bob" => true; "Destructuring (left)")]
    #[test_case("{bob} = arguments" => true; "Destructuring (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("yield xyzzy" => false; "Yield (no)")]
    #[test_case("x => x+xyzzy" => false; "Arrow (no)")]
    #[test_case("async x => x + xyzzy" => false; "AsyncArrow (no)")]
    #[test_case("xyzzy = bob" => false; "Assignment (no)")]
    #[test_case("xyzzy += bob" => false; "Assignment op (no)")]
    #[test_case("xyzzy &&= bob" => false; "LAND (no)")]
    #[test_case("xyzzy ||= bob" => false; "LOR (no)")]
    #[test_case("xyzzy ??= bob" => false; "Coal (no)")]
    #[test_case("{xyzzy} = bob" => false; "Destructuring (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("function blue(){}" => false; "named function")]
    #[test_case("function (){}" => true; "anonymous function")]
    #[test_case("function *a(){}" => false; "named generator")]
    #[test_case("function *(){}" => true; "anonymous generator")]
    #[test_case("async function blue(){}" => false; "async named function")]
    #[test_case("async function (){}" => true; "async anonymous function")]
    #[test_case("async function *a(){}" => false; "async named generator")]
    #[test_case("async function *(){}" => true; "async anonymous generator")]
    #[test_case("class {}" => true; "anonymous class")]
    #[test_case("class a{}" => false; "named class")]
    #[test_case("x => x*2" => true; "arrow function")]
    #[test_case("async x => x + 3" => true; "async arrow function")]
    #[test_case("10" => false; "literal")]
    #[test_case("(function *(){})" => true; "parenthesized function expr")]
    fn is_anonymous_function_definition(src: &str) -> bool {
        Maker::new(src).assignment_expression().is_anonymous_function_definition()
    }

    #[test_case("function blue(){}" => true; "named function")]
    #[test_case("function (){}" => false; "anonymous function")]
    #[test_case("function *a(){}" => true; "named generator")]
    #[test_case("function *(){}" => false; "anonymous generator")]
    #[test_case("async function blue(){}" => true; "async named function")]
    #[test_case("async function (){}" => false; "async anonymous function")]
    #[test_case("async function *a(){}" => true; "async named generator")]
    #[test_case("async function *(){}" => false; "async anonymous generator")]
    #[test_case("class {}" => false; "anonymous class")]
    #[test_case("class a{}" => true; "named class")]
    #[test_case("x => x*2" => false; "arrow function")]
    #[test_case("async x => x + 3" => false; "async arrow function")]
    #[test_case("10" => false; "literal")]
    #[test_case("(function *(){})" => false; "parenthesized function expr")]
    #[test_case("a+b" => false; "additive")]
    #[test_case("a^b" => false; "bitwise xor")]
    #[test_case("a&b" => false; "bitwise and")]
    #[test_case("a|b" => false; "bitwise or")]
    #[test_case("a??b" => false; "coalesce")]
    #[test_case("a||b" => false; "logical or")]
    #[test_case("a&&b" => false; "logical and")]
    #[test_case("a<<b" => false; "shift expr")]
    #[test_case("(a,b)" => false; "comma")]
    #[test_case("a==b" => false; "equality")]
    #[test_case("a**b" => false; "exponent")]
    #[test_case("a.b" => false; "member")]
    #[test_case("a()" => false; "call")]
    #[test_case("new a" => false; "new expr")]
    #[test_case("a*b" => false; "multiplicative")]
    #[test_case("a<b" => false; "relational")]
    #[test_case("-a" => false; "unary expr")]
    #[test_case("a++" => false; "update expr")]
    #[test_case("a?b:c" => false; "conditional")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).assignment_expression().is_named_function()
    }

    #[test_case(" 12" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 2 } }; "fall-thru")]
    #[test_case(" yield 12" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 8 } }; "yield expression")]
    #[test_case(" x => x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 6 } }; "arrow function")]
    #[test_case(" async x => x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 12 } }; "async arrow function")]
    #[test_case(" x = x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 5 } }; "assignment")]
    #[test_case(" x += x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 6 } }; "op-assignment")]
    #[test_case(" x &&= x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 7 } }; "logical-and assignment")]
    #[test_case(" x ||= x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 7 } }; "logical-or assignment")]
    #[test_case(" x ??= x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 7 } }; "coalesce assignment")]
    #[test_case(" {alpha, beta} = x" => Location { starting_line: 1, starting_column: 2, span: Span { starting_index: 1, length: 17 } }; "destructuring assignment")]
    fn location(src: &str) -> Location {
        Maker::new(src).assignment_expression().location()
    }

    #[test_case("10" => None; "literal")]
    #[test_case("function(){}" => ssome("function (  ) {  }"); "fall-thru; function")]
    #[test_case("x => x" => ssome("x => x"); "arrow")]
    #[test_case("async x => x" => ssome("async x => x"); "async arrow")]
    #[test_case("x = 10" => None; "other assignment")]
    fn function_definition(src: &str) -> Option<String> {
        Maker::new(src).assignment_expression().function_definition().map(|x| x.to_string())
    }

    #[test_case("function(){}" => ssome("function (  ) {  }"); "nameless function")]
    #[test_case("function foo(){}" => None; "named function")]
    #[test_case("x = 10" => None; "not a function")]
    fn anonymous_function_definition(src: &str) -> Option<String> {
        Maker::new(src).assignment_expression().anonymous_function_definition().map(|x| x.to_string())
    }
}

mod assignment_operator {
    use super::*;
    use test_case::test_case;

    #[test_case(AssignmentOperator::Multiply => false; "Multiply")]
    #[test_case(AssignmentOperator::Divide => false; "Divide")]
    #[test_case(AssignmentOperator::Modulo => false; "Modulo")]
    #[test_case(AssignmentOperator::Add => false; "Add")]
    #[test_case(AssignmentOperator::Subtract => false; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift => false; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift => false; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift => false; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd => false; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor => false; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr => false; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate => false; "Exponentiate")]
    fn contains(op: AssignmentOperator) -> bool {
        op.contains(ParseNodeKind::This)
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", AssignmentOperator::BitwiseOr), "");
    }

    #[test_case(AssignmentOperator::Multiply => "*="; "Multiply")]
    #[test_case(AssignmentOperator::Divide => "/="; "Divide")]
    #[test_case(AssignmentOperator::Modulo => "%="; "Modulo")]
    #[test_case(AssignmentOperator::Add => "+="; "Add")]
    #[test_case(AssignmentOperator::Subtract => "-="; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift => "<<="; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift => ">>="; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift => ">>>="; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd => "&="; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor => "^="; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr => "|="; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate => "**="; "Exponentiate")]
    fn display(op: AssignmentOperator) -> String {
        format!("{}", op)
    }

    #[test_case(AssignmentOperator::Multiply => vec!["AssignmentOperator: *="]; "Multiply")]
    #[test_case(AssignmentOperator::Divide => vec!["AssignmentOperator: /="]; "Divide")]
    #[test_case(AssignmentOperator::Modulo => vec!["AssignmentOperator: %="]; "Modulo")]
    #[test_case(AssignmentOperator::Add => vec!["AssignmentOperator: +="]; "Add")]
    #[test_case(AssignmentOperator::Subtract => vec!["AssignmentOperator: -="]; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift => vec!["AssignmentOperator: <<="]; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift => vec!["AssignmentOperator: >>="]; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift => vec!["AssignmentOperator: >>>="]; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd => vec!["AssignmentOperator: &="]; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor => vec!["AssignmentOperator: ^="]; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr => vec!["AssignmentOperator: |="]; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate => vec!["AssignmentOperator: **="]; "Exponentiate")]
    fn pretty(op: AssignmentOperator) -> Vec<String> {
        pretty_data(&op)
    }

    #[test_case(AssignmentOperator::Multiply => vec!["Punctuator: *="]; "Multiply")]
    #[test_case(AssignmentOperator::Divide => vec!["Punctuator: /="]; "Divide")]
    #[test_case(AssignmentOperator::Modulo => vec!["Punctuator: %="]; "Modulo")]
    #[test_case(AssignmentOperator::Add => vec!["Punctuator: +="]; "Add")]
    #[test_case(AssignmentOperator::Subtract => vec!["Punctuator: -="]; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift => vec!["Punctuator: <<="]; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift => vec!["Punctuator: >>="]; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift => vec!["Punctuator: >>>="]; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd => vec!["Punctuator: &="]; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor => vec!["Punctuator: ^="]; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr => vec!["Punctuator: |="]; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate => vec!["Punctuator: **="]; "Exponentiate")]
    fn concise(op: AssignmentOperator) -> Vec<String> {
        concise_data(&op)
    }

    #[test_case(AssignmentOperator::Multiply; "Multiply")]
    #[test_case(AssignmentOperator::Divide; "Divide")]
    #[test_case(AssignmentOperator::Modulo; "Modulo")]
    #[test_case(AssignmentOperator::Add; "Add")]
    #[test_case(AssignmentOperator::Subtract; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate; "Exponentiate")]
    fn pretty_errors(op: AssignmentOperator) {
        pretty_error_validate(&op);
    }

    #[test_case(AssignmentOperator::Multiply; "Multiply")]
    #[test_case(AssignmentOperator::Divide; "Divide")]
    #[test_case(AssignmentOperator::Modulo; "Modulo")]
    #[test_case(AssignmentOperator::Add; "Add")]
    #[test_case(AssignmentOperator::Subtract; "Subtract")]
    #[test_case(AssignmentOperator::LeftShift; "LeftShift")]
    #[test_case(AssignmentOperator::SignedRightShift; "SignedRightShift")]
    #[test_case(AssignmentOperator::UnsignedRightShift; "UnsignedRightShift")]
    #[test_case(AssignmentOperator::BitwiseAnd; "BitwiseAnd")]
    #[test_case(AssignmentOperator::BitwiseXor; "BitwiseXor")]
    #[test_case(AssignmentOperator::BitwiseOr; "BitwiseOr")]
    #[test_case(AssignmentOperator::Exponentiate; "Exponentiate")]
    fn concise_errors(op: AssignmentOperator) {
        concise_error_validate(&op);
    }
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

    #[test_case("{package}", true => sset(&[PACKAGE_NOT_ALLOWED]); "ObjectAssignmentPattern")]
    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "ArrayAssignmentPattern")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("[arguments]" => true; "array (yes)")]
    #[test_case("{arguments}" => true; "object (yes)")]
    #[test_case("[xyzzy]" => false; "array (no)")]
    #[test_case("{xyzzy}" => false; "object (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case(" {a,b,c}" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 7 } }; "object pattern")]
    #[test_case(" [x,y,z]" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 7 } }; "array pattern")]
    fn location(src: &str) -> Location {
        Maker::new(src).assignment_pattern().location()
    }

    #[test_case("[]" => true; "is a pattern")]
    fn is_destructuring(src: &str) -> bool {
        Maker::new(src).assignment_pattern().is_destructuring()
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
    #[test_case("{...package}", true => sset(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentRestProperty }")]
    #[test_case("{package}", true => sset(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentPropertyList }")]
    #[test_case("{package,}", true => sset(&[PACKAGE_NOT_ALLOWED]); "{ AssignmentPropertyList , } (trailing comma)")]
    #[test_case("{package,...interface}", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "{ AssignmentPropertyList , AssignmentRestProperty }")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("{}" => false; "empty")]
    #[test_case("{...arguments}" => true; "Rest Only (yes)")]
    #[test_case("{arguments}" => true; "List (yes)")]
    #[test_case("{arguments,}" => true; "Comma (yes)")]
    #[test_case("{arguments,...bob}" => true; "List+Rest (left)")]
    #[test_case("{bob,...arguments}" => true; "List+Rest (right)")]
    #[test_case("{...xyzzy}" => false; "Rest Only (no)")]
    #[test_case("{xyzzy}" => false; "List (no)")]
    #[test_case("{xyzzy,}" => false; "Comma (no)")]
    #[test_case("{xyzzy,...bob}" => false; "List+Rest (no)")]
    fn contains_arguments(src: &str) -> bool {
        ObjectAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case(" {}" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 2 } }; "empty")]
    #[test_case(" { ...a }" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 8 } }; "rest-only")]
    #[test_case(" {a,b,c}" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 7 } }; "list-only")]
    #[test_case(" {a,...b}" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 8 } }; "list+rest")]
    fn location(src: &str) -> Location {
        Maker::new(src).object_assignment_pattern().location()
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
    #[test_case("[...package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentRestElement ]")]
    #[test_case("[,...package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ Elision AssignmentRestElement ]")]
    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList ]")]
    #[test_case("[package,]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList , ] (trailing comma)")]
    #[test_case("[package,,]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentElementList , Elision ]")]
    #[test_case("[package,...interface]", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "[ AssignmentElementList , AssignmentRestElement ]")]
    #[test_case("[package,,...interface]", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "[ AssignmentElementList , Elision AssignmentRestElement ]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("[]" => false; "Empty")]
    #[test_case("[,]" => false; "Elision")]
    #[test_case("[...arguments]" => true; "Rest Only (yes)")]
    #[test_case("[,...arguments]" => true; "Elision Rest (yes)")]
    #[test_case("[arguments]" => true; "List (yes)")]
    #[test_case("[arguments,]" => true; "Comma (yes)")]
    #[test_case("[arguments,,]" => true; "List Elision (yes)")]
    #[test_case("[arguments,...bob]" => true; "List Rest (left)")]
    #[test_case("[bob,...arguments]" => true; "List Rest (right)")]
    #[test_case("[arguments,,...bob]" => true; "List Elision Rest (left)")]
    #[test_case("[bob,,...arguments]" => true; "List Elision Rest (right)")]
    #[test_case("[...xyzzy]" => false; "Rest Only (no)")]
    #[test_case("[,...xyzzy]" => false; "Elision Rest (no)")]
    #[test_case("[xyzzy]" => false; "List (no)")]
    #[test_case("[xyzzy,]" => false; "Comma (no)")]
    #[test_case("[xyzzy,,]" => false; "List Elision (no)")]
    #[test_case("[xyzzy,...bob]" => false; "List Rest (no)")]
    #[test_case("[xyzzy,,...bob]" => false; "List Elision Rest (no)")]
    fn contains_arguments(src: &str) -> bool {
        ArrayAssignmentPattern::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case(" [ ...a ]" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 8 } }; "rest-only")]
    #[test_case(" [1,2,3]" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 7 } }; "list-only")]
    #[test_case(" [1,...b]" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 8 } }; "list+rest")]
    fn location(src: &str) -> Location {
        Maker::new(src).array_assignment_pattern().location()
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

    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "... DestructuringAssignmentTarget")]
    #[test_case("...{a}", true => sset(&["`...` must be followed by an assignable reference in assignment contexts"]); "assignable refs")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("...arguments" => true; "Rest (yes)")]
    #[test_case("...xyzzy" => false; "Rest (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentRestProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentProperty")]
    #[test_case("package,interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentPropertyList , AssignmentProperty")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "Item (yes)")]
    #[test_case("arguments,bob" => true; "List (left)")]
    #[test_case("bob,arguments" => true; "List (right)")]
    #[test_case("xyzzy" => false; "Item (no)")]
    #[test_case("xyzzy,bob" => false; "List (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentPropertyList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentElisionElement")]
    #[test_case("package,interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AssignmentElementList , AssignmentElisionElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentElementList::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "Item (yes)")]
    #[test_case("arguments,bob" => true; "List (left)")]
    #[test_case("bob,arguments" => true; "List (right)")]
    #[test_case("xyzzy" => false; "Item (no)")]
    #[test_case("xyzzy,bob" => false; "List (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentElementList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentElement")]
    #[test_case(",package", true => sset(&[PACKAGE_NOT_ALLOWED]); "Elision AssignmentElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "Item (yes)")]
    #[test_case(",arguments" => true; "Elision Item (yes)")]
    #[test_case("xyzzy" => false; "Item (no)")]
    #[test_case(",xyzzy" => false; "Elision Item (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentElisionElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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

    #[test_case("eval", true => sset(&["Identifier eval is an invalid left-hand-side"]); "IdentifierReference (eval)")]
    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "IdentifierReference (package)")]
    #[test_case("eval=interface", true => sset(&["Identifier eval is an invalid left-hand-side", INTERFACE_NOT_ALLOWED]); "IdentifierReference Initializer (eval)")]
    #[test_case("package=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "IdentifierReference Initializer")]
    #[test_case("[package]:interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "PropertyName : AssignmentElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentProperty::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "id (yes)")]
    #[test_case("arguments=bob" => true; "initialized (left)")]
    #[test_case("bob=arguments" => true; "initialized (right)")]
    #[test_case("[arguments]:bob" => true; "prop : exp (left)")]
    #[test_case("[bob]:arguments" => true; "prop : exp (right)")]
    #[test_case("xyzzy" => false; "id (no)")]
    #[test_case("xyzzy=bob" => false; "initialized (no)")]
    #[test_case("[xyzzy]:bob" => false; "prop : exp (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentProperty::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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
        assert_ne!(
            "",
            format!("{:?}", AssignmentElement::parse(&mut newparser("A"), Scanner::new(), false, false).unwrap())
        );
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "DestructuringAssignmentTarget")]
    #[test_case("package=interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "DestructuringAssignmentTarget Initializer")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "Item (yes)")]
    #[test_case("arguments=bob" => true; "Initializer (left)")]
    #[test_case("bob=arguments" => true; "Initializer (right)")]
    #[test_case("xyzzy" => false; "Item (no)")]
    #[test_case("xyzzy=bob" => false; "Initializer (no)")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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

    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "... DestructuringAssignmentTarget")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("...arguments" => true; "yes")]
    #[test_case("...no" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
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
    #[test_case("{...{},...{}}" => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::RightBrace), (1, 7, 1))); "ObjectLiteral but not AssignmentPattern")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }
    #[test]
    fn debug() {
        let (item, _) =
            DestructuringAssignmentTarget::parse(&mut newparser("k"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", item));
    }

    #[test_case("blue")]
    #[test_case("{}"; "ObjectLiteral")]
    fn pretty_errors(src: &str) {
        let (item, _) =
            DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("blue")]
    #[test_case("{}"; "ObjectLiteral")]
    fn concise_errors(src: &str) {
        let (item, _) =
            DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
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

    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentPattern")]
    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "lhs")]
    #[test_case("(a=>a)", true => sset(&[INVALID_LHS]); "not simple")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("{arguments}" => true; "Pattern (yes)")]
    #[test_case("no" => false; "Exp (no)")]
    #[test_case("{no}" => false; "Pattern (no)")]
    fn contains_arguments(src: &str) -> bool {
        DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .contains_arguments()
    }
}

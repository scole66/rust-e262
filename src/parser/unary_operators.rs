use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::async_function_definitions::AwaitExpression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// UnaryExpression[Yield, Await] :
//      UpdateExpression[?Yield, ?Await]
//      delete UnaryExpression[?Yield, ?Await]
//      void UnaryExpression[?Yield, ?Await]
//      typeof UnaryExpression[?Yield, ?Await]
//      + UnaryExpression[?Yield, ?Await]
//      - UnaryExpression[?Yield, ?Await]
//      ~ UnaryExpression[?Yield, ?Await]
//      ! UnaryExpression[?Yield, ?Await]
//      [+Await]AwaitExpression[?Yield]
#[derive(Debug)]
pub enum UnaryExpression {
    UpdateExpression(Box<UpdateExpression>),
    Delete(Box<UnaryExpression>),
    Void(Box<UnaryExpression>),
    Typeof(Box<UnaryExpression>),
    NoOp(Box<UnaryExpression>),
    Negate(Box<UnaryExpression>),
    Complement(Box<UnaryExpression>),
    Not(Box<UnaryExpression>),
    Await(Box<AwaitExpression>),
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryExpression::UpdateExpression(boxed) => write!(f, "{}", boxed),
            UnaryExpression::Delete(boxed) => write!(f, "delete {}", boxed),
            UnaryExpression::Void(boxed) => write!(f, "void {}", boxed),
            UnaryExpression::Typeof(boxed) => write!(f, "typeof {}", boxed),
            UnaryExpression::NoOp(boxed) => write!(f, "+ {}", boxed),
            UnaryExpression::Negate(boxed) => write!(f, "- {}", boxed),
            UnaryExpression::Complement(boxed) => write!(f, "~ {}", boxed),
            UnaryExpression::Not(boxed) => write!(f, "! {}", boxed),
            UnaryExpression::Await(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for UnaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UnaryExpression: {}", first, self)?;
        match &self {
            UnaryExpression::UpdateExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Delete(boxed)
            | UnaryExpression::Void(boxed)
            | UnaryExpression::Typeof(boxed)
            | UnaryExpression::NoOp(boxed)
            | UnaryExpression::Negate(boxed)
            | UnaryExpression::Complement(boxed)
            | UnaryExpression::Not(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Await(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |node: &UnaryExpression, op, kind| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}UnaryExpression: {}", first, self)
                .and_then(|_| pprint_token(writer, op, kind, &successive, Spot::NotFinal))
                .and_then(|_| node.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            UnaryExpression::UpdateExpression(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Await(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Delete(node) => work(node, "delete", TokenType::Keyword),
            UnaryExpression::Void(node) => work(node, "void", TokenType::Keyword),
            UnaryExpression::Typeof(node) => work(node, "typeof", TokenType::Keyword),
            UnaryExpression::NoOp(node) => work(node, "+", TokenType::Punctuator),
            UnaryExpression::Negate(node) => work(node, "-", TokenType::Punctuator),
            UnaryExpression::Complement(node) => work(node, "~", TokenType::Punctuator),
            UnaryExpression::Not(node) => work(node, "!", TokenType::Punctuator),
        }
    }
}

impl IsFunctionDefinition for UnaryExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.is_function_definition(),
            UnaryExpression::Delete(_)
            | UnaryExpression::Void(_)
            | UnaryExpression::Typeof(_)
            | UnaryExpression::NoOp(_)
            | UnaryExpression::Negate(_)
            | UnaryExpression::Complement(_)
            | UnaryExpression::Not(_)
            | UnaryExpression::Await(_) => false,
        }
    }
}

impl AssignmentTargetType for UnaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.assignment_target_type(),
            UnaryExpression::Delete(_)
            | UnaryExpression::Void(_)
            | UnaryExpression::Typeof(_)
            | UnaryExpression::NoOp(_)
            | UnaryExpression::Negate(_)
            | UnaryExpression::Complement(_)
            | UnaryExpression::Not(_)
            | UnaryExpression::Await(_) => ATTKind::Invalid,
        }
    }
}

impl UnaryExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (token, after_token) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        let mut unary_helper = |f: fn(Box<Self>) -> Self| UnaryExpression::parse(parser, after_token, yield_flag, await_flag).map(|(boxed, after)| (Box::new(f(boxed)), after));
        match token {
            Token::Identifier(id) if id.matches(Keyword::Delete) => unary_helper(UnaryExpression::Delete),
            Token::Identifier(id) if id.matches(Keyword::Void) => unary_helper(UnaryExpression::Void),
            Token::Identifier(id) if id.matches(Keyword::Typeof) => unary_helper(UnaryExpression::Typeof),
            Token::Punctuator(Punctuator::Plus) => unary_helper(UnaryExpression::NoOp),
            Token::Punctuator(Punctuator::Minus) => unary_helper(UnaryExpression::Negate),
            Token::Punctuator(Punctuator::Tilde) => unary_helper(UnaryExpression::Complement),
            Token::Punctuator(Punctuator::Bang) => unary_helper(UnaryExpression::Not),
            _ => Err(ParseError::new("UnaryExpression expected", scanner.line, scanner.column))
                .otherwise(|| {
                    if await_flag {
                        AwaitExpression::parse(parser, scanner, yield_flag).map(|(ae, after)| (Box::new(UnaryExpression::Await(ae)), after))
                    } else {
                        Err(ParseError::new(String::new(), scanner.line, scanner.column))
                    }
                })
                .otherwise(|| UpdateExpression::parse(parser, scanner, yield_flag, await_flag).map(|(ue, after)| (Box::new(UnaryExpression::UpdateExpression(ue)), after))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // UNARY EXPRESSION
    #[test]
    fn unary_expression_test_update_expression() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("900"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UnaryExpression::UpdateExpression(_)));
        pretty_check(&*ue, "UnaryExpression: 900", vec!["UpdateExpression: 900"]);
        concise_check(&*ue, "Numeric: 900", vec![]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_delete() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("delete bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Delete(_)));
        pretty_check(&*ue, "UnaryExpression: delete bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: delete bob", vec!["Keyword: delete", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_void() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("void bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(*ue, UnaryExpression::Void(_)));
        pretty_check(&*ue, "UnaryExpression: void bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: void bob", vec!["Keyword: void", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_typeof() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("typeof bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Typeof(_)));
        pretty_check(&*ue, "UnaryExpression: typeof bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: typeof bob", vec!["Keyword: typeof", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_numberify() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("+bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::NoOp(_)));
        pretty_check(&*ue, "UnaryExpression: + bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: + bob", vec!["Punctuator: +", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_negate() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("-bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Negate(_)));
        pretty_check(&*ue, "UnaryExpression: - bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: - bob", vec!["Punctuator: -", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_complement() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("~bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Complement(_)));
        pretty_check(&*ue, "UnaryExpression: ~ bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: ~ bob", vec!["Punctuator: ~", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_not() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("!bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Not(_)));
        pretty_check(&*ue, "UnaryExpression: ! bob", vec!["UnaryExpression: bob"]);
        concise_check(&*ue, "UnaryExpression: ! bob", vec!["Punctuator: !", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_await() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
        chk_scan(&scanner, 9);
        assert!(matches!(*ue, UnaryExpression::Await(_)));
        pretty_check(&*ue, "UnaryExpression: await bob", vec!["AwaitExpression: await bob"]);
        concise_check(&*ue, "AwaitExpression: await bob", vec!["Keyword: await", "IdentifierName: bob"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
        format!("{:?}", ue);
    }
    #[test]
    fn unary_expression_test_nomatch() {
        check_err(UnaryExpression::parse(&mut newparser(""), Scanner::new(), false, false), "UnaryExpression expected", 1, 1);
    }
    #[test]
    fn unary_expression_test_incomplete() {
        check_err(UnaryExpression::parse(&mut newparser("delete"), Scanner::new(), false, false), "UnaryExpression expected", 1, 7);
        check_err(UnaryExpression::parse(&mut newparser("void"), Scanner::new(), false, false), "UnaryExpression expected", 1, 5);
        check_err(UnaryExpression::parse(&mut newparser("typeof"), Scanner::new(), false, false), "UnaryExpression expected", 1, 7);
        check_err(UnaryExpression::parse(&mut newparser("+"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
        check_err(UnaryExpression::parse(&mut newparser("-"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
        check_err(UnaryExpression::parse(&mut newparser("~"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
        check_err(UnaryExpression::parse(&mut newparser("!"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
        check_err(UnaryExpression::parse(&mut newparser("await"), Scanner::new(), false, true), "UnaryExpression expected", 1, 6);
    }

    #[test]
    fn unary_expression_test_prettyerrors_1() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_2() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_3() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_4() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_5() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_6() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_7() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_prettyerrors_8() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_1() {
        let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_2() {
        let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_3() {
        let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_4() {
        let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_5() {
        let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_6() {
        let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_7() {
        let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn unary_expression_test_conciseerrors_8() {
        let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
        concise_error_validate(&*item);
    }
}

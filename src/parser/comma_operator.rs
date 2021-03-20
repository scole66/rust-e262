use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// Expression[In, Yield, Await] :
//      AssignmentExpression[?In, ?Yield, ?Await]
//      Expression[?In, ?Yield, ?Await] , AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Expression {
    FallThru(Rc<AssignmentExpression>),
    Comma(Rc<Expression>, Rc<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::FallThru(node) => node.fmt(f),
            Expression::Comma(left, right) => write!(f, "{} , {}", left, right),
        }
    }
}

impl PrettyPrint for Expression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Expression: {}", first, self)?;
        match &self {
            Expression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Expression::Comma(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Expression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            Expression::Comma(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}Expression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for Expression {
    fn is_function_definition(&self) -> bool {
        match &self {
            Expression::FallThru(node) => node.is_function_definition(),
            Expression::Comma(_, _) => false,
        }
    }
}

impl AssignmentTargetType for Expression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            Expression::FallThru(node) => node.assignment_target_type(),
            Expression::Comma(_, _) => ATTKind::Invalid,
        }
    }
}

impl Expression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Expression expected", scanner.line, scanner.column)).otherwise(|| {
            AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
                let mut current = Rc::new(Expression::FallThru(left));
                let mut current_scanner = after_left;
                while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                    .and_then(|after_token| AssignmentExpression::parse(parser, after_token, in_flag, yield_flag, await_flag))
                {
                    current = Rc::new(Expression::Comma(current, right));
                    current_scanner = after_right;
                }
                (current, current_scanner)
            })
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // EXPRESSION
    #[test]
    fn expression_test_01() {
        let (se, scanner) = check(Expression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, Expression::FallThru(_)));
        pretty_check(&*se, "Expression: a", vec!["AssignmentExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn expression_test_02() {
        let (se, scanner) = check(Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*se, Expression::Comma(..)));
        pretty_check(&*se, "Expression: a , b", vec!["Expression: a", "AssignmentExpression: b"]);
        concise_check(&*se, "Expression: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn expression_test_cache_01() {
        let mut parser = newparser("blue(67)+90");
        let (node, scanner) = check(Expression::parse(&mut parser, Scanner::new(), true, false, false));
        let (node2, scanner2) = check(Expression::parse(&mut parser, Scanner::new(), true, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn expression_test_03() {
        let (node, scanner) = check(Expression::parse(&mut newparser("a,"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
    }
    #[test]
    fn expression_test_prettyerrors_1() {
        let (item, _) = Expression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn expression_test_prettyerrors_2() {
        let (item, _) = Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn expression_test_conciseerrors_1() {
        let (item, _) = Expression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn expression_test_conciseerrors_2() {
        let (item, _) = Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
}

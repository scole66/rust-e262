use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::binary_logical_operators::ShortCircuitExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ConditionalExpression[In, Yield, Await] :
//      ShortCircuitExpression[?In, ?Yield, ?Await]
//      ShortCircuitExpression[?In, ?Yield, ?Await] ? AssignmentExpression[+In, ?Yield, ?Await] : AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ConditionalExpression {
    FallThru(Rc<ShortCircuitExpression>),
    Conditional(Rc<ShortCircuitExpression>, Rc<AssignmentExpression>, Rc<AssignmentExpression>),
}

impl fmt::Display for ConditionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ConditionalExpression::FallThru(node) => node.fmt(f),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                write!(f, "{} ? {} : {}", condition, thenish, elseish)
            }
        }
    }
}

impl PrettyPrint for ConditionalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
        match &self {
            ConditionalExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                condition.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                thenish.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elseish.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ConditionalExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ConditionalExpression::Conditional(a, b, c) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
                a.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "?", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                b.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                c.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for ConditionalExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => false,
            ConditionalExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ConditionalExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => ATTKind::Invalid,
            ConditionalExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl ConditionalExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (left, after_left) = ShortCircuitExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match scan_for_punct(after_left, parser.source, ScanGoal::InputElementDiv, Punctuator::Question)
            .and_then(|after_q| AssignmentExpression::parse(parser, after_q, true, yield_flag, await_flag))
            .and_then(|(ae1, after_ae1)| {
                scan_for_punct(after_ae1, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)
                    .and_then(|after_colon| AssignmentExpression::parse(parser, after_colon, in_flag, yield_flag, await_flag))
                    .map(|(ae2, after_ae2)| (ae1, ae2, after_ae2))
            }) {
            Ok((thenish, elseish, after)) => Ok((Rc::new(ConditionalExpression::Conditional(left, thenish, elseish)), after)),
            Err(_) => Ok((Rc::new(ConditionalExpression::FallThru(left)), after_left)),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.conditional_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.conditional_expression_cache.insert(key, result.clone());
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

    // CONDITIONAL EXPRESSION
    #[test]
    fn conditional_expression_test_01() {
        let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ConditionalExpression::FallThru(_)));
        pretty_check(&*se, "ConditionalExpression: a", vec!["ShortCircuitExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn conditional_expression_test_02() {
        let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:c"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, ConditionalExpression::Conditional(..)));
        pretty_check(&*se, "ConditionalExpression: a ? b : c", vec!["ShortCircuitExpression: a", "AssignmentExpression: b", "AssignmentExpression: c"]);
        concise_check(&*se, "ConditionalExpression: a ? b : c", vec!["IdentifierName: a", "Punctuator: ?", "IdentifierName: b", "Punctuator: :", "IdentifierName: c"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn conditional_expression_test_03() {
        let (node, scanner) = check(ConditionalExpression::parse(&mut newparser("a?"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
    }
    #[test]
    fn conditional_expression_test_04() {
        let (node, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
    }
    #[test]
    fn conditional_expression_test_05() {
        let (node, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
    }
    #[test]
    fn conditional_expression_test_prettyerrors_1() {
        let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conditional_expression_test_prettyerrors_2() {
        let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conditional_expression_test_conciseerrors_1() {
        let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conditional_expression_test_conciseerrors_2() {
        let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
}

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::multiplicative_operators::MultiplicativeExpression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum AdditiveExpression {
    MultiplicativeExpression(Box<MultiplicativeExpression>),
    AdditiveExpressionAdd((Box<AdditiveExpression>, Box<MultiplicativeExpression>)),
    AdditiveExpressionSubtract((Box<AdditiveExpression>, Box<MultiplicativeExpression>)),
}

impl fmt::Display for AdditiveExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AdditiveExpression::MultiplicativeExpression(boxed) => write!(f, "{}", boxed),
            AdditiveExpression::AdditiveExpressionAdd((ae, me)) => {
                write!(f, "{} + {}", ae, me)
            }
            AdditiveExpression::AdditiveExpressionSubtract((ae, me)) => {
                write!(f, "{} - {}", ae, me)
            }
        }
    }
}

impl PrettyPrint for AdditiveExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AdditiveExpression: {}", first, self)?;
        match &self {
            AdditiveExpression::MultiplicativeExpression(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AdditiveExpression::AdditiveExpressionAdd((ae, me))
            | AdditiveExpression::AdditiveExpressionSubtract((ae, me)) => {
                ae.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                me.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |left: &Box<AdditiveExpression>, right: &Box<MultiplicativeExpression>, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}AdditiveExpression: {}", first, self)
                .and_then(|_| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, &successive, Spot::NotFinal))
                .and_then(|_| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            AdditiveExpression::MultiplicativeExpression(node) => node.concise_with_leftpad(writer, pad, state),
            AdditiveExpression::AdditiveExpressionAdd((left, right)) => work(left, right, "+"),
            AdditiveExpression::AdditiveExpressionSubtract((left, right)) => work(left, right, "-"),
        }
    }
}

impl IsFunctionDefinition for AdditiveExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            AdditiveExpression::AdditiveExpressionAdd(_) | AdditiveExpression::AdditiveExpressionSubtract(_) => false,
            AdditiveExpression::MultiplicativeExpression(me) => me.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for AdditiveExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            AdditiveExpression::AdditiveExpressionAdd(_) | AdditiveExpression::AdditiveExpressionSubtract(_) => {
                ATTKind::Invalid
            }
            AdditiveExpression::MultiplicativeExpression(me) => me.assignment_target_type(),
        }
    }
}

impl AdditiveExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_me = MultiplicativeExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_me {
            Some((me, after_me)) => {
                let mut current = Box::new(AdditiveExpression::MultiplicativeExpression(me));
                let mut current_scanner = after_me;
                loop {
                    let (token, after_op) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                    let kind_fn: fn(Box<AdditiveExpression>, Box<MultiplicativeExpression>) -> AdditiveExpression;
                    match token {
                        scanner::Token::Plus => {
                            kind_fn = |ae, me| AdditiveExpression::AdditiveExpressionAdd((ae, me));
                        }
                        scanner::Token::Minus => {
                            kind_fn = |ae, me| AdditiveExpression::AdditiveExpressionSubtract((ae, me));
                        }
                        _ => {
                            break;
                        }
                    }
                    let pot_me2 = MultiplicativeExpression::parse(parser, after_op, yield_flag, await_flag)?;
                    match pot_me2 {
                        None => {
                            break;
                        }
                        Some((me2, after_me2)) => {
                            current = Box::new(kind_fn(current, me2));
                            current_scanner = after_me2;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};

    // ADDITIVE EXPRESSION
    #[test]
    fn additive_expression_test_01() {
        let (ae, scanner) = check(AdditiveExpression::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(_)));
        pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn additive_expression_test_02() {
        let (ae, scanner) = check(AdditiveExpression::parse(
            &mut newparser("a+b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*ae, AdditiveExpression::AdditiveExpressionAdd(_)));
        pretty_check(
            &*ae,
            "AdditiveExpression: a + b",
            vec!["AdditiveExpression: a", "MultiplicativeExpression: b"],
        );
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn additive_expression_test_03() {
        let (ae, scanner) = check(AdditiveExpression::parse(
            &mut newparser("a-b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*ae, AdditiveExpression::AdditiveExpressionSubtract(_)));
        pretty_check(
            &*ae,
            "AdditiveExpression: a - b",
            vec!["AdditiveExpression: a", "MultiplicativeExpression: b"],
        );
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn additive_expression_test_04() {
        let (ae, scanner) = check(AdditiveExpression::parse(
            &mut newparser("a-@"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(_)));
        pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn additive_expression_test_05() {
        check_none(AdditiveExpression::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn additive_expression_test_06() {
        assert!(AdditiveExpression::parse(&mut newparser("\\u0066or"), Scanner::new(), false, false).is_err());
        assert!(AdditiveExpression::parse(&mut newparser("a + \\u0066or"), Scanner::new(), false, false).is_err());
    }
    #[test]
    fn additive_expression_test_prettyerrors() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("3+4"), Scanner::new(), false, false)
            .unwrap()
            .unwrap();
        pretty_error_validate(*item);
    }
}

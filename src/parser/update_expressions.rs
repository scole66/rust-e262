use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// UpdateExpression[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] ++
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] --
//      ++ UnaryExpression[?Yield, ?Await]
//      -- UnaryExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum UpdateExpression {
    LeftHandSideExpression(Box<LeftHandSideExpression>),
    PostIncrement(Box<LeftHandSideExpression>),
    PostDecrement(Box<LeftHandSideExpression>),
    PreIncrement(Box<UnaryExpression>),
    PreDecrement(Box<UnaryExpression>),
}

impl fmt::Display for UpdateExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.fmt(f),
            UpdateExpression::PostIncrement(boxed) => write!(f, "{} ++", boxed),
            UpdateExpression::PostDecrement(boxed) => write!(f, "{} --", boxed),
            UpdateExpression::PreIncrement(boxed) => write!(f, "++ {}", boxed),
            UpdateExpression::PreDecrement(boxed) => write!(f, "-- {}", boxed),
        }
    }
}

impl PrettyPrint for UpdateExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UpdateExpression: {}", first, self)?;
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) | UpdateExpression::PostIncrement(boxed) | UpdateExpression::PostDecrement(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            UpdateExpression::PreIncrement(boxed) | UpdateExpression::PreDecrement(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}UpdateExpression: {}", first, self).and(Ok(successive))
        };
        let workafter = |writer: &mut T, node: &Box<LeftHandSideExpression>, op: &str| {
            head(writer).and_then(|successive| node.concise_with_leftpad(writer, &successive, Spot::NotFinal).and_then(|_| pprint_token(writer, op, &successive, Spot::Final)))
        };
        let workbefore = |writer: &mut T, node: &Box<UnaryExpression>, op: &str| {
            head(writer).and_then(|successive| pprint_token(writer, op, &successive, Spot::NotFinal).and_then(|_| node.concise_with_leftpad(writer, &successive, Spot::Final)))
        };
        match self {
            UpdateExpression::LeftHandSideExpression(node) => node.concise_with_leftpad(writer, pad, state),
            UpdateExpression::PostIncrement(node) => workafter(writer, node, "++"),
            UpdateExpression::PostDecrement(node) => workafter(writer, node, "--"),
            UpdateExpression::PreIncrement(node) => workbefore(writer, node, "++"),
            UpdateExpression::PreDecrement(node) => workbefore(writer, node, "--"),
        }
    }
}

impl IsFunctionDefinition for UpdateExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.is_function_definition(),
            UpdateExpression::PostIncrement(_) | UpdateExpression::PostDecrement(_) | UpdateExpression::PreIncrement(_) | UpdateExpression::PreDecrement(_) => false,
        }
    }
}

impl AssignmentTargetType for UpdateExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.assignment_target_type(),
            UpdateExpression::PostIncrement(_) | UpdateExpression::PostDecrement(_) | UpdateExpression::PreIncrement(_) | UpdateExpression::PreDecrement(_) => ATTKind::Invalid,
        }
    }
}

impl UpdateExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("UpdateExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_plusses = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::PlusPlus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_plusses, yield_flag, await_flag)?;
                Ok((Box::new(UpdateExpression::PreIncrement(ue)), after_ue))
            })
            .otherwise(|| {
                let after_minuses = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::MinusMinus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_minuses, yield_flag, await_flag)?;
                Ok((Box::new(UpdateExpression::PreDecrement(ue)), after_ue))
            })
            .otherwise(|| {
                enum AftLHS {
                    Nothing,
                    Inc,
                    Dec,
                }
                let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
                no_line_terminator(after_lhs, parser.source)
                    .and_then(|()| {
                        let (punct, after_punct) = scan_for_punct_set(after_lhs, parser.source, ScanGoal::InputElementDiv, &[Punctuator::PlusPlus, Punctuator::MinusMinus])?;
                        match punct {
                            Punctuator::PlusPlus => Ok((AftLHS::Inc, after_punct)),
                            Punctuator::MinusMinus | _ => Ok((AftLHS::Dec, after_punct)),
                        }
                    })
                    .otherwise(|| Ok((AftLHS::Nothing, after_lhs)))
                    .map(|(aft, scan)| {
                        (
                            Box::new(match aft {
                                AftLHS::Nothing => UpdateExpression::LeftHandSideExpression(lhs),
                                AftLHS::Inc => UpdateExpression::PostIncrement(lhs),
                                AftLHS::Dec => UpdateExpression::PostDecrement(lhs),
                            }),
                            scan,
                        )
                    })
            })
    }
}
// Punctuator::PlusPlus => Ok((Box::new(UpdateExpression::PostIncrement(lhs)), after_punct)),
// Punctuator::MinusMinus|_ => Ok((Box::new(UpdateExpression::PostDecrement(lhs)), after_punct))

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;
    // UPDATE EXPRESSION
    #[test]
    fn update_expression_test_lhs() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("78"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: 78", vec!["LeftHandSideExpression: 78"]);
    }
    #[test]
    fn update_expression_test_preinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: ++ a", vec!["UnaryExpression: a"]);
    }
    #[test]
    fn update_expression_test_predec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: -- a", vec!["UnaryExpression: a"]);
    }
    #[test]
    fn update_expression_test_postinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a ++", vec!["LeftHandSideExpression: a"]);
    }
    #[test]
    fn update_expression_test_postdec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a --", vec!["LeftHandSideExpression: a"]);
    }
    #[test]
    fn update_expression_test_newline() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a\n++"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    }
    #[test]
    fn update_expression_test_nomatch() {
        check_err(UpdateExpression::parse(&mut newparser("**"), Scanner::new(), false, false), "UpdateExpression expected", 1, 1);
    }
    #[test]
    fn update_expression_test_syntax_error_01() {
        check_err(UpdateExpression::parse(&mut newparser("++ ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
    }
    #[test]
    fn update_expression_test_syntax_error_02() {
        check_err(UpdateExpression::parse(&mut newparser("-- ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
    }
}

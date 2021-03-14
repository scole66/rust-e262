use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::arrow_function_definitions::ArrowFunction;
use super::async_arrow_function_definitions::AsyncArrowFunction;
use super::conditional_operator::ConditionalExpression;
use super::generator_function_definitions::YieldExpression;
use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AssignmentExpression[In, Yield, Await] :
//      ConditionalExpression[?In, ?Yield, ?Await]
//      [+Yield]YieldExpression[?In, ?Await]
//      ArrowFunction[?In, ?Yield, ?Await]
//      AsyncArrowFunction[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] = AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] &&= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ||= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ??= AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentExpression {
    FallThru(Rc<ConditionalExpression>),
    Yield(Rc<YieldExpression>),
    Arrow(Rc<ArrowFunction>),
    AsyncArrow(Rc<AsyncArrowFunction>),
    Assignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    OpAssignment(Rc<LeftHandSideExpression>, AssignmentOperator, Rc<AssignmentExpression>),
    LandAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    LorAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    CoalAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
}

impl fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentExpression::FallThru(node) => node.fmt(f),
            AssignmentExpression::Yield(node) => node.fmt(f),
            AssignmentExpression::Arrow(node) => node.fmt(f),
            AssignmentExpression::AsyncArrow(node) => node.fmt(f),
            AssignmentExpression::Assignment(left, right) => write!(f, "{} = {}", left, right),
            AssignmentExpression::OpAssignment(left, op, right) => write!(f, "{} {} {}", left, op, right),
            AssignmentExpression::LandAssignment(left, right) => write!(f, "{} &&= {}", left, right),
            AssignmentExpression::LorAssignment(left, right) => write!(f, "{} ||= {}", left, right),
            AssignmentExpression::CoalAssignment(left, right) => write!(f, "{} ??= {}", left, right),
        }
    }
}

impl PrettyPrint for AssignmentExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
        match &self {
            AssignmentExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Yield(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Arrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::AsyncArrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            AssignmentExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Yield(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Arrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::AsyncArrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Assignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LandAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "&&=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LorAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "||=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::CoalAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "??=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for AssignmentExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _) => false,
            AssignmentExpression::Arrow(_) | AssignmentExpression::AsyncArrow(_) => true,
            AssignmentExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for AssignmentExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Arrow(_)
            | AssignmentExpression::AsyncArrow(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _) => ATTKind::Invalid,
            AssignmentExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl AssignmentExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("AssignmentExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                if yield_flag {
                    YieldExpression::parse(parser, scanner, in_flag, await_flag).map(|(yieldexp, after_yield)| (Rc::new(AssignmentExpression::Yield(yieldexp)), after_yield))
                } else {
                    Err(ParseError::new(String::new(), scanner.line, scanner.column))
                }
            })
            .otherwise(|| ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(af, after_af)| (Rc::new(AssignmentExpression::Arrow(af)), after_af)))
            .otherwise(|| AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(aaf, after_aaf)| (Rc::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf)))
            .otherwise(|| {
                LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(lhs, after_lhs)| {
                    scan_for_punct_set(
                        after_lhs,
                        parser.source,
                        ScanGoal::InputElementDiv,
                        &[
                            Punctuator::Eq,
                            Punctuator::AmpAmpEq,
                            Punctuator::PipePipeEq,
                            Punctuator::QQEq,
                            Punctuator::StarEq,
                            Punctuator::SlashEq,
                            Punctuator::PercentEq,
                            Punctuator::PlusEq,
                            Punctuator::MinusEq,
                            Punctuator::LtLtEq,
                            Punctuator::GtGtEq,
                            Punctuator::GtGtGtEq,
                            Punctuator::AmpEq,
                            Punctuator::PipeEq,
                            Punctuator::StarStarEq,
                            Punctuator::CaretEq,
                        ],
                    )
                    .and_then(|(op, after_op)| {
                        let make_ae = match op {
                            Punctuator::Eq => |lhs, ae| AssignmentExpression::Assignment(lhs, ae),
                            Punctuator::AmpAmpEq => |lhs, ae| AssignmentExpression::LandAssignment(lhs, ae),
                            Punctuator::PipePipeEq => |lhs, ae| AssignmentExpression::LorAssignment(lhs, ae),
                            Punctuator::QQEq => |lhs, ae| AssignmentExpression::CoalAssignment(lhs, ae),
                            Punctuator::StarEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Multiply, ae),
                            Punctuator::SlashEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Divide, ae),
                            Punctuator::PercentEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Modulo, ae),
                            Punctuator::PlusEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Add, ae),
                            Punctuator::MinusEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Subtract, ae),
                            Punctuator::LtLtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::LeftShift, ae),
                            Punctuator::GtGtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::SignedRightShift, ae),
                            Punctuator::GtGtGtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::UnsignedRightShift, ae),
                            Punctuator::AmpEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseAnd, ae),
                            Punctuator::PipeEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseOr, ae),
                            Punctuator::StarStarEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Exponentiate, ae),
                            _ => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae),
                        };
                        AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag).map(|(ae, after_ae)| (Rc::new(make_ae(lhs, ae)), after_ae))
                    })
                })
            })
            .otherwise(|| ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(ce, after_ce)| (Rc::new(AssignmentExpression::FallThru(ce)), after_ce)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.assignment_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.assignment_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// AssignmentOperator : one of
//      *= /= %= += -= <<= >>= >>>= &= ^= |= **=
#[derive(Debug)]
pub enum AssignmentOperator {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    LeftShift,
    SignedRightShift,
    UnsignedRightShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Exponentiate,
}

impl fmt::Display for AssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentOperator::Multiply => write!(f, "*="),
            AssignmentOperator::Divide => write!(f, "/="),
            AssignmentOperator::Modulo => write!(f, "%="),
            AssignmentOperator::Add => write!(f, "+="),
            AssignmentOperator::Subtract => write!(f, "-="),
            AssignmentOperator::LeftShift => write!(f, "<<="),
            AssignmentOperator::SignedRightShift => write!(f, ">>="),
            AssignmentOperator::UnsignedRightShift => write!(f, ">>>="),
            AssignmentOperator::BitwiseAnd => write!(f, "&="),
            AssignmentOperator::BitwiseXor => write!(f, "^="),
            AssignmentOperator::BitwiseOr => write!(f, "|="),
            AssignmentOperator::Exponentiate => write!(f, "**="),
        }
    }
}

impl PrettyPrint for AssignmentOperator {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentOperator: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, self, TokenType::Punctuator, pad, state)
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    #[test]
    fn assignment_expression_test_01() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*node, AssignmentExpression::FallThru(..)));
        pretty_check(&*node, "AssignmentExpression: a", vec!["ConditionalExpression: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn assignment_expression_test_02() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*node, AssignmentExpression::Yield(..)));
        pretty_check(&*node, "AssignmentExpression: yield a", vec!["YieldExpression: yield a"]);
        concise_check(&*node, "YieldExpression: yield a", vec!["Keyword: yield", "IdentifierName: a"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_03() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::Arrow(..)));
        pretty_check(&*node, "AssignmentExpression: a => a", vec!["ArrowFunction: a => a"]);
        concise_check(&*node, "ArrowFunction: a => a", vec!["IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
        format!("{:?}", node);
        assert!(node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_031() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(&*node, AssignmentExpression::AsyncArrow(..)));
        pretty_check(&*node, "AssignmentExpression: async a => a", vec!["AsyncArrowFunction: async a => a"]);
        concise_check(&*node, "AsyncArrowFunction: async a => a", vec!["Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
        format!("{:?}", node);
        assert!(node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_04() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*node, AssignmentExpression::Assignment(..)));
        pretty_check(&*node, "AssignmentExpression: a = b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a = b", vec!["IdentifierName: a", "Punctuator: =", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_05() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a *= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: *=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a *= b", vec!["IdentifierName: a", "Punctuator: *=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_06() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a /= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: /=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a /= b", vec!["IdentifierName: a", "Punctuator: /=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_07() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a %= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: %=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a %= b", vec!["IdentifierName: a", "Punctuator: %=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_08() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a += b", vec!["LeftHandSideExpression: a", "AssignmentOperator: +=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a += b", vec!["IdentifierName: a", "Punctuator: +=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_09() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a -= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: -=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a -= b", vec!["IdentifierName: a", "Punctuator: -=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_10() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a <<= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: <<=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a <<= b", vec!["IdentifierName: a", "Punctuator: <<=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_11() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a >>= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: >>=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a >>= b", vec!["IdentifierName: a", "Punctuator: >>=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_12() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a >>>= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: >>>=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a >>>= b", vec!["IdentifierName: a", "Punctuator: >>>=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_13() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a &= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: &=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a &= b", vec!["IdentifierName: a", "Punctuator: &=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_14() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a ^= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: ^=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a ^= b", vec!["IdentifierName: a", "Punctuator: ^=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_15() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a |= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: |=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a |= b", vec!["IdentifierName: a", "Punctuator: |=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_16() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a **= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: **=", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a **= b", vec!["IdentifierName: a", "Punctuator: **=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_17() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::LandAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a &&= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a &&= b", vec!["IdentifierName: a", "Punctuator: &&=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_18() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::LorAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a ||= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a ||= b", vec!["IdentifierName: a", "Punctuator: ||=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn assignment_expression_test_19() {
        let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*node, AssignmentExpression::CoalAssignment(..)));
        pretty_check(&*node, "AssignmentExpression: a ??= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
        concise_check(&*node, "AssignmentExpression: a ??= b", vec!["IdentifierName: a", "Punctuator: ??=", "IdentifierName: b"]);
        format!("{:?}", node);
        assert!(!node.is_function_definition());
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
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
}

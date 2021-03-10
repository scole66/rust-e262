use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::binary_bitwise_operators::BitwiseORExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LogicalANDExpression[In, Yield, Await] :
//      BitwiseORExpression[?In, ?Yield, ?Await]
//      LogicalANDExpression[?In, ?Yield, ?Await] && BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LogicalANDExpression {
    BitwiseORExpression(Box<BitwiseORExpression>),
    LogicalAND(Box<LogicalANDExpression>, Box<BitwiseORExpression>),
}

impl fmt::Display for LogicalANDExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LogicalANDExpression::BitwiseORExpression(pn) => write!(f, "{}", pn),
            LogicalANDExpression::LogicalAND(left, right) => write!(f, "{} && {}", left, right),
        }
    }
}

impl PrettyPrint for LogicalANDExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LogicalANDExpression: {}", first, self)?;
        match &self {
            LogicalANDExpression::BitwiseORExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LogicalANDExpression::LogicalAND(left, right) => {
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
            LogicalANDExpression::BitwiseORExpression(node) => node.concise_with_leftpad(writer, pad, state),
            LogicalANDExpression::LogicalAND(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}LogicalANDExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "&&", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for LogicalANDExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            LogicalANDExpression::LogicalAND(_, _) => false,
            LogicalANDExpression::BitwiseORExpression(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for LogicalANDExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            LogicalANDExpression::LogicalAND(_, _) => ATTKind::Invalid,
            LogicalANDExpression::BitwiseORExpression(node) => node.assignment_target_type(),
        }
    }
}

impl LogicalANDExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        BitwiseORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
            let mut current = Box::new(LogicalANDExpression::BitwiseORExpression(left));
            let mut current_scanner = after_left;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::AmpAmp)
                .and_then(|after_op| BitwiseORExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Box::new(LogicalANDExpression::LogicalAND(current, right));
                current_scanner = after_right;
            }
            (current, current_scanner)
        })
    }
}

// LogicalORExpression[In, Yield, Await] :
//      LogicalANDExpression[?In, ?Yield, ?Await]
//      LogicalORExpression[?In, ?Yield, ?Await] || LogicalANDExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LogicalORExpression {
    LogicalANDExpression(Box<LogicalANDExpression>),
    LogicalOR(Box<LogicalORExpression>, Box<LogicalANDExpression>),
}

impl fmt::Display for LogicalORExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LogicalORExpression::LogicalANDExpression(node) => node.fmt(f),
            LogicalORExpression::LogicalOR(left, right) => write!(f, "{} || {}", left, right),
        }
    }
}

impl PrettyPrint for LogicalORExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LogicalORExpression: {}", first, self)?;
        match &self {
            LogicalORExpression::LogicalANDExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LogicalORExpression::LogicalOR(left, right) => {
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
            LogicalORExpression::LogicalANDExpression(node) => node.concise_with_leftpad(writer, pad, state),
            LogicalORExpression::LogicalOR(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}LogicalORExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "||", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for LogicalORExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            LogicalORExpression::LogicalOR(_, _) => false,
            LogicalORExpression::LogicalANDExpression(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for LogicalORExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            LogicalORExpression::LogicalOR(_, _) => ATTKind::Invalid,
            LogicalORExpression::LogicalANDExpression(node) => node.assignment_target_type(),
        }
    }
}

impl LogicalORExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        LogicalANDExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
            let mut current = Box::new(LogicalORExpression::LogicalANDExpression(left));
            let mut current_scanner = after_left;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::PipePipe)
                .and_then(|after_op| LogicalANDExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Box::new(LogicalORExpression::LogicalOR(current, right));
                current_scanner = after_right;
            }
            (current, current_scanner)
        })
    }
}

// CoalesceExpression[In, Yield, Await] :
//      CoalesceExpressionHead[?In, ?Yield, ?Await] ?? BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub struct CoalesceExpression {
    head: Box<CoalesceExpressionHead>,
    tail: Box<BitwiseORExpression>,
}

impl fmt::Display for CoalesceExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ?? {}", self.head, self.tail)
    }
}

impl PrettyPrint for CoalesceExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoalesceExpression: {}", first, self)?;
        self.head.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.tail.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoalesceExpression: {}", first, self)?;
        self.head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "??", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.tail.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoalesceExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        BitwiseORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(left, after_left)| {
            let mut current_head = CoalesceExpressionHead::BitwiseORExpression(left);
            let mut current_scanner = after_left;
            let mut exp_scanner = scanner;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::QQ)
                .and_then(|after_op| BitwiseORExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current_head = CoalesceExpressionHead::CoalesceExpression(Box::new(CoalesceExpression { head: Box::new(current_head), tail: right }));
                exp_scanner = after_right;
                current_scanner = after_right;
            }
            match current_head {
                CoalesceExpressionHead::BitwiseORExpression(_) => Err(ParseError::new("Invalid Coalesce Expression", scanner.line, scanner.column)),
                CoalesceExpressionHead::CoalesceExpression(exp) => Ok((exp, exp_scanner)),
            }
        })
    }
}

// CoalesceExpressionHead[In, Yield, Await] :
//      CoalesceExpression[?In, ?Yield, ?Await]
//      BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum CoalesceExpressionHead {
    CoalesceExpression(Box<CoalesceExpression>),
    BitwiseORExpression(Box<BitwiseORExpression>),
}

impl fmt::Display for CoalesceExpressionHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CoalesceExpressionHead::CoalesceExpression(node) => node.fmt(f),
            CoalesceExpressionHead::BitwiseORExpression(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for CoalesceExpressionHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoalesceExpressionHead: {}", first, self)?;
        match &self {
            CoalesceExpressionHead::CoalesceExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CoalesceExpressionHead::BitwiseORExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            CoalesceExpressionHead::CoalesceExpression(node) => node.concise_with_leftpad(writer, pad, state),
            CoalesceExpressionHead::BitwiseORExpression(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl CoalesceExpressionHead {
    // Note that CoalesceExpression and CoalesceExpressionHead interact tightly. The implementation for parsing them
    // together is far simpler than giving each its own parse routine. So there's no independent implementation for
    // CoalesceExpressionHead here; look to CoalesceExpression to find the bundle.
}

// ShortCircuitExpression[In, Yield, Await] :
//      LogicalORExpression[?In, ?Yield, ?Await]
//      CoalesceExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ShortCircuitExpression {
    LogicalORExpression(Box<LogicalORExpression>),
    CoalesceExpression(Box<CoalesceExpression>),
}

impl fmt::Display for ShortCircuitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ShortCircuitExpression::LogicalORExpression(node) => node.fmt(f),
            ShortCircuitExpression::CoalesceExpression(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for ShortCircuitExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ShortCircuitExpression: {}", first, self)?;
        match &self {
            ShortCircuitExpression::LogicalORExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ShortCircuitExpression::CoalesceExpression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ShortCircuitExpression::LogicalORExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ShortCircuitExpression::CoalesceExpression(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl IsFunctionDefinition for ShortCircuitExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            ShortCircuitExpression::CoalesceExpression(_) => false,
            ShortCircuitExpression::LogicalORExpression(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ShortCircuitExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            ShortCircuitExpression::CoalesceExpression(_) => ATTKind::Invalid,
            ShortCircuitExpression::LogicalORExpression(node) => node.assignment_target_type(),
        }
    }
}

impl ShortCircuitExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("Improper Expression", scanner.line, scanner.column))
            .otherwise(|| {
                CoalesceExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(coal, after_coal)| (Box::new(ShortCircuitExpression::CoalesceExpression(coal)), after_coal))
            })
            .otherwise(|| {
                LogicalORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(lor, after_lor)| (Box::new(ShortCircuitExpression::LogicalORExpression(lor)), after_lor))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // LOGICAL AND EXPRESSION
    #[test]
    fn logical_and_expression_test_01() {
        let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(_)));
        pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn logical_and_expression_test_02() {
        let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a&&b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pn, LogicalANDExpression::LogicalAND(..)));
        pretty_check(&*pn, "LogicalANDExpression: a && b", vec!["LogicalANDExpression: a", "BitwiseORExpression: b"]);
        concise_check(&*pn, "LogicalANDExpression: a && b", vec!["IdentifierName: a", "Punctuator: &&", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn logical_and_expression_test_03() {
        let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a&&"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(..)));
        pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn logical_and_expression_test_prettyerrors_1() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn logical_and_expression_test_prettyerrors_2() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn logical_and_expression_test_conciseerrors_1() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn logical_and_expression_test_conciseerrors_2() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // LOGICAL OR EXPRESSION
    #[test]
    fn logical_or_expression_test_01() {
        let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(_)));
        pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn logical_or_expression_test_02() {
        let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a||b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pn, LogicalORExpression::LogicalOR(..)));
        pretty_check(&*pn, "LogicalORExpression: a || b", vec!["LogicalORExpression: a", "LogicalANDExpression: b"]);
        concise_check(&*pn, "LogicalORExpression: a || b", vec!["IdentifierName: a", "Punctuator: ||", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn logical_or_expression_test_03() {
        let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a||"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(..)));
        pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn logical_or_expression_test_prettyerrors_1() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn logical_or_expression_test_prettyerrors_2() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn logical_or_expression_test_conciseerrors_1() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn logical_or_expression_test_conciseerrors_2() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // COALESCE EXPRESSION
    #[test]
    fn coalesce_expression_test_01() {
        let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        pretty_check(&*pn, "CoalesceExpression: a ?? b", vec!["CoalesceExpressionHead: a", "BitwiseORExpression: b"]);
        concise_check(&*pn, "CoalesceExpression: a ?? b", vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"]);
        format!("{:?}", pn);
    }
    #[test]
    fn coalesce_expression_test_02() {
        let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 7);
        pretty_check(&*pn, "CoalesceExpression: z ?? a ?? b", vec!["CoalesceExpressionHead: z ?? a", "BitwiseORExpression: b"]);
        concise_check(&*pn, "CoalesceExpression: z ?? a ?? b", vec!["CoalesceExpression: z ?? a", "Punctuator: ??", "IdentifierName: b"]);
        format!("{:?}", pn);
    }
    #[test]
    fn coalesce_expression_test_03() {
        check_err(CoalesceExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn coalesce_expression_test_04() {
        check_err(CoalesceExpression::parse(&mut newparser("a??"), Scanner::new(), true, false, false), "Invalid Coalesce Expression", 1, 1);
    }
    #[test]
    fn coalesce_expression_test_prettyerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn coalesce_expression_test_conciseerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // COALESCE EXPRESSION HEAD
    #[test]
    fn coalesce_expression_head_test_01() {
        let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
        let head = &pn.head;
        pretty_check(&**head, "CoalesceExpressionHead: a", vec!["BitwiseORExpression: a"]);
        concise_check(&**head, "IdentifierName: a", vec![]);
    }
    #[test]
    fn coalesce_expression_head_test_02() {
        let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
        let head = &pn.head;
        pretty_check(&**head, "CoalesceExpressionHead: z ?? a", vec!["CoalesceExpression: z ?? a"]);
        concise_check(&**head, "CoalesceExpression: z ?? a", vec!["IdentifierName: z", "Punctuator: ??", "IdentifierName: a"]);
    }
    #[test]
    fn coalesce_expression_head_test_prettyerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item.head);
    }
    #[test]
    fn coalesce_expression_head_test_prettyerrors_2() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item.head);
    }
    #[test]
    fn coalesce_expression_head_test_conciseerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item.head);
    }
    #[test]
    fn coalesce_expression_head_test_conciseerrors_2() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item.head);
    }

    // SHORT CIRCUIT EXPRESSION
    #[test]
    fn short_circuit_expression_test_01() {
        let (pn, scanner) = check(ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pn, ShortCircuitExpression::CoalesceExpression(..)));
        pretty_check(&*pn, "ShortCircuitExpression: a ?? b", vec!["CoalesceExpression: a ?? b"]);
        concise_check(&*pn, "CoalesceExpression: a ?? b", vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn short_circuit_expression_test_02() {
        let (pn, scanner) = check(ShortCircuitExpression::parse(&mut newparser("6"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, ShortCircuitExpression::LogicalORExpression(..)));
        pretty_check(&*pn, "ShortCircuitExpression: 6", vec!["LogicalORExpression: 6"]);
        concise_check(&*pn, "Numeric: 6", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn short_circuit_expression_test_03() {
        check_err(ShortCircuitExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "Improper Expression", 1, 1);
    }
    #[test]
    fn short_circuit_expression_test_prettyerrors_1() {
        let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn short_circuit_expression_test_prettyerrors_2() {
        let (item, _) = ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn short_circuit_expression_test_conciseerrors_1() {
        let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn short_circuit_expression_test_conciseerrors_2() {
        let (item, _) = ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
}

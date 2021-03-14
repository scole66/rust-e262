use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::equality_operators::EqualityExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// BitwiseANDExpression[In, Yield, Await] :
//      EqualityExpression[?In, ?Yield, ?Await]
//      BitwiseANDExpression[?In, ?Yield, ?Await] & EqualityExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum BitwiseANDExpression {
    EqualityExpression(Rc<EqualityExpression>),
    BitwiseAND(Rc<BitwiseANDExpression>, Rc<EqualityExpression>),
}

impl fmt::Display for BitwiseANDExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BitwiseANDExpression::EqualityExpression(ee) => write!(f, "{}", ee),
            BitwiseANDExpression::BitwiseAND(be, ee) => write!(f, "{} & {}", be, ee),
        }
    }
}

impl PrettyPrint for BitwiseANDExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BitwiseANDExpression: {}", first, self)?;
        match &self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.pprint_with_leftpad(writer, &successive, Spot::Final),
            BitwiseANDExpression::BitwiseAND(be, ee) => {
                be.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BitwiseANDExpression::EqualityExpression(node) => node.concise_with_leftpad(writer, pad, state),
            BitwiseANDExpression::BitwiseAND(be, ee) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BitwiseANDExpression: {}", first, self)?;
                be.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "&", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                ee.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for BitwiseANDExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for BitwiseANDExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl BitwiseANDExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        EqualityExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(ee1, after_ee1)| {
            let mut current = Rc::new(BitwiseANDExpression::EqualityExpression(ee1));
            let mut current_scanner = after_ee1;
            while let Ok((ee2, after_ee2)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Amp)
                .and_then(|after_op| EqualityExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Rc::new(BitwiseANDExpression::BitwiseAND(current, ee2));
                current_scanner = after_ee2;
            }
            (current, current_scanner)
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.bitwise_and_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.bitwise_and_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// BitwiseXORExpression[In, Yield, Await] :
//      BitwiseANDExpression[?In, ?Yield, ?Await]
//      BitwiseXORExpression[?In, ?Yield, ?Await] ^ BitwiseANDExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum BitwiseXORExpression {
    BitwiseANDExpression(Rc<BitwiseANDExpression>),
    BitwiseXOR(Rc<BitwiseXORExpression>, Rc<BitwiseANDExpression>),
}

impl fmt::Display for BitwiseXORExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BitwiseXORExpression::BitwiseANDExpression(band) => write!(f, "{}", band),
            BitwiseXORExpression::BitwiseXOR(bxor, band) => write!(f, "{} ^ {}", bxor, band),
        }
    }
}

impl PrettyPrint for BitwiseXORExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BitwiseXORExpression: {}", first, self)?;
        match &self {
            BitwiseXORExpression::BitwiseANDExpression(band) => band.pprint_with_leftpad(writer, &successive, Spot::Final),
            BitwiseXORExpression::BitwiseXOR(bxor, band) => {
                bxor.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                band.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(node) => node.concise_with_leftpad(writer, pad, state),
            BitwiseXORExpression::BitwiseXOR(bxor, band) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BitwiseXORExpression: {}", first, self)?;
                bxor.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "^", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                band.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for BitwiseXORExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(band) => band.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for BitwiseXORExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(band) => band.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl BitwiseXORExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        BitwiseANDExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(band1, after_band1)| {
            let mut current = Rc::new(BitwiseXORExpression::BitwiseANDExpression(band1));
            let mut current_scanner = after_band1;
            while let Ok((band2, after_band2)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Caret)
                .and_then(|after_op| BitwiseANDExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Rc::new(BitwiseXORExpression::BitwiseXOR(current, band2));
                current_scanner = after_band2;
            }
            (current, current_scanner)
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.bitwise_xor_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.bitwise_xor_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// BitwiseORExpression[In, Yield, Await] :
//      BitwiseXORExpression[?In, ?Yield, ?Await]
//      BitwiseORExpression[?In, ?Yield, ?Await] | BitwiseXORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum BitwiseORExpression {
    BitwiseXORExpression(Rc<BitwiseXORExpression>),
    BitwiseOR(Rc<BitwiseORExpression>, Rc<BitwiseXORExpression>),
}

impl fmt::Display for BitwiseORExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BitwiseORExpression::BitwiseXORExpression(bxor) => write!(f, "{}", bxor),
            BitwiseORExpression::BitwiseOR(bor, bxor) => write!(f, "{} | {}", bor, bxor),
        }
    }
}

impl PrettyPrint for BitwiseORExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BitwiseORExpression: {}", first, self)?;
        match &self {
            BitwiseORExpression::BitwiseXORExpression(bxor) => bxor.pprint_with_leftpad(writer, &successive, Spot::Final),
            BitwiseORExpression::BitwiseOR(bor, bxor) => {
                bor.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                bxor.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BitwiseORExpression::BitwiseXORExpression(node) => node.concise_with_leftpad(writer, pad, state),
            BitwiseORExpression::BitwiseOR(bor, bxor) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BitwiseORExpression: {}", first, self)?;
                bor.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "|", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                bxor.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for BitwiseORExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            BitwiseORExpression::BitwiseXORExpression(bxor) => bxor.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for BitwiseORExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            BitwiseORExpression::BitwiseXORExpression(bxor) => bxor.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl BitwiseORExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        BitwiseXORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(bxor1, after_bxor1)| {
            let mut current = Rc::new(BitwiseORExpression::BitwiseXORExpression(bxor1));
            let mut current_scanner = after_bxor1;
            while let Ok((bxor2, after_bxor2)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Pipe)
                .and_then(|after_op| BitwiseXORExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Rc::new(BitwiseORExpression::BitwiseOR(current, bxor2));
                current_scanner = after_bxor2;
            }
            (current, current_scanner)
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.bitwise_or_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.bitwise_or_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    #[test]
    fn bitwise_and_expression_test_01() {
        let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
        pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_and_expression_test_02() {
        let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a&b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, BitwiseANDExpression::BitwiseAND(_, _)));
        pretty_check(&*pn, "BitwiseANDExpression: a & b", vec!["BitwiseANDExpression: a", "EqualityExpression: b"]);
        concise_check(&*pn, "BitwiseANDExpression: a & b", vec!["IdentifierName: a", "Punctuator: &", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn bitwise_and_expression_test_03() {
        let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a&@"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
        pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_and_expression_test_04() {
        check_err(BitwiseANDExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn bitwise_and_expression_test_prettyerrors() {
        let (item, _) = BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn bitwise_and_expression_test_conciseerrors() {
        let (item, _) = BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test]
    fn bitwise_xor_expression_test_01() {
        let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseXORExpression::BitwiseANDExpression(_)));
        pretty_check(&*pn, "BitwiseXORExpression: a", vec!["BitwiseANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_xor_expression_test_02() {
        let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a^b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, BitwiseXORExpression::BitwiseXOR(_, _)));
        pretty_check(&*pn, "BitwiseXORExpression: a ^ b", vec!["BitwiseXORExpression: a", "BitwiseANDExpression: b"]);
        concise_check(&*pn, "BitwiseXORExpression: a ^ b", vec!["IdentifierName: a", "Punctuator: ^", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn bitwise_xor_expression_test_03() {
        let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a^@"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseXORExpression::BitwiseANDExpression(_)));
        pretty_check(&*pn, "BitwiseXORExpression: a", vec!["BitwiseANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_xor_expression_test_04() {
        check_err(BitwiseXORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn bitwise_xor_expression_test_prettyerrors() {
        let (item, _) = BitwiseXORExpression::parse(&mut newparser("a ^ b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn bitwise_xor_expression_test_conciseerrors() {
        let (item, _) = BitwiseXORExpression::parse(&mut newparser("a ^ b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test]
    fn bitwise_or_expression_test_01() {
        let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseORExpression::BitwiseXORExpression(_)));
        pretty_check(&*pn, "BitwiseORExpression: a", vec!["BitwiseXORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_or_expression_test_02() {
        let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a|b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, BitwiseORExpression::BitwiseOR(_, _)));
        pretty_check(&*pn, "BitwiseORExpression: a | b", vec!["BitwiseORExpression: a", "BitwiseXORExpression: b"]);
        concise_check(&*pn, "BitwiseORExpression: a | b", vec!["IdentifierName: a", "Punctuator: |", "IdentifierName: b"]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn bitwise_or_expression_test_03() {
        let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a|@"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseORExpression::BitwiseXORExpression(_)));
        pretty_check(&*pn, "BitwiseORExpression: a", vec!["BitwiseXORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
        assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn bitwise_or_expression_test_04() {
        check_err(BitwiseORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn bitwise_or_expression_test_prettyerrors() {
        let (item, _) = BitwiseORExpression::parse(&mut newparser("a | b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn bitwise_or_expression_test_conciseerrors() {
        let (item, _) = BitwiseORExpression::parse(&mut newparser("a | b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
}

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::additive_operators::AdditiveExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ShiftExpression[Yield, Await] :
//      AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum ShiftExpression {
    AdditiveExpression(Rc<AdditiveExpression>),
    LeftShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    SignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    UnsignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
}

impl fmt::Display for ShiftExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShiftExpression::AdditiveExpression(ae) => write!(f, "{}", ae),
            ShiftExpression::LeftShift(se, ae) => write!(f, "{} << {}", se, ae),
            ShiftExpression::SignedRightShift(se, ae) => write!(f, "{} >> {}", se, ae),
            ShiftExpression::UnsignedRightShift(se, ae) => write!(f, "{} >>> {}", se, ae),
        }
    }
}

impl PrettyPrint for ShiftExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ShiftExpression: {}", first, self)?;
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
            ShiftExpression::LeftShift(se, ae) | ShiftExpression::SignedRightShift(se, ae) | ShiftExpression::UnsignedRightShift(se, ae) => {
                se.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |left: &ShiftExpression, right: &AdditiveExpression, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}ShiftExpression: {}", first, self)
                .and_then(|_| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal))
                .and_then(|_| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            ShiftExpression::AdditiveExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ShiftExpression::LeftShift(left, right) => work(left, right, "<<"),
            ShiftExpression::SignedRightShift(left, right) => work(left, right, ">>"),
            ShiftExpression::UnsignedRightShift(left, right) => work(left, right, ">>>"),
        }
    }
}

impl IsFunctionDefinition for ShiftExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for ShiftExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl ShiftExpression {
    // Only one parent. No need to cache.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        AdditiveExpression::parse(parser, scanner, yield_flag, await_flag).map(|(ae, after_ae)| {
            let mut current = Rc::new(ShiftExpression::AdditiveExpression(ae));
            let mut current_scan = after_ae;
            while let Ok((make_se, ae2, after_ae2)) = scan_for_punct_set(current_scan, parser.source, ScanGoal::InputElementDiv, &[Punctuator::GtGt, Punctuator::GtGtGt, Punctuator::LtLt])
                .and_then(|(shift_op, after_op)| {
                    let make_se = match shift_op {
                        Punctuator::GtGt => |se, ae| ShiftExpression::SignedRightShift(se, ae),
                        Punctuator::LtLt => |se, ae| ShiftExpression::LeftShift(se, ae),
                        _ => |se, ae| ShiftExpression::UnsignedRightShift(se, ae),
                    };
                    AdditiveExpression::parse(parser, after_op, yield_flag, await_flag).map(|(ae2, after_ae2)| (make_se, ae2, after_ae2))
                })
            {
                current = Rc::new(make_se(current, ae2));
                current_scan = after_ae2;
            }
            (current, current_scan)
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.contains(kind),
            ShiftExpression::LeftShift(l, r) => l.contains(kind) || r.contains(kind),
            ShiftExpression::SignedRightShift(l, r) => l.contains(kind) || r.contains(kind),
            ShiftExpression::UnsignedRightShift(l, r) => l.contains(kind) || r.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::relational_operators::RelationalExpression;
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// EqualityExpression[In, Yield, Await] :
//      RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] == RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] != RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] === RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] !== RelationalExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum EqualityExpression {
    RelationalExpression(Rc<RelationalExpression>),
    Equal(Rc<EqualityExpression>, Rc<RelationalExpression>),
    NotEqual(Rc<EqualityExpression>, Rc<RelationalExpression>),
    StrictEqual(Rc<EqualityExpression>, Rc<RelationalExpression>),
    NotStrictEqual(Rc<EqualityExpression>, Rc<RelationalExpression>),
}

impl fmt::Display for EqualityExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EqualityExpression::RelationalExpression(re) => write!(f, "{}", re),
            EqualityExpression::Equal(ee, re) => write!(f, "{} == {}", ee, re),
            EqualityExpression::NotEqual(ee, re) => write!(f, "{} != {}", ee, re),
            EqualityExpression::StrictEqual(ee, re) => write!(f, "{} === {}", ee, re),
            EqualityExpression::NotStrictEqual(ee, re) => write!(f, "{} !== {}", ee, re),
        }
    }
}

impl PrettyPrint for EqualityExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}EqualityExpression: {}", first, self)?;
        match &self {
            EqualityExpression::RelationalExpression(re) => re.pprint_with_leftpad(writer, &successive, Spot::Final),
            EqualityExpression::Equal(ee, re) | EqualityExpression::NotEqual(ee, re) | EqualityExpression::StrictEqual(ee, re) | EqualityExpression::NotStrictEqual(ee, re) => {
                ee.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                re.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |ee: &EqualityExpression, re: &RelationalExpression, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}EqualityExpression: {}", first, self)
                .and_then(|_| ee.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal))
                .and_then(|_| re.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            EqualityExpression::RelationalExpression(node) => node.concise_with_leftpad(writer, pad, state),
            EqualityExpression::Equal(ee, re) => work(ee, re, "=="),
            EqualityExpression::NotEqual(ee, re) => work(ee, re, "!="),
            EqualityExpression::StrictEqual(ee, re) => work(ee, re, "==="),
            EqualityExpression::NotStrictEqual(ee, re) => work(ee, re, "!=="),
        }
    }
}

impl IsFunctionDefinition for EqualityExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            EqualityExpression::RelationalExpression(re) => re.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for EqualityExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            EqualityExpression::RelationalExpression(re) => re.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl EqualityExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (re1, after_re1) = RelationalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Rc::new(EqualityExpression::RelationalExpression(re1));
        let mut current_scanner = after_re1;
        loop {
            let (op_token, after_op) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
            let make_ee = match op_token {
                Token::Punctuator(Punctuator::EqEq) => |ee, re| EqualityExpression::Equal(ee, re),
                Token::Punctuator(Punctuator::BangEq) => |ee, re| EqualityExpression::NotEqual(ee, re),
                Token::Punctuator(Punctuator::EqEqEq) => |ee, re| EqualityExpression::StrictEqual(ee, re),
                Token::Punctuator(Punctuator::BangEqEq) => |ee, re| EqualityExpression::NotStrictEqual(ee, re),
                _ => {
                    break;
                }
            };
            let pot_re2 = RelationalExpression::parse(parser, after_op, in_flag, yield_flag, await_flag);
            match pot_re2 {
                Err(_) => {
                    break;
                }
                Ok((re2, after_re2)) => {
                    current = Rc::new(make_ee(current, re2));
                    current_scanner = after_re2;
                }
            }
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            EqualityExpression::RelationalExpression(n) => n.contains(kind),
            EqualityExpression::Equal(l, r) => l.contains(kind) || r.contains(kind),
            EqualityExpression::NotEqual(l, r) => l.contains(kind) || r.contains(kind),
            EqualityExpression::StrictEqual(l, r) => l.contains(kind) || r.contains(kind),
            EqualityExpression::NotStrictEqual(l, r) => l.contains(kind) || r.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::relational_operators::RelationalExpression;
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, StringToken, Token};
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
                Token::Punctuator(Punctuator::EqEq) => EqualityExpression::Equal,
                Token::Punctuator(Punctuator::BangEq) => EqualityExpression::NotEqual,
                Token::Punctuator(Punctuator::EqEqEq) => EqualityExpression::StrictEqual,
                Token::Punctuator(Punctuator::BangEqEq) => EqualityExpression::NotStrictEqual,
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

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            EqualityExpression::RelationalExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            EqualityExpression::RelationalExpression(n) => n.all_private_identifiers_valid(names),
            EqualityExpression::Equal(l, r) | EqualityExpression::NotEqual(l, r) | EqualityExpression::StrictEqual(l, r) | EqualityExpression::NotStrictEqual(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _strict: bool) -> Vec<Object> {
        todo!()
    }
}

#[cfg(test)]
mod tests;

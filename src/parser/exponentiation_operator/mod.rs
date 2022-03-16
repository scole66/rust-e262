use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::unary_operators::UnaryExpression;
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ExponentiationExpression[Yield, Await] :
//      UnaryExpression[?Yield, ?Await]
//      UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum ExponentiationExpression {
    UnaryExpression(Rc<UnaryExpression>),
    Exponentiation(Rc<UpdateExpression>, Rc<ExponentiationExpression>),
}

impl fmt::Display for ExponentiationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => write!(f, "{}", boxed),
            ExponentiationExpression::Exponentiation(ue, ee) => write!(f, "{} ** {}", ue, ee),
        }
    }
}

impl PrettyPrint for ExponentiationExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ExponentiationExpression: {}", first, self)?;
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ExponentiationExpression::Exponentiation(ue, ee) => {
                ue.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ExponentiationExpression::UnaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ExponentiationExpression::Exponentiation(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ExponentiationExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "**", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for ExponentiationExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            ExponentiationExpression::Exponentiation(..) => false,
            ExponentiationExpression::UnaryExpression(ue) => ue.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ExponentiationExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            ExponentiationExpression::Exponentiation(..) => ATTKind::Invalid,
            ExponentiationExpression::UnaryExpression(ue) => ue.assignment_target_type(),
        }
    }
}

impl ExponentiationExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ExponentiationExpression), scanner))
            .otherwise(|| {
                let (ue, after_ue) = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
                let after_op = scan_for_punct(after_ue, parser.source, ScanGoal::InputElementDiv, Punctuator::StarStar)?;
                let (ee, after_ee) = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::Exponentiation(ue, ee)), after_ee))
            })
            .otherwise(|| {
                let (unary, after_unary) = UnaryExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::UnaryExpression(unary)), after_unary))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.contains(kind),
            ExponentiationExpression::Exponentiation(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.as_string_literal(),
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
            ExponentiationExpression::UnaryExpression(n) => n.all_private_identifiers_valid(names),
            ExponentiationExpression::Exponentiation(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.early_errors(agent, errs, strict),
            ExponentiationExpression::Exponentiation(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            ExponentiationExpression::UnaryExpression(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests;

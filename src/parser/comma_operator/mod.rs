use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// Expression[In, Yield, Await] :
//      AssignmentExpression[?In, ?Yield, ?Await]
//      Expression[?In, ?Yield, ?Await] , AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Expression {
    FallThru(Rc<AssignmentExpression>),
    Comma(Rc<Expression>, Rc<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::FallThru(node) => node.fmt(f),
            Expression::Comma(left, right) => write!(f, "{} , {}", left, right),
        }
    }
}

impl PrettyPrint for Expression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Expression: {}", first, self)?;
        match &self {
            Expression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Expression::Comma(left, right) => {
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
            Expression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            Expression::Comma(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}Expression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for Expression {
    fn is_function_definition(&self) -> bool {
        match &self {
            Expression::FallThru(node) => node.is_function_definition(),
            Expression::Comma(_, _) => false,
        }
    }
}

impl AssignmentTargetType for Expression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            Expression::FallThru(node) => node.assignment_target_type(),
            Expression::Comma(_, _) => ATTKind::Invalid,
        }
    }
}

impl Expression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::Expression), scanner)).otherwise(|| {
            AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
                let mut current = Rc::new(Expression::FallThru(left));
                let mut current_scanner = after_left;
                while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                    .and_then(|after_token| AssignmentExpression::parse(parser, after_token, in_flag, yield_flag, await_flag))
                {
                    current = Rc::new(Expression::Comma(current, right));
                    current_scanner = after_right;
                }
                (current, current_scanner)
            })
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Expression::FallThru(node) => node.contains(kind),
            Expression::Comma(left, right) => left.contains(kind) || right.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            Expression::FallThru(node) => node.as_string_literal(),
            Expression::Comma(..) => None,
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
            Expression::FallThru(node) => node.all_private_identifiers_valid(names),
            Expression::Comma(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            Expression::FallThru(ae) => ae.contains_arguments(),
            Expression::Comma(e, ae) => e.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            Expression::FallThru(node) => node.early_errors(agent, errs, strict),
            Expression::Comma(left, right) => {
                left.early_errors(agent, errs, strict);
                right.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            Expression::FallThru(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests;

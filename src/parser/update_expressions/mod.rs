use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// UpdateExpression[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] ++
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] --
//      ++ UnaryExpression[?Yield, ?Await]
//      -- UnaryExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum UpdateExpression {
    LeftHandSideExpression(Rc<LeftHandSideExpression>),
    PostIncrement(Rc<LeftHandSideExpression>),
    PostDecrement(Rc<LeftHandSideExpression>),
    PreIncrement(Rc<UnaryExpression>),
    PreDecrement(Rc<UnaryExpression>),
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
        let workafter = |writer: &mut T, node: &LeftHandSideExpression, op: &str| {
            head(writer)
                .and_then(|successive| node.concise_with_leftpad(writer, &successive, Spot::NotFinal).and_then(|_| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::Final)))
        };
        let workbefore = |writer: &mut T, node: &UnaryExpression, op: &str| {
            head(writer)
                .and_then(|successive| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal).and_then(|_| node.concise_with_leftpad(writer, &successive, Spot::Final)))
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("UpdateExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_plusses = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::PlusPlus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_plusses, yield_flag, await_flag)?;
                Ok((Rc::new(UpdateExpression::PreIncrement(ue)), after_ue))
            })
            .otherwise(|| {
                let after_minuses = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::MinusMinus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_minuses, yield_flag, await_flag)?;
                Ok((Rc::new(UpdateExpression::PreDecrement(ue)), after_ue))
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
                            _ => Ok((AftLHS::Dec, after_punct)),
                        }
                    })
                    .otherwise(|| Ok((AftLHS::Nothing, after_lhs)))
                    .map(|(aft, scan)| {
                        (
                            Rc::new(match aft {
                                AftLHS::Nothing => UpdateExpression::LeftHandSideExpression(lhs),
                                AftLHS::Inc => UpdateExpression::PostIncrement(lhs),
                                AftLHS::Dec => UpdateExpression::PostDecrement(lhs),
                            }),
                            scan,
                        )
                    })
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.update_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.update_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(n) => n.contains(kind),
            UpdateExpression::PostIncrement(n) => n.contains(kind),
            UpdateExpression::PostDecrement(n) => n.contains(kind),
            UpdateExpression::PreIncrement(n) => n.contains(kind),
            UpdateExpression::PreDecrement(n) => n.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            UpdateExpression::LeftHandSideExpression(n) => n.as_string_literal(),
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
            UpdateExpression::LeftHandSideExpression(n) | UpdateExpression::PostIncrement(n) | UpdateExpression::PostDecrement(n) => n.all_private_identifiers_valid(names),
            UpdateExpression::PreIncrement(n) | UpdateExpression::PreDecrement(n) => n.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            UpdateExpression::LeftHandSideExpression(n) => n.early_errors(agent, errs, strict),
            UpdateExpression::PostIncrement(n) | UpdateExpression::PostDecrement(n) => {
                //  UpdateExpression :
                //      LeftHandSideExpression ++
                //      LeftHandSideExpression --
                // * It is an early Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
                if n.assignment_target_type() != ATTKind::Simple {
                    errs.push(create_syntax_error_object(agent, "Invalid target for update"));
                }
                n.early_errors(agent, errs, strict);
            }
            UpdateExpression::PreIncrement(n) | UpdateExpression::PreDecrement(n) => {
                //  UpdateExpression :
                //      ++ UnaryExpression
                //      -- UnaryExpression
                // * It is an early Syntax Error if AssignmentTargetType of UnaryExpression is not simple.
                if n.assignment_target_type() != ATTKind::Simple {
                    errs.push(create_syntax_error_object(agent, "Invalid target for update"));
                }
                n.early_errors(agent, errs, strict);
            }
        }
    }
}

#[cfg(test)]
mod tests;

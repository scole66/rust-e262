use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::exponentiation_operator::ExponentiationExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// MultiplicativeOperator : one of
//      * / %
#[derive(Debug)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

impl fmt::Display for MultiplicativeOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            MultiplicativeOperator::Multiply => write!(f, "*"),
            MultiplicativeOperator::Divide => write!(f, "/"),
            MultiplicativeOperator::Modulo => write!(f, "%"),
        }
    }
}

impl PrettyPrint for MultiplicativeOperator {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}MultiplicativeOperator: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, self, TokenType::Punctuator, pad, state)
    }
}

impl MultiplicativeOperator {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Rc<MultiplicativeOperator>, Scanner), ParseError> {
        let (op, after_op) = scan_for_punct_set(scanner, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Star, Punctuator::Slash, Punctuator::Percent])?;
        match op {
            Punctuator::Star => Ok((Rc::new(MultiplicativeOperator::Multiply), after_op)),
            Punctuator::Slash => Ok((Rc::new(MultiplicativeOperator::Divide), after_op)),
            _ => Ok((Rc::new(MultiplicativeOperator::Modulo), after_op)),
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// MultiplicativeExpression[Yield, Await] :
//      ExponentiationExpression[?Yield, ?Await]
//      MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum MultiplicativeExpression {
    ExponentiationExpression(Rc<ExponentiationExpression>),
    MultiplicativeExpressionExponentiationExpression(Rc<MultiplicativeExpression>, Rc<MultiplicativeOperator>, Rc<ExponentiationExpression>),
}

impl fmt::Display for MultiplicativeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            MultiplicativeExpression::ExponentiationExpression(boxed) => write!(f, "{}", boxed),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                write!(f, "{} {} {}", me, mo, ee)
            }
        }
    }
}

impl IsFunctionDefinition for MultiplicativeExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..) => false,
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for MultiplicativeExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..) => ATTKind::Invalid,
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.assignment_target_type(),
        }
    }
}

impl PrettyPrint for MultiplicativeExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MultiplicativeExpression: {}", first, self)?;
        match &self {
            MultiplicativeExpression::ExponentiationExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                mo.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            MultiplicativeExpression::ExponentiationExpression(node) => node.concise_with_leftpad(writer, pad, state),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}MultiplicativeExpression: {}", first, self)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                mo.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl MultiplicativeExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (ee, after_ee) = ExponentiationExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(MultiplicativeExpression::ExponentiationExpression(ee));
        let mut current_scanner = after_ee;
        while let Ok((op, ee2, scan)) = MultiplicativeOperator::parse(parser, current_scanner).and_then(|(op, after_op)| {
            let (ee2, after_ee2) = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
            Ok((op, ee2, after_ee2))
        }) {
            current = Rc::new(MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(current, op, ee2));
            current_scanner = scan;
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MultiplicativeExpression::ExponentiationExpression(n) => n.contains(kind),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(l, op, r) => l.contains(kind) || op.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            MultiplicativeExpression::ExponentiationExpression(n) => n.as_string_literal(),
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
            MultiplicativeExpression::ExponentiationExpression(n) => n.all_private_identifiers_valid(names),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(l, _, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _strict: bool) -> Vec<Object> {
        todo!()
    }
}

#[cfg(test)]
mod tests;

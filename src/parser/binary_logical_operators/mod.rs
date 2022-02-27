use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::binary_bitwise_operators::BitwiseORExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LogicalANDExpression[In, Yield, Await] :
//      BitwiseORExpression[?In, ?Yield, ?Await]
//      LogicalANDExpression[?In, ?Yield, ?Await] && BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LogicalANDExpression {
    BitwiseORExpression(Rc<BitwiseORExpression>),
    LogicalAND(Rc<LogicalANDExpression>, Rc<BitwiseORExpression>),
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
    // No need to cache. Only one parent.
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        BitwiseORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
            let mut current = Rc::new(LogicalANDExpression::BitwiseORExpression(left));
            let mut current_scanner = after_left;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::AmpAmp)
                .and_then(|after_op| BitwiseORExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Rc::new(LogicalANDExpression::LogicalAND(current, right));
                current_scanner = after_right;
            }
            (current, current_scanner)
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LogicalANDExpression::BitwiseORExpression(n) => n.contains(kind),
            LogicalANDExpression::LogicalAND(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            LogicalANDExpression::BitwiseORExpression(n) => n.as_string_literal(),
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
            LogicalANDExpression::BitwiseORExpression(n) => n.all_private_identifiers_valid(names),
            LogicalANDExpression::LogicalAND(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

// LogicalORExpression[In, Yield, Await] :
//      LogicalANDExpression[?In, ?Yield, ?Await]
//      LogicalORExpression[?In, ?Yield, ?Await] || LogicalANDExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LogicalORExpression {
    LogicalANDExpression(Rc<LogicalANDExpression>),
    LogicalOR(Rc<LogicalORExpression>, Rc<LogicalANDExpression>),
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
    // Only one parent, no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        LogicalANDExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
            let mut current = Rc::new(LogicalORExpression::LogicalANDExpression(left));
            let mut current_scanner = after_left;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::PipePipe)
                .and_then(|after_op| LogicalANDExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current = Rc::new(LogicalORExpression::LogicalOR(current, right));
                current_scanner = after_right;
            }
            (current, current_scanner)
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LogicalORExpression::LogicalANDExpression(n) => n.contains(kind),
            LogicalORExpression::LogicalOR(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            LogicalORExpression::LogicalANDExpression(n) => n.as_string_literal(),
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
            LogicalORExpression::LogicalANDExpression(n) => n.all_private_identifiers_valid(names),
            LogicalORExpression::LogicalOR(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

// CoalesceExpression[In, Yield, Await] :
//      CoalesceExpressionHead[?In, ?Yield, ?Await] ?? BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub struct CoalesceExpression {
    head: Rc<CoalesceExpressionHead>,
    tail: Rc<BitwiseORExpression>,
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        BitwiseORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(left, after_left)| {
            let mut current_head = CoalesceExpressionHead::BitwiseORExpression(left);
            let mut current_scanner = after_left;
            let mut exp_scanner = scanner;
            while let Ok((right, after_right)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::QQ)
                .and_then(|after_op| BitwiseORExpression::parse(parser, after_op, in_flag, yield_flag, await_flag))
            {
                current_head = CoalesceExpressionHead::CoalesceExpression(Rc::new(CoalesceExpression { head: Rc::new(current_head), tail: right }));
                exp_scanner = after_right;
                current_scanner = after_right;
            }
            match current_head {
                CoalesceExpressionHead::BitwiseORExpression(_) => Err(ParseError::new(PECode::InvalidCoalesceExpression, scanner)),
                CoalesceExpressionHead::CoalesceExpression(exp) => Ok((exp, exp_scanner)),
            }
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.coalesce_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.coalesce_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.head.contains(kind) || self.tail.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.head.all_private_identifiers_valid(names) && self.tail.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

// CoalesceExpressionHead[In, Yield, Await] :
//      CoalesceExpression[?In, ?Yield, ?Await]
//      BitwiseORExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum CoalesceExpressionHead {
    CoalesceExpression(Rc<CoalesceExpression>),
    BitwiseORExpression(Rc<BitwiseORExpression>),
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CoalesceExpressionHead::CoalesceExpression(n) => n.contains(kind),
            CoalesceExpressionHead::BitwiseORExpression(n) => n.contains(kind),
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
            CoalesceExpressionHead::CoalesceExpression(n) => n.all_private_identifiers_valid(names),
            CoalesceExpressionHead::BitwiseORExpression(n) => n.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

// ShortCircuitExpression[In, Yield, Await] :
//      LogicalORExpression[?In, ?Yield, ?Await]
//      CoalesceExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ShortCircuitExpression {
    LogicalORExpression(Rc<LogicalORExpression>),
    CoalesceExpression(Rc<CoalesceExpression>),
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
    // No need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ImproperExpression, scanner))
            .otherwise(|| {
                CoalesceExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(coal, after_coal)| (Rc::new(ShortCircuitExpression::CoalesceExpression(coal)), after_coal))
            })
            .otherwise(|| {
                LogicalORExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(lor, after_lor)| (Rc::new(ShortCircuitExpression::LogicalORExpression(lor)), after_lor))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ShortCircuitExpression::LogicalORExpression(n) => n.contains(kind),
            ShortCircuitExpression::CoalesceExpression(n) => n.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ShortCircuitExpression::LogicalORExpression(n) => n.as_string_literal(),
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
            ShortCircuitExpression::LogicalORExpression(n) => n.all_private_identifiers_valid(names),
            ShortCircuitExpression::CoalesceExpression(n) => n.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;

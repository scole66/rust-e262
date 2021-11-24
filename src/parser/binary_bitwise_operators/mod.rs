use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::equality_operators::EqualityExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
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
    // No caching needed. Only one parent.
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BitwiseANDExpression::EqualityExpression(n) => n.contains(kind),
            BitwiseANDExpression::BitwiseAND(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            BitwiseANDExpression::EqualityExpression(n) => n.as_string_literal(),
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
            BitwiseANDExpression::EqualityExpression(n) => n.all_private_identifiers_valid(names),
            BitwiseANDExpression::BitwiseAND(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _strict: bool) -> Vec<Object> {
        todo!()
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
    // Only one parent. No need to cache.
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(n) => n.contains(kind),
            BitwiseXORExpression::BitwiseXOR(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(n) => n.as_string_literal(),
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
            BitwiseXORExpression::BitwiseANDExpression(n) => n.all_private_identifiers_valid(names),
            BitwiseXORExpression::BitwiseXOR(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _strict: bool) -> Vec<Object> {
        todo!()
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BitwiseORExpression::BitwiseXORExpression(n) => n.contains(kind),
            BitwiseORExpression::BitwiseOR(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            BitwiseORExpression::BitwiseXORExpression(n) => n.as_string_literal(),
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
            BitwiseORExpression::BitwiseXORExpression(n) => n.all_private_identifiers_valid(names),
            BitwiseORExpression::BitwiseOR(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _strict: bool) -> Vec<Object> {
        todo!()
    }
}

#[cfg(test)]
mod tests;

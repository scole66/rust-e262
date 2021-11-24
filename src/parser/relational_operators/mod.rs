use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::bitwise_shift_operators::ShiftExpression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, StringToken, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// RelationalExpression[In, Yield, Await] :
//      ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] < ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] > ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] <= ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] >= ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] instanceof ShiftExpression[?Yield, ?Await]
//      [+In] RelationalExpression[+In, ?Yield, ?Await] in ShiftExpression[?Yield, ?Await]
//      [+In] PrivateIdentifier in ShiftExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum RelationalExpression {
    ShiftExpression(Rc<ShiftExpression>),
    Less(Rc<RelationalExpression>, Rc<ShiftExpression>),
    Greater(Rc<RelationalExpression>, Rc<ShiftExpression>),
    LessEqual(Rc<RelationalExpression>, Rc<ShiftExpression>),
    GreaterEqual(Rc<RelationalExpression>, Rc<ShiftExpression>),
    InstanceOf(Rc<RelationalExpression>, Rc<ShiftExpression>),
    In(Rc<RelationalExpression>, Rc<ShiftExpression>),
    PrivateIn(IdentifierData, Rc<ShiftExpression>),
}

impl fmt::Display for RelationalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationalExpression::ShiftExpression(se) => write!(f, "{}", se),
            RelationalExpression::Less(re, se) => write!(f, "{} < {}", re, se),
            RelationalExpression::Greater(re, se) => write!(f, "{} > {}", re, se),
            RelationalExpression::LessEqual(re, se) => write!(f, "{} <= {}", re, se),
            RelationalExpression::GreaterEqual(re, se) => write!(f, "{} >= {}", re, se),
            RelationalExpression::InstanceOf(re, se) => write!(f, "{} instanceof {}", re, se),
            RelationalExpression::In(re, se) => write!(f, "{} in {}", re, se),
            RelationalExpression::PrivateIn(id, se) => write!(f, "#{} in {}", id, se),
        }
    }
}

impl PrettyPrint for RelationalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}RelationalExpression: {}", first, self)?;
        match &self {
            RelationalExpression::ShiftExpression(se) => se.pprint_with_leftpad(writer, &successive, Spot::Final),
            RelationalExpression::Less(re, se)
            | RelationalExpression::Greater(re, se)
            | RelationalExpression::LessEqual(re, se)
            | RelationalExpression::GreaterEqual(re, se)
            | RelationalExpression::InstanceOf(re, se)
            | RelationalExpression::In(re, se) => {
                re.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                se.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            RelationalExpression::PrivateIn(_, se) => se.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |re: &RelationalExpression, se: &ShiftExpression, op, kind| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}RelationalExpression: {}", first, self)
                .and_then(|_| re.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, kind, &successive, Spot::NotFinal))
                .and_then(|_| se.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            RelationalExpression::ShiftExpression(node) => node.concise_with_leftpad(writer, pad, state),
            RelationalExpression::Less(re, se) => work(re, se, "<", TokenType::Punctuator),
            RelationalExpression::Greater(re, se) => work(re, se, ">", TokenType::Punctuator),
            RelationalExpression::LessEqual(re, se) => work(re, se, "<=", TokenType::Punctuator),
            RelationalExpression::GreaterEqual(re, se) => work(re, se, ">=", TokenType::Punctuator),
            RelationalExpression::InstanceOf(re, se) => work(re, se, "instanceof", TokenType::Keyword),
            RelationalExpression::In(re, se) => work(re, se, "in", TokenType::Keyword),
            RelationalExpression::PrivateIn(id, se) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}RelationalExpression: {}", first, self)?;
                pprint_token(writer, format!("#{}", id), TokenType::PrivateIdentifier, &successive, Spot::NotFinal)?;
                pprint_token(writer, "in", TokenType::Keyword, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for RelationalExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            RelationalExpression::ShiftExpression(se) => se.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for RelationalExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            RelationalExpression::ShiftExpression(se) => se.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl RelationalExpression {
    fn is_relational_token(tok: &Token, in_flag: bool) -> bool {
        tok.matches_punct(Punctuator::Lt)
            || tok.matches_punct(Punctuator::Gt)
            || tok.matches_punct(Punctuator::LtEq)
            || tok.matches_punct(Punctuator::GtEq)
            || tok.matches_keyword(Keyword::Instanceof)
            || (tok.matches_keyword(Keyword::In) && in_flag)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("RelationalExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                ShiftExpression::parse(parser, scanner, yield_flag, await_flag).map(|(se, after_se)| {
                    let mut current = Rc::new(RelationalExpression::ShiftExpression(se));
                    let mut current_scanner = after_se;
                    loop {
                        let (op, after_op) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
                        let make_re = match &op {
                            Token::Punctuator(Punctuator::Lt) => RelationalExpression::Less,
                            Token::Punctuator(Punctuator::Gt) => RelationalExpression::Greater,
                            Token::Punctuator(Punctuator::LtEq) => RelationalExpression::LessEqual,
                            Token::Punctuator(Punctuator::GtEq) => RelationalExpression::GreaterEqual,
                            Token::Identifier(id) if id.matches(Keyword::Instanceof) => RelationalExpression::InstanceOf,
                            _ => RelationalExpression::In,
                        };
                        if Self::is_relational_token(&op, in_flag) {
                            match ShiftExpression::parse(parser, after_op, yield_flag, await_flag) {
                                Err(_) => {
                                    break;
                                }
                                Ok((se2, after_se2)) => {
                                    current = Rc::new(make_re(current, se2));
                                    current_scanner = after_se2;
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    (current, current_scanner)
                })
            })
            .otherwise(|| {
                if in_flag {
                    scan_for_private_identifier(scanner, parser.source, ScanGoal::InputElementRegExp).and_then(|(pid, after_pid)| {
                        scan_for_keyword(after_pid, parser.source, ScanGoal::InputElementDiv, Keyword::In).and_then(|after_in| {
                            ShiftExpression::parse(parser, after_in, yield_flag, await_flag).map(|(se, after_se)| (Rc::new(RelationalExpression::PrivateIn(pid, se)), after_se))
                        })
                    })
                } else {
                    Err(ParseError::new("", 0, 0))
                }
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            RelationalExpression::ShiftExpression(n) => n.contains(kind),
            RelationalExpression::Less(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::Greater(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::LessEqual(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::GreaterEqual(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::InstanceOf(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::In(l, r) => l.contains(kind) || r.contains(kind),
            RelationalExpression::PrivateIn(_, r) => r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            RelationalExpression::ShiftExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            RelationalExpression::ShiftExpression(n) => n.all_private_identifiers_valid(names),
            RelationalExpression::Less(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            RelationalExpression::Greater(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            RelationalExpression::LessEqual(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            RelationalExpression::GreaterEqual(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            RelationalExpression::InstanceOf(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            RelationalExpression::In(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),

            // RelationalExpression : PrivateIdentifier in ShiftExpression
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of ShiftExpression with argument names.
            //  2. Return false.
            RelationalExpression::PrivateIn(pid, r) => names.contains(&pid.string_value) && r.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;

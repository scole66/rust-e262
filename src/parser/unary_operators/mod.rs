use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::async_function_definitions::AwaitExpression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, StringToken, Token};
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// UnaryExpression[Yield, Await] :
//      UpdateExpression[?Yield, ?Await]
//      delete UnaryExpression[?Yield, ?Await]
//      void UnaryExpression[?Yield, ?Await]
//      typeof UnaryExpression[?Yield, ?Await]
//      + UnaryExpression[?Yield, ?Await]
//      - UnaryExpression[?Yield, ?Await]
//      ~ UnaryExpression[?Yield, ?Await]
//      ! UnaryExpression[?Yield, ?Await]
//      [+Await]AwaitExpression[?Yield]
#[derive(Debug)]
pub enum UnaryExpression {
    UpdateExpression(Rc<UpdateExpression>),
    Delete(Rc<UnaryExpression>),
    Void(Rc<UnaryExpression>),
    Typeof(Rc<UnaryExpression>),
    NoOp(Rc<UnaryExpression>),
    Negate(Rc<UnaryExpression>),
    Complement(Rc<UnaryExpression>),
    Not(Rc<UnaryExpression>),
    Await(Rc<AwaitExpression>),
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryExpression::UpdateExpression(boxed) => write!(f, "{}", boxed),
            UnaryExpression::Delete(boxed) => write!(f, "delete {}", boxed),
            UnaryExpression::Void(boxed) => write!(f, "void {}", boxed),
            UnaryExpression::Typeof(boxed) => write!(f, "typeof {}", boxed),
            UnaryExpression::NoOp(boxed) => write!(f, "+ {}", boxed),
            UnaryExpression::Negate(boxed) => write!(f, "- {}", boxed),
            UnaryExpression::Complement(boxed) => write!(f, "~ {}", boxed),
            UnaryExpression::Not(boxed) => write!(f, "! {}", boxed),
            UnaryExpression::Await(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for UnaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UnaryExpression: {}", first, self)?;
        match &self {
            UnaryExpression::UpdateExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Delete(boxed)
            | UnaryExpression::Void(boxed)
            | UnaryExpression::Typeof(boxed)
            | UnaryExpression::NoOp(boxed)
            | UnaryExpression::Negate(boxed)
            | UnaryExpression::Complement(boxed)
            | UnaryExpression::Not(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Await(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |node: &UnaryExpression, op, kind| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}UnaryExpression: {}", first, self)
                .and_then(|_| pprint_token(writer, op, kind, &successive, Spot::NotFinal))
                .and_then(|_| node.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            UnaryExpression::UpdateExpression(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Await(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Delete(node) => work(node, "delete", TokenType::Keyword),
            UnaryExpression::Void(node) => work(node, "void", TokenType::Keyword),
            UnaryExpression::Typeof(node) => work(node, "typeof", TokenType::Keyword),
            UnaryExpression::NoOp(node) => work(node, "+", TokenType::Punctuator),
            UnaryExpression::Negate(node) => work(node, "-", TokenType::Punctuator),
            UnaryExpression::Complement(node) => work(node, "~", TokenType::Punctuator),
            UnaryExpression::Not(node) => work(node, "!", TokenType::Punctuator),
        }
    }
}

impl IsFunctionDefinition for UnaryExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.is_function_definition(),
            UnaryExpression::Delete(_)
            | UnaryExpression::Void(_)
            | UnaryExpression::Typeof(_)
            | UnaryExpression::NoOp(_)
            | UnaryExpression::Negate(_)
            | UnaryExpression::Complement(_)
            | UnaryExpression::Not(_)
            | UnaryExpression::Await(_) => false,
        }
    }
}

impl AssignmentTargetType for UnaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.assignment_target_type(),
            UnaryExpression::Delete(_)
            | UnaryExpression::Void(_)
            | UnaryExpression::Typeof(_)
            | UnaryExpression::NoOp(_)
            | UnaryExpression::Negate(_)
            | UnaryExpression::Complement(_)
            | UnaryExpression::Not(_)
            | UnaryExpression::Await(_) => ATTKind::Invalid,
        }
    }
}

impl UnaryExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (token, after_token) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        let mut unary_helper = |f: fn(Rc<Self>) -> Self| UnaryExpression::parse(parser, after_token, yield_flag, await_flag).map(|(boxed, after)| (Rc::new(f(boxed)), after));
        match token {
            Token::Identifier(id) if id.matches(Keyword::Delete) => unary_helper(UnaryExpression::Delete),
            Token::Identifier(id) if id.matches(Keyword::Void) => unary_helper(UnaryExpression::Void),
            Token::Identifier(id) if id.matches(Keyword::Typeof) => unary_helper(UnaryExpression::Typeof),
            Token::Punctuator(Punctuator::Plus) => unary_helper(UnaryExpression::NoOp),
            Token::Punctuator(Punctuator::Minus) => unary_helper(UnaryExpression::Negate),
            Token::Punctuator(Punctuator::Tilde) => unary_helper(UnaryExpression::Complement),
            Token::Punctuator(Punctuator::Bang) => unary_helper(UnaryExpression::Not),
            _ => Err(ParseError::new("UnaryExpression expected", scanner.line, scanner.column))
                .otherwise(|| {
                    if await_flag {
                        AwaitExpression::parse(parser, scanner, yield_flag).map(|(ae, after)| (Rc::new(UnaryExpression::Await(ae)), after))
                    } else {
                        Err(ParseError::new(String::new(), scanner.line, scanner.column))
                    }
                })
                .otherwise(|| UpdateExpression::parse(parser, scanner, yield_flag, await_flag).map(|(ue, after)| (Rc::new(UnaryExpression::UpdateExpression(ue)), after))),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.unary_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.unary_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            UnaryExpression::UpdateExpression(n) => n.contains(kind),
            UnaryExpression::Delete(n) => n.contains(kind),
            UnaryExpression::Void(n) => n.contains(kind),
            UnaryExpression::Typeof(n) => n.contains(kind),
            UnaryExpression::NoOp(n) => n.contains(kind),
            UnaryExpression::Negate(n) => n.contains(kind),
            UnaryExpression::Complement(n) => n.contains(kind),
            UnaryExpression::Not(n) => n.contains(kind),
            UnaryExpression::Await(n) => kind == ParseNodeKind::AwaitExpression || n.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            UnaryExpression::UpdateExpression(n) => n.as_string_literal(),
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
            UnaryExpression::UpdateExpression(n) => n.all_private_identifiers_valid(names),
            UnaryExpression::Delete(n)
            | UnaryExpression::Void(n)
            | UnaryExpression::Typeof(n)
            | UnaryExpression::NoOp(n)
            | UnaryExpression::Negate(n)
            | UnaryExpression::Complement(n)
            | UnaryExpression::Not(n) => n.all_private_identifiers_valid(names),
            UnaryExpression::Await(n) => n.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            UnaryExpression::UpdateExpression(n) => n.early_errors(agent, errs, strict),
            UnaryExpression::Delete(_n) => {
                // Static Semantics: Early Errors
                //      UnaryExpression : delete UnaryExpression
                //
                //  * It is a Syntax Error if the UnaryExpression is contained in strict mode code and the derived
                //    UnaryExpression is one of:
                //      PrimaryExpression : IdentifierReference
                //      MemberExpression : MemberExpression . PrivateIdentifier
                //      CallExpression : CallExpression . PrivateIdentifier
                //      OptionalChain : ?. PrivateIdentifier
                //      OptionalChain : OptionalChain . PrivateIdentifier
                // * It is a Syntax Error if the derived UnaryExpression is
                //      PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
                //   and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in
                //   place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is
                //   recursively applied.
                //
                // NOTE |   The last rule means that expressions such as delete (((foo))) produce early errors because
                //      |   of recursive application of the first rule.
                todo!()
            }
            UnaryExpression::Void(n) | UnaryExpression::Typeof(n) | UnaryExpression::NoOp(n) | UnaryExpression::Negate(n) | UnaryExpression::Complement(n) | UnaryExpression::Not(n) => {
                n.early_errors(agent, errs, strict)
            }
            UnaryExpression::Await(n) => n.early_errors(agent, errs, strict),
        }
    }
}

#[cfg(test)]
mod tests;

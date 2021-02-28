use itertools;
use std::cmp;
use std::cmp::Ordering;

use super::scanner;
use super::scanner::{scan_token, IdentifierData, Keyword, Punctuator, ScanGoal, Scanner, Token};
use crate::strings::JSString;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ParseGoal {
    Script,
    Module,
}

pub struct Parser<'a> {
    pub source: &'a str,
    pub strict: bool,
    pub goal: ParseGoal,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, strict: bool, goal: ParseGoal) -> Self {
        Self { source, strict, goal }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub msg: String,
    pub line: u32,
    pub column: u32,
}

impl ParseError {
    pub fn new<T>(msg: T, line: u32, column: u32) -> Self
    where
        T: Into<String>,
    {
        ParseError { msg: msg.into(), line, column }
    }

    // compare: returns Less, Equal, Greater based on the line & column of the error.
    // Note that this _is not_ PartialOrd, because we're not looking at the error string.
    // Implementing PartialOrd would mean we need to implement PartialEq, and I don't
    // want to say two Errors are Eq if they simply reside at the same position!
    pub fn compare(left: &ParseError, right: &ParseError) -> Ordering {
        if left.line < right.line {
            Ordering::Less
        } else if left.line > right.line {
            Ordering::Greater
        } else {
            if left.column < right.column {
                Ordering::Less
            } else if left.column > right.column {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }
    }

    pub fn compare_option(left: &Option<ParseError>, right: &Option<ParseError>) -> Ordering {
        match (left, right) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(l), Some(r)) => Self::compare(l, r),
        }
    }

    pub fn compare_opt(left: &&Option<ParseError>, right: &&Option<ParseError>) -> Ordering {
        Self::compare_option(*left, *right)
    }

    pub fn compare_refopt(left: &&&Option<ParseError>, right: &&&Option<ParseError>) -> Ordering {
        Self::compare_opt(*left, *right)
    }
}

impl From<ParseError> for String {
    fn from(source: ParseError) -> Self {
        format!("{}:{}: {}", source.line, source.column, source.msg)
    }
}

////
// Otherwise:
//
// If self is Ok, return unchanged.
// If self is Err, run supplied function.
// If function's result is Ok return that.
// If function's result is Err, compare the errs and return the greatest one. (Favoring the earlier error)
//
// This way: the error we report is the one that got "furthest".
//
// Use like:
//    parse_first_kind(parse_args)
//       .otherwise(|| parse_second_kind(parse_args))
//       .otherwise(|| parse_third_kind(parse_args))
pub trait Otherwise<T> {
    fn otherwise<O>(self, f: O) -> Result<T, ParseError>
    where
        O: FnOnce() -> Result<T, ParseError>;
}

impl<T> Otherwise<T> for Result<T, ParseError> {
    fn otherwise<O>(self, f: O) -> Self
    where
        O: FnOnce() -> Result<T, ParseError>,
    {
        self.or_else(|err1| f().map_err(|err2| cmp::max_by(err2, err1, ParseError::compare)))
    }
}

pub trait StringValue {
    fn string_value(&self) -> JSString;
}

pub trait BoundNames {
    fn bound_names(&self) -> Vec<JSString>;
}

pub trait HasName {
    fn has_name(&self) -> bool;
}

pub trait IsFunctionDefinition {
    fn is_function_definition(&self) -> bool;
}

pub trait IsIdentifierReference {
    fn is_identifier_reference(&self) -> bool;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ATTKind {
    Invalid,
    Simple,
}
pub trait AssignmentTargetType {
    fn assignment_target_type(&self) -> ATTKind;
}

pub fn scan_for_punct(scanner: Scanner, src: &str, goal: ScanGoal, punct: Punctuator) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(punct) {
        Ok(after_tok)
    } else {
        Err(ParseError::new(format!("‘{}’ expected", punct), scanner.line, scanner.column))
    }
}

pub fn scan_for_punct_set(scanner: Scanner, src: &str, goal: ScanGoal, punct_set: &[Punctuator]) -> Result<(Punctuator, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&p) = punct_set.iter().find(|&p| tok.matches_punct(*p)) {
        Ok((p, after_tok))
    } else {
        Err(ParseError::new(format!("One of [{}] expected", itertools::join(punct_set.iter().map(|&p| format!("‘{}’", p)), ", ")), scanner.line, scanner.column))
    }
}

pub fn scan_for_keyword(scanner: Scanner, src: &str, goal: ScanGoal, kwd: Keyword) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_keyword(kwd) {
        Ok(after_tok)
    } else {
        Err(ParseError::new(format!("‘{}’ expected", kwd), scanner.line, scanner.column))
    }
}

pub fn scan_for_keywords(scanner: Scanner, src: &str, goal: ScanGoal, kwds: &[Keyword]) -> Result<(Keyword, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&k) = kwds.iter().find(|&k| tok.matches_keyword(*k)) {
        Ok((k, after_tok))
    } else {
        Err(ParseError::new(format!("One of [{}] expected", itertools::join(kwds.iter().map(|&k| format!("‘{}’", k)), ", ")), scanner.line, scanner.column))
    }
}

pub fn scan_for_identifiername(scanner: Scanner, src: &str, goal: ScanGoal) -> Result<(IdentifierData, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Token::Identifier(id) = tok {
        Ok((id, after_tok))
    } else {
        Err(ParseError::new("IdentifierName expected", scanner.line, scanner.column))
    }
}

//no_line_terminator(after_cont, parser.source)?;
// If there is no newline sequence between the scanner's spot and the _end_ of the next token, return Ok
// else return Err.
//
// (Note that this is really supposed to be) between the current spot and the _start_ of the next token,
// but the scanner doesn't support that yet. Some tokens span more than one line.)
pub fn no_line_terminator(scanner: Scanner, src: &str) -> Result<(), ParseError> {
    let (_, after_tok) = scan_token(&scanner, src, ScanGoal::InputElementDiv);
    if after_tok.line == scanner.line {
        Ok(())
    } else {
        Err(ParseError::new("Newline not allowed here.", scanner.line, scanner.column))
    }
}

pub mod additive_operators;
pub mod arrow_function_definitions;
pub mod assignment_operators;
pub mod async_arrow_function_definitions;
pub mod async_function_definitions;
pub mod async_generator_function_definitions;
pub mod binary_bitwise_operators;
pub mod binary_logical_operators;
pub mod bitwise_shift_operators;
pub mod block;
pub mod break_statement;
pub mod class_definitions;
pub mod comma_operator;
pub mod conditional_operator;
pub mod continue_statement;
pub mod debugger_statement;
pub mod declarations_and_variables;
pub mod empty_statement;
pub mod equality_operators;
pub mod exponentiation_operator;
pub mod expression_statement;
pub mod function_definitions;
pub mod generator_function_definitions;
pub mod identifiers;
pub mod if_statement;
pub mod iteration_statements;
pub mod labelled_statements;
pub mod left_hand_side_expressions;
pub mod method_definitions;
pub mod multiplicative_operators;
pub mod parameter_lists;
pub mod primary_expressions;
pub mod relational_operators;
pub mod return_statement;
pub mod statements_and_declarations;
pub mod switch_statement;
pub mod throw_statement;
pub mod try_statement;
pub mod unary_operators;
pub mod update_expressions;
pub mod with_statement;

#[cfg(test)]
pub mod testhelp {
    use super::*;
    use std::fmt;
    pub fn check<T>(res: Result<(Box<T>, Scanner), ParseError>) -> (Box<T>, Scanner) {
        assert!(res.is_ok());
        return res.unwrap();
    }
    pub fn check_err<T>(res: Result<(Box<T>, Scanner), ParseError>, msg: &str, line: u32, column: u32)
    where
        T: fmt::Debug,
    {
        assert!(res.is_err());
        let err = res.unwrap_err();
        assert_eq!((err.msg, err.line, err.column), (String::from(msg), line, column));
    }
    pub fn chk_scan(scanner: &Scanner, count: u32) {
        assert_eq!(*scanner, Scanner { line: 1, column: count + 1, start_idx: count as usize });
    }
    pub fn newparser(text: &str) -> Parser {
        Parser::new(text, false, ParseGoal::Script)
    }
    pub fn check_parse_error<T, U>(result: Result<T, ParseError>, msg: U)
    where
        T: fmt::Debug,
        U: Into<String>,
    {
        assert!(result.is_err());
        let pe = result.unwrap_err();
        assert_eq!(pe.line, 1);
        assert_eq!(pe.column, 1);
        assert_eq!(pe.msg, msg.into());
    }
}

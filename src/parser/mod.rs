use super::scanner;

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

pub trait StringValue {
    fn string_value(&self) -> scanner::JSString;
}

pub trait BoundNames {
    fn bound_names(&self) -> Vec<scanner::JSString>;
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

pub fn rewrap<T, E>(value: T) -> Result<Option<T>, E> {
    Ok(Some(value))
}

pub mod additive_operators;
pub mod assignment_operators;
pub mod async_function_definitions;
pub mod bitwise_shift_operators;
pub mod comma_operator;
pub mod exponentiation_operator;
pub mod identifiers;
pub mod left_hand_side_expressions;
pub mod method_definitions;
pub mod multiplicative_operators;
pub mod primary_expressions;
pub mod unary_operators;
pub mod update_expressions;

#[cfg(test)]
pub mod testhelp {
    use super::scanner::Scanner;
    use super::*;
    pub fn check<T>(res: Result<Option<(Box<T>, Scanner)>, String>) -> (Box<T>, Scanner) {
        assert!(res.is_ok());
        let potential = res.unwrap();
        assert!(potential.is_some());
        potential.unwrap()
    }
    pub fn check_none<T>(res: Result<Option<(Box<T>, Scanner)>, String>) {
        assert!(res.is_ok());
        let potential = res.unwrap();
        assert!(potential.is_none());
    }
    pub fn chk_scan(scanner: &Scanner, count: u32) {
        assert_eq!(
            *scanner,
            Scanner {
                line: 1,
                column: count + 1,
                start_idx: count as usize
            }
        );
    }
    pub fn newparser(text: &str) -> Parser {
        Parser::new(text, false, ParseGoal::Script)
    }
}

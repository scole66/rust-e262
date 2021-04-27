use super::scanner;
use super::scanner::{scan_token, IdentifierData, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::strings::JSString;
use ahash::RandomState;
use arrow_function_definitions::{ArrowFormalParameters, ExpressionBody};
use assignment_operators::AssignmentExpression;
use async_arrow_function_definitions::CoverCallExpressionAndAsyncArrowHead;
use async_function_definitions::AsyncFunctionBody;
use async_generator_function_definitions::AsyncGeneratorBody;
use binary_bitwise_operators::BitwiseORExpression;
use binary_logical_operators::CoalesceExpression;
use block::{Block, StatementList};
use class_definitions::ClassTail;
use comma_operator::Expression;
use declarations_and_variables::{BindingElement, BindingPattern, BindingRestElement, BindingRestProperty, LexicalDeclaration, SingleNameBinding, VariableDeclarationList};
use function_definitions::{FunctionBody, FunctionDeclaration};
use generator_function_definitions::GeneratorBody;
use identifiers::{BindingIdentifier, Identifier, IdentifierReference, LabelIdentifier};
use iteration_statements::{DoWhileStatement, ForBinding, ForDeclaration, ForInOfStatement, ForStatement, IterationStatement, WhileStatement};
use left_hand_side_expressions::{Arguments, CallExpression, LeftHandSideExpression, MemberExpression, MetaProperty};
use method_definitions::MethodDefinition;
use parameter_lists::{FormalParameter, FormalParameters, UniqueFormalParameters};
use primary_expressions::{CoverParenthesizedExpressionAndArrowParameterList, Elisions, Initializer, LiteralPropertyName, PropertyName, TemplateLiteral};
use statements_and_declarations::{BreakableStatement, Declaration, HoistableDeclaration, Statement};
use std::cmp;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;
use switch_statement::{CaseBlock, CaseClause, CaseClauses, DefaultClause, SwitchStatement};
use throw_statement::ThrowStatement;
use try_statement::{Catch, CatchParameter, Finally, TryStatement};
use unary_operators::UnaryExpression;
use update_expressions::UpdateExpression;
use with_statement::WithStatement;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ParseGoal {
    Script,
    Module,
}

impl Default for ParseGoal {
    fn default() -> Self {
        ParseGoal::Script
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct YieldAwaitKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct YieldAwaitTaggedKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    tagged_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct InYieldAwaitKey {
    scanner: Scanner,
    in_flag: bool,
    yield_flag: bool,
    await_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct InKey {
    scanner: Scanner,
    in_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct InAwaitKey {
    scanner: Scanner,
    in_flag: bool,
    await_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct YieldAwaitReturnKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    return_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct YieldKey {
    scanner: Scanner,
    yield_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct YieldAwaitDefaultKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    default_flag: bool,
}

type ParseResult<T> = Result<(Rc<T>, Scanner), ParseError>;

#[derive(Default)]
pub struct Parser<'a> {
    pub source: &'a str,
    pub strict: bool,
    pub goal: ParseGoal,
    pub arguments_cache: HashMap<YieldAwaitKey, ParseResult<Arguments>, RandomState>,
    pub arrow_formal_parameters_cache: HashMap<YieldAwaitKey, ParseResult<ArrowFormalParameters>, RandomState>,
    pub assignment_expression_cache: HashMap<InYieldAwaitKey, ParseResult<AssignmentExpression>, RandomState>,
    pub async_function_body_cache: HashMap<Scanner, (Rc<AsyncFunctionBody>, Scanner), RandomState>,
    pub async_generator_body_cache: HashMap<Scanner, (Rc<AsyncGeneratorBody>, Scanner), RandomState>,
    pub binding_element_cache: HashMap<YieldAwaitKey, ParseResult<BindingElement>, RandomState>,
    pub binding_identifier_cache: HashMap<YieldAwaitKey, ParseResult<BindingIdentifier>, RandomState>,
    pub binding_pattern_cache: HashMap<YieldAwaitKey, ParseResult<BindingPattern>, RandomState>,
    pub binding_rest_element_cache: HashMap<YieldAwaitKey, ParseResult<BindingRestElement>, RandomState>,
    pub binding_rest_property_cache: HashMap<YieldAwaitKey, ParseResult<BindingRestProperty>, RandomState>,
    pub bitwise_or_expression_cache: HashMap<InYieldAwaitKey, ParseResult<BitwiseORExpression>, RandomState>,
    pub block_cache: HashMap<YieldAwaitReturnKey, ParseResult<Block>, RandomState>,
    pub breakable_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<BreakableStatement>, RandomState>,
    pub call_expression_cache: HashMap<YieldAwaitKey, ParseResult<CallExpression>, RandomState>,
    pub case_block_cache: HashMap<YieldAwaitReturnKey, ParseResult<CaseBlock>, RandomState>,
    pub case_clause_cache: HashMap<YieldAwaitReturnKey, ParseResult<CaseClause>, RandomState>,
    pub case_clauses_cache: HashMap<YieldAwaitReturnKey, ParseResult<CaseClauses>, RandomState>,
    pub catch_cache: HashMap<YieldAwaitReturnKey, ParseResult<Catch>, RandomState>,
    pub catch_parameter_cache: HashMap<YieldAwaitKey, ParseResult<CatchParameter>, RandomState>,
    pub class_tail_cache: HashMap<YieldAwaitKey, ParseResult<ClassTail>, RandomState>,
    pub coalesce_expression_cache: HashMap<InYieldAwaitKey, ParseResult<CoalesceExpression>, RandomState>,
    pub cover_call_expression_and_async_arrow_head_cache: HashMap<YieldAwaitKey, ParseResult<CoverCallExpressionAndAsyncArrowHead>, RandomState>,
    pub cpeaapl_cache: HashMap<YieldAwaitKey, ParseResult<CoverParenthesizedExpressionAndArrowParameterList>, RandomState>,
    pub declaration_cache: HashMap<YieldAwaitKey, ParseResult<Declaration>, RandomState>,
    pub default_clause_cache: HashMap<YieldAwaitReturnKey, ParseResult<DefaultClause>, RandomState>,
    pub do_while_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<DoWhileStatement>, RandomState>,
    pub elision_cache: HashMap<Scanner, ParseResult<Elisions>, RandomState>,
    pub expression_body_cache: HashMap<InAwaitKey, ParseResult<ExpressionBody>, RandomState>,
    pub expression_cache: HashMap<InYieldAwaitKey, ParseResult<Expression>, RandomState>,
    pub finally_cache: HashMap<YieldAwaitReturnKey, ParseResult<Finally>, RandomState>,
    pub for_binding_cache: HashMap<YieldAwaitKey, ParseResult<ForBinding>, RandomState>,
    pub for_declaration_cache: HashMap<YieldAwaitKey, ParseResult<ForDeclaration>, RandomState>,
    pub for_in_of_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<ForInOfStatement>, RandomState>,
    pub for_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<ForStatement>, RandomState>,
    pub formal_parameter_cache: HashMap<YieldAwaitKey, ParseResult<FormalParameter>, RandomState>,
    pub formal_parameters_cache: HashMap<YieldAwaitKey, (Rc<FormalParameters>, Scanner), RandomState>,
    pub function_body_cache: HashMap<YieldAwaitKey, (Rc<FunctionBody>, Scanner), RandomState>,
    pub function_declaration_cache: HashMap<YieldAwaitDefaultKey, ParseResult<FunctionDeclaration>, RandomState>,
    pub generator_body_cache: HashMap<Scanner, (Rc<GeneratorBody>, Scanner), RandomState>,
    pub hoistable_declaration_cache: HashMap<YieldAwaitDefaultKey, ParseResult<HoistableDeclaration>, RandomState>,
    pub identifier_cache: HashMap<Scanner, ParseResult<Identifier>, RandomState>,
    pub identifier_reference_cache: HashMap<YieldAwaitKey, ParseResult<IdentifierReference>, RandomState>,
    pub initializer_cache: HashMap<InYieldAwaitKey, ParseResult<Initializer>, RandomState>,
    pub iteration_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<IterationStatement>, RandomState>,
    pub label_identifier_cache: HashMap<YieldAwaitKey, ParseResult<LabelIdentifier>, RandomState>,
    pub lexical_declaration_cache: HashMap<InYieldAwaitKey, ParseResult<LexicalDeclaration>, RandomState>,
    pub lhs_cache: HashMap<YieldAwaitKey, ParseResult<LeftHandSideExpression>, RandomState>,
    pub lpn_cache: HashMap<Scanner, ParseResult<LiteralPropertyName>, RandomState>,
    pub member_expression_cache: HashMap<YieldAwaitKey, ParseResult<MemberExpression>, RandomState>,
    pub meta_property_cache: HashMap<Scanner, ParseResult<MetaProperty>, RandomState>,
    pub method_definition_cache: HashMap<YieldAwaitKey, ParseResult<MethodDefinition>, RandomState>,
    pub property_name_cache: HashMap<YieldAwaitKey, ParseResult<PropertyName>, RandomState>,
    pub single_name_binding_cache: HashMap<YieldAwaitKey, ParseResult<SingleNameBinding>, RandomState>,
    pub statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<Statement>, RandomState>,
    pub statement_list_cache: HashMap<YieldAwaitReturnKey, ParseResult<StatementList>, RandomState>,
    pub switch_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<SwitchStatement>, RandomState>,
    pub template_literal_cache: HashMap<YieldAwaitTaggedKey, ParseResult<TemplateLiteral>, RandomState>,
    pub throw_statement_cache: HashMap<YieldAwaitKey, ParseResult<ThrowStatement>, RandomState>,
    pub try_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<TryStatement>, RandomState>,
    pub unary_expression_cache: HashMap<YieldAwaitKey, ParseResult<UnaryExpression>, RandomState>,
    pub unique_formal_parameters_cache: HashMap<YieldAwaitKey, (Rc<UniqueFormalParameters>, Scanner), RandomState>,
    pub update_expression_cache: HashMap<YieldAwaitKey, ParseResult<UpdateExpression>, RandomState>,
    pub variable_declaration_list_cache: HashMap<InYieldAwaitKey, ParseResult<VariableDeclarationList>, RandomState>,
    pub while_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<WhileStatement>, RandomState>,
    pub with_statement_cache: HashMap<YieldAwaitReturnKey, ParseResult<WithStatement>, RandomState>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, strict: bool, goal: ParseGoal) -> Self {
        Self { source, strict, goal, ..Default::default() }
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
        } else if left.column < right.column {
            Ordering::Less
        } else if left.column > right.column {
            Ordering::Greater
        } else {
            Ordering::Equal
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
pub mod scripts;
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
    pub fn check<T>(res: ParseResult<T>) -> (Rc<T>, Scanner) {
        assert!(res.is_ok());
        res.unwrap()
    }
    pub fn check_err<T>(res: ParseResult<T>, msg: &str, line: u32, column: u32)
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
    pub fn check_parse_error<T, U>(result: ParseResult<T>, msg: U)
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

#[cfg(test)]
#[allow(clippy::clone_on_copy)]
mod tests {
    use super::*;
    use ahash::AHasher;
    use std::hash::{Hash, Hasher};

    fn calculate_hash<T: Hash>(t: &T) -> u64 {
        let mut s = AHasher::new_with_keys(1234, 5678);
        t.hash(&mut s);
        s.finish()
    }

    #[test]
    fn parse_goal_01() {
        format!("{:?} {:?}", ParseGoal::Script, ParseGoal::Module);
    }
    #[test]
    fn parse_goal_02() {
        let left = ParseGoal::Script;
        let right_first = ParseGoal::Script;
        let right_second = ParseGoal::Module;
        assert_eq!(left, right_first);
        assert_ne!(left, right_second);
    }
    #[test]
    fn parse_goal_03() {
        let a = ParseGoal::Script;
        let b = a.clone();
        assert_eq!(a, b);
    }
    #[test]
    fn parse_goal_04() {
        let a = ParseGoal::default();
        assert_eq!(a, ParseGoal::Script);
    }

    #[test]
    fn yield_await_key_01() {
        let left = YieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: false };
        let right = left.clone();
        let third = YieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn yield_await_tagged_key_01() {
        let left = YieldAwaitTaggedKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: false, tagged_flag: false };
        let right = left.clone();
        let third = YieldAwaitTaggedKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: false, await_flag: true, tagged_flag: false };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn in_yield_await_key_01() {
        let left = InYieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, yield_flag: false, await_flag: false };
        let right = left.clone();
        let third = InYieldAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, yield_flag: false, await_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn in_key_01() {
        let left = InKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true };
        let right = left.clone();
        let third = InKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, in_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn in_await_key_01() {
        let left = InAwaitKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, in_flag: true, await_flag: false };
        let right = left.clone();
        let third = InAwaitKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, in_flag: true, await_flag: false };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn yield_await_return_key_01() {
        let left = YieldAwaitReturnKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true, await_flag: false, return_flag: true };
        let right = left.clone();
        let third = YieldAwaitReturnKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true, await_flag: false, return_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn yield_key_01() {
        let left = YieldKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true };
        let right = left.clone();
        let third = YieldKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }
    #[test]
    fn yield_await_default_key_01() {
        let left = YieldAwaitDefaultKey { scanner: Scanner { line: 10, column: 12, start_idx: 11 }, yield_flag: true, await_flag: true, default_flag: true };
        let right = left.clone();
        let third = YieldAwaitDefaultKey { scanner: Scanner { line: 10, column: 22, start_idx: 11 }, yield_flag: true, await_flag: true, default_flag: true };
        assert_eq!(left.eq(&right), true);
        assert_eq!(left.ne(&third), true);
        assert_ne!(calculate_hash(&left), calculate_hash(&third));
        assert_eq!(calculate_hash(&left), calculate_hash(&right));
        format!("{:?}", left);
    }

    #[test]
    fn parser_01() {
        let p = Parser::new("program text", false, ParseGoal::Script);
        assert_eq!(p.strict, false);
        assert_eq!(p.source, "program text");
        assert_eq!(p.goal, ParseGoal::Script);
        assert!(p.arguments_cache.is_empty());
        assert!(p.arrow_formal_parameters_cache.is_empty());
        assert!(p.assignment_expression_cache.is_empty());
        assert!(p.async_function_body_cache.is_empty());
        assert!(p.async_generator_body_cache.is_empty());
        assert!(p.binding_element_cache.is_empty());
        assert!(p.binding_identifier_cache.is_empty());
        assert!(p.binding_pattern_cache.is_empty());
        assert!(p.binding_rest_element_cache.is_empty());
        assert!(p.binding_rest_property_cache.is_empty());
        assert!(p.bitwise_or_expression_cache.is_empty());
        assert!(p.block_cache.is_empty());
        assert!(p.breakable_statement_cache.is_empty());
        assert!(p.call_expression_cache.is_empty());
        assert!(p.case_block_cache.is_empty());
        assert!(p.case_clause_cache.is_empty());
        assert!(p.case_clauses_cache.is_empty());
        assert!(p.catch_cache.is_empty());
        assert!(p.catch_parameter_cache.is_empty());
        assert!(p.class_tail_cache.is_empty());
        assert!(p.coalesce_expression_cache.is_empty());
        assert!(p.cover_call_expression_and_async_arrow_head_cache.is_empty());
        assert!(p.cpeaapl_cache.is_empty());
        assert!(p.declaration_cache.is_empty());
        assert!(p.default_clause_cache.is_empty());
        assert!(p.do_while_statement_cache.is_empty());
        assert!(p.elision_cache.is_empty());
        assert!(p.expression_body_cache.is_empty());
        assert!(p.expression_cache.is_empty());
        assert!(p.finally_cache.is_empty());
        assert!(p.for_binding_cache.is_empty());
        assert!(p.for_declaration_cache.is_empty());
        assert!(p.for_in_of_statement_cache.is_empty());
        assert!(p.for_statement_cache.is_empty());
        assert!(p.formal_parameter_cache.is_empty());
        assert!(p.formal_parameters_cache.is_empty());
        assert!(p.function_body_cache.is_empty());
        assert!(p.function_declaration_cache.is_empty());
        assert!(p.generator_body_cache.is_empty());
        assert!(p.hoistable_declaration_cache.is_empty());
        assert!(p.identifier_cache.is_empty());
        assert!(p.identifier_reference_cache.is_empty());
        assert!(p.initializer_cache.is_empty());
        assert!(p.iteration_statement_cache.is_empty());
        assert!(p.label_identifier_cache.is_empty());
        assert!(p.lexical_declaration_cache.is_empty());
        assert!(p.lhs_cache.is_empty());
        assert!(p.lpn_cache.is_empty());
        assert!(p.member_expression_cache.is_empty());
        assert!(p.meta_property_cache.is_empty());
        assert!(p.method_definition_cache.is_empty());
        assert!(p.property_name_cache.is_empty());
        assert!(p.single_name_binding_cache.is_empty());
        assert!(p.statement_cache.is_empty());
        assert!(p.statement_list_cache.is_empty());
        assert!(p.switch_statement_cache.is_empty());
        assert!(p.template_literal_cache.is_empty());
        assert!(p.throw_statement_cache.is_empty());
        assert!(p.try_statement_cache.is_empty());
        assert!(p.unary_expression_cache.is_empty());
        assert!(p.unique_formal_parameters_cache.is_empty());
        assert!(p.update_expression_cache.is_empty());
        assert!(p.variable_declaration_list_cache.is_empty());
        assert!(p.while_statement_cache.is_empty());
        assert!(p.with_statement_cache.is_empty());
    }

    #[test]
    fn parse_error_01() {
        let e1 = ParseError::new("&str style error", 10, 11);
        let e2 = ParseError::new(String::from("String style error"), 20, 22);
        assert_eq!(e1.msg, "&str style error");
        assert_eq!(e1.line, 10);
        assert_eq!(e1.column, 11);
        assert_eq!(e2.msg, "String style error");
        assert_eq!(e2.line, 20);
        assert_eq!(e2.column, 22);
        let e3 = e2.clone();
        assert_eq!(e3.msg, "String style error");
        assert_eq!(e3.line, 20);
        assert_eq!(e3.column, 22);
        format!("{:?}", e1);
    }
    #[test]
    fn parse_error_compare_01() {
        let e1 = ParseError::new("&str style error", 10, 11);
        let e2 = ParseError::new(String::from("String style error"), 20, 22);

        assert_eq!(ParseError::compare(&e1, &e2), Ordering::Less);
        assert_eq!(ParseError::compare(&e2, &e1), Ordering::Greater);

        let e3 = ParseError::new("other", 10, 20);
        assert_eq!(ParseError::compare(&e1, &e3), Ordering::Less);
        assert_eq!(ParseError::compare(&e3, &e1), Ordering::Greater);

        let e4 = ParseError::new("overlap", 10, 11);
        assert_eq!(ParseError::compare(&e1, &e4), Ordering::Equal);
        assert_eq!(ParseError::compare(&e4, &e1), Ordering::Equal);
    }
    #[test]
    fn parse_error_compare_options_01() {
        let e1 = None;
        let e2 = None;
        assert_eq!(ParseError::compare_option(&e1, &e2), Ordering::Equal);
        let e3 = Some(ParseError::new("At (10, 10)", 10, 10));
        assert_eq!(ParseError::compare_option(&e1, &e3), Ordering::Less);
        assert_eq!(ParseError::compare_option(&e3, &e1), Ordering::Greater);
        let e4 = Some(ParseError::new("At (10, 30)", 10, 30));
        assert_eq!(ParseError::compare_option(&e3, &e4), Ordering::Less);
    }
    #[test]
    fn parse_error_to_string() {
        let e1 = ParseError::new("special message", 25, 50);
        let s1 = String::from(e1);
        assert_eq!(s1, "25:50: special message")
    }

    #[test]
    fn otherwise_01() {
        let item: Result<i32, ParseError> = Ok(10);
        let result = item.otherwise(|| Ok(1));
        assert!(result.is_ok());
        if let Ok(i) = result {
            assert_eq!(i, 10);
        }
    }
    #[test]
    fn otherwise_02() {
        let item: Result<i32, ParseError> = Err(ParseError::new("otherwise_02", 10, 20));
        let result = item.otherwise(|| Ok(1));
        assert!(result.is_ok());
        if let Ok(i) = result {
            assert_eq!(i, 1);
        }
    }
    #[test]
    fn otherwise_03() {
        let item: Result<i32, ParseError> = Err(ParseError::new("otherwise_03", 10, 20));
        let result = item.otherwise(|| Err(ParseError::new("otherwise_after", 20, 3)));
        assert!(result.is_err());
        if let Err(pe) = result {
            assert_eq!(pe.msg, "otherwise_after");
        }
    }
    #[test]
    fn otherwise_04() {
        let item: Result<i32, ParseError> = Err(ParseError::new("earlier in time", 10, 20));
        let result = item.otherwise(|| Err(ParseError::new("earlier in position", 2, 3)));
        assert!(result.is_err());
        if let Err(pe) = result {
            assert_eq!(pe.msg, "earlier in time");
        }
    }
    #[test]
    fn otherwise_05() {
        let item: Result<i32, ParseError> = Err(ParseError::new("earlier in time", 10, 20));
        let result = item.otherwise(|| Err(ParseError::new("later in time", 10, 20)));
        assert!(result.is_err());
        if let Err(pe) = result {
            assert_eq!(pe.msg, "earlier in time");
        }
    }
}

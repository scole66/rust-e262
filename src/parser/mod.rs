use super::scanner;
use super::scanner::{scan_token, IdentifierData, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::strings::JSString;
use ahash::AHashMap;
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
use counter::Counter;
use declarations_and_variables::{BindingElement, BindingPattern, BindingRestElement, BindingRestProperty, LexicalDeclaration, SingleNameBinding, VariableDeclarationList};
use function_definitions::{FunctionBody, FunctionDeclaration};
use generator_function_definitions::GeneratorBody;
use identifiers::{BindingIdentifier, Identifier, IdentifierReference, LabelIdentifier};
use iteration_statements::ForBinding;
use left_hand_side_expressions::{Arguments, CallExpression, LeftHandSideExpression, MemberExpression, MetaProperty};
use method_definitions::MethodDefinition;
use parameter_lists::{FormalParameter, FormalParameters, UniqueFormalParameters};
use primary_expressions::{CoverParenthesizedExpressionAndArrowParameterList, Elisions, Initializer, LiteralPropertyName, PropertyName, TemplateLiteral};
use statements_and_declarations::Statement;
use std::cmp;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use try_statement::CatchParameter;
use unary_operators::UnaryExpression;
use update_expressions::UpdateExpression;

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

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum ParseNodeKind {
    ArrowFunction,
    AssignmentExpression,
    AssignmentOperator,
    AssignmentPattern,
    AsyncArrowFunction,
    AsyncConciseBody,
    AwaitExpression,
    BindingElement,
    BindingPattern,
    BindingProperty,
    BlockStatement,
    BreakableStatement,
    BreakStatement,
    CallExpression,
    CatchParameter,
    ClassBody,
    ClassElement,
    ClassElementName,
    ClassHeritage,
    ConciseBody,
    ConditionalExpression,
    ContinueStatement,
    DebuggerStatement,
    Declaration,
    EmptyStatement,
    ExponentiationExpression,
    Expression,
    ExpressionBody,
    ExpressionStatement,
    ForBinding,
    HoistableDeclaration,
    IdentifierName,
    IfStatement,
    IterationStatement,
    LabelledItem,
    LabelledStatement,
    LeftHandSideExpression,
    LexicalBinding,
    Literal,
    MemberExpression,
    MethodDefinition,
    NewTarget,
    NoSubstitutionTemplate,
    ObjectBindingPattern,
    OptionalExpression,
    PrimaryExpression,
    PrivateIdentifier,
    PropertyName,
    RegularExpression,
    RelationalExpression,
    ReturnStatement,
    ScriptBody,
    Statement,
    StatementList,
    StatementListItem,
    SubstitutionTemplate,
    Super,
    SuperCall,
    SuperProperty,
    TemplateLiteral,
    TemplateMiddle,
    TemplateSpans,
    TemplateTail,
    This,
    ThrowStatement,
    TryStatement,
    UnaryExpression,
    UpdateExpression,
    VariableDeclaration,
    VariableStatement,
    WithStatement,
    YieldExpression,
}
impl fmt::Display for ParseNodeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            ParseNodeKind::ArrowFunction => "ArrowFunction",
            ParseNodeKind::AssignmentExpression => "AssignmentExpression",
            ParseNodeKind::AssignmentOperator => "AssignmentOperator",
            ParseNodeKind::AssignmentPattern => "AssignmentPattern",
            ParseNodeKind::AsyncArrowFunction => "AsyncArrowFunction",
            ParseNodeKind::AsyncConciseBody => "AsyncConciseBody",
            ParseNodeKind::AwaitExpression => "AwaitExpression",
            ParseNodeKind::BindingElement => "BindingElement",
            ParseNodeKind::BindingPattern => "BindingPattern",
            ParseNodeKind::BindingProperty => "BindingProperty",
            ParseNodeKind::BlockStatement => "BlockStatement",
            ParseNodeKind::BreakableStatement => "BreakableStatement",
            ParseNodeKind::BreakStatement => "BreakStatement",
            ParseNodeKind::CallExpression => "CallExpression",
            ParseNodeKind::CatchParameter => "CatchParameter",
            ParseNodeKind::ClassBody => "ClassBody",
            ParseNodeKind::ClassElement => "ClassElement",
            ParseNodeKind::ClassElementName => "ClassElementName",
            ParseNodeKind::ClassHeritage => "ClassHeritage",
            ParseNodeKind::ConciseBody => "ConciseBody",
            ParseNodeKind::ConditionalExpression => "ConditionalExpression",
            ParseNodeKind::ContinueStatement => "ContinueStatement",
            ParseNodeKind::DebuggerStatement => "DebuggerStatement",
            ParseNodeKind::Declaration => "Declaration",
            ParseNodeKind::EmptyStatement => "EmptyStatement",
            ParseNodeKind::ExponentiationExpression => "ExponentiationExpression",
            ParseNodeKind::Expression => "Expression",
            ParseNodeKind::ExpressionBody => "ExpressionBody",
            ParseNodeKind::ExpressionStatement => "ExpressionStatement",
            ParseNodeKind::ForBinding => "ForBinding",
            ParseNodeKind::HoistableDeclaration => "HoistableDeclaration",
            ParseNodeKind::IdentifierName => "IdentifierName",
            ParseNodeKind::IfStatement => "IfStatement",
            ParseNodeKind::IterationStatement => "IterationStatement",
            ParseNodeKind::LabelledItem => "LabelledItem",
            ParseNodeKind::LabelledStatement => "LabelledStatement",
            ParseNodeKind::LeftHandSideExpression => "LeftHandSideExpression",
            ParseNodeKind::LexicalBinding => "LexicalBinding",
            ParseNodeKind::Literal => "Literal",
            ParseNodeKind::MemberExpression => "MemberExpression",
            ParseNodeKind::MethodDefinition => "MethodDefinition",
            ParseNodeKind::NewTarget => "NewTarget",
            ParseNodeKind::NoSubstitutionTemplate => "NoSubstitutionTemplate",
            ParseNodeKind::ObjectBindingPattern => "ObjectBindingPattern",
            ParseNodeKind::OptionalExpression => "OptionalExpression",
            ParseNodeKind::PrimaryExpression => "PrimaryExpression",
            ParseNodeKind::PrivateIdentifier => "PrivateIdentifier",
            ParseNodeKind::PropertyName => "PropertyName",
            ParseNodeKind::RegularExpression => "RegularExpression",
            ParseNodeKind::RelationalExpression => "RelationalExpression",
            ParseNodeKind::ReturnStatement => "ReturnStatement",
            ParseNodeKind::ScriptBody => "ScriptBody",
            ParseNodeKind::Statement => "Statement",
            ParseNodeKind::StatementList => "StatementList",
            ParseNodeKind::StatementListItem => "StatementListItem",
            ParseNodeKind::SubstitutionTemplate => "SubstitutionTemplate",
            ParseNodeKind::Super => "Super",
            ParseNodeKind::SuperCall => "SuperCall",
            ParseNodeKind::SuperProperty => "SuperProperty",
            ParseNodeKind::TemplateLiteral => "TemplateLiteral",
            ParseNodeKind::TemplateMiddle => "TemplateMiddle",
            ParseNodeKind::TemplateSpans => "TemplateSpans",
            ParseNodeKind::TemplateTail => "TemplateTail",
            ParseNodeKind::This => "This",
            ParseNodeKind::ThrowStatement => "ThrowStatement",
            ParseNodeKind::TryStatement => "TryStatement",
            ParseNodeKind::UnaryExpression => "UnaryExpression",
            ParseNodeKind::UpdateExpression => "UpdateExpression",
            ParseNodeKind::VariableDeclaration => "VariableDeclaration",
            ParseNodeKind::VariableStatement => "VariableStatement",
            ParseNodeKind::WithStatement => "WithStatement",
            ParseNodeKind::YieldExpression => "YieldExpression",
        })
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
    pub direct: bool,
    pub goal: ParseGoal,
    pub arguments_cache: AHashMap<YieldAwaitKey, ParseResult<Arguments>, RandomState>,
    pub arrow_formal_parameters_cache: AHashMap<YieldAwaitKey, ParseResult<ArrowFormalParameters>, RandomState>,
    pub assignment_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<AssignmentExpression>, RandomState>,
    pub async_function_body_cache: AHashMap<Scanner, (Rc<AsyncFunctionBody>, Scanner), RandomState>,
    pub async_generator_body_cache: AHashMap<Scanner, (Rc<AsyncGeneratorBody>, Scanner), RandomState>,
    pub binding_element_cache: AHashMap<YieldAwaitKey, ParseResult<BindingElement>, RandomState>,
    pub binding_identifier_cache: AHashMap<YieldAwaitKey, ParseResult<BindingIdentifier>, RandomState>,
    pub binding_pattern_cache: AHashMap<YieldAwaitKey, ParseResult<BindingPattern>, RandomState>,
    pub binding_rest_element_cache: AHashMap<YieldAwaitKey, ParseResult<BindingRestElement>, RandomState>,
    pub binding_rest_property_cache: AHashMap<YieldAwaitKey, ParseResult<BindingRestProperty>, RandomState>,
    pub bitwise_or_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<BitwiseORExpression>, RandomState>,
    pub block_cache: AHashMap<YieldAwaitReturnKey, ParseResult<Block>, RandomState>,
    pub call_expression_cache: AHashMap<YieldAwaitKey, ParseResult<CallExpression>, RandomState>,
    pub catch_parameter_cache: AHashMap<YieldAwaitKey, ParseResult<CatchParameter>, RandomState>,
    pub class_tail_cache: AHashMap<YieldAwaitKey, ParseResult<ClassTail>, RandomState>,
    pub coalesce_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<CoalesceExpression>, RandomState>,
    pub cover_call_expression_and_async_arrow_head_cache: AHashMap<YieldAwaitKey, ParseResult<CoverCallExpressionAndAsyncArrowHead>, RandomState>,
    pub cpeaapl_cache: AHashMap<YieldAwaitKey, ParseResult<CoverParenthesizedExpressionAndArrowParameterList>, RandomState>,
    pub elision_cache: AHashMap<Scanner, ParseResult<Elisions>, RandomState>,
    pub expression_body_cache: AHashMap<InAwaitKey, ParseResult<ExpressionBody>, RandomState>,
    pub expression_cache: AHashMap<InYieldAwaitKey, ParseResult<Expression>, RandomState>,
    pub for_binding_cache: AHashMap<YieldAwaitKey, ParseResult<ForBinding>, RandomState>,
    pub formal_parameter_cache: AHashMap<YieldAwaitKey, ParseResult<FormalParameter>, RandomState>,
    pub formal_parameters_cache: AHashMap<YieldAwaitKey, (Rc<FormalParameters>, Scanner), RandomState>,
    pub function_body_cache: AHashMap<YieldAwaitKey, (Rc<FunctionBody>, Scanner), RandomState>,
    pub function_declaration_cache: AHashMap<YieldAwaitDefaultKey, ParseResult<FunctionDeclaration>, RandomState>,
    pub generator_body_cache: AHashMap<Scanner, (Rc<GeneratorBody>, Scanner), RandomState>,
    pub identifier_cache: AHashMap<Scanner, ParseResult<Identifier>, RandomState>,
    pub identifier_reference_cache: AHashMap<YieldAwaitKey, ParseResult<IdentifierReference>, RandomState>,
    pub initializer_cache: AHashMap<InYieldAwaitKey, ParseResult<Initializer>, RandomState>,
    pub label_identifier_cache: AHashMap<YieldAwaitKey, ParseResult<LabelIdentifier>, RandomState>,
    pub lexical_declaration_cache: AHashMap<InYieldAwaitKey, ParseResult<LexicalDeclaration>, RandomState>,
    pub lhs_cache: AHashMap<YieldAwaitKey, ParseResult<LeftHandSideExpression>, RandomState>,
    pub lpn_cache: AHashMap<Scanner, ParseResult<LiteralPropertyName>, RandomState>,
    pub member_expression_cache: AHashMap<YieldAwaitKey, ParseResult<MemberExpression>, RandomState>,
    pub meta_property_cache: AHashMap<Scanner, ParseResult<MetaProperty>, RandomState>,
    pub method_definition_cache: AHashMap<YieldAwaitKey, ParseResult<MethodDefinition>, RandomState>,
    pub property_name_cache: AHashMap<YieldAwaitKey, ParseResult<PropertyName>, RandomState>,
    pub single_name_binding_cache: AHashMap<YieldAwaitKey, ParseResult<SingleNameBinding>, RandomState>,
    pub statement_cache: AHashMap<YieldAwaitReturnKey, ParseResult<Statement>, RandomState>,
    pub statement_list_cache: AHashMap<YieldAwaitReturnKey, ParseResult<StatementList>, RandomState>,
    pub template_literal_cache: AHashMap<YieldAwaitTaggedKey, ParseResult<TemplateLiteral>, RandomState>,
    pub unary_expression_cache: AHashMap<YieldAwaitKey, ParseResult<UnaryExpression>, RandomState>,
    pub unique_formal_parameters_cache: AHashMap<YieldAwaitKey, (Rc<UniqueFormalParameters>, Scanner), RandomState>,
    pub update_expression_cache: AHashMap<YieldAwaitKey, ParseResult<UpdateExpression>, RandomState>,
    pub variable_declaration_list_cache: AHashMap<InYieldAwaitKey, ParseResult<VariableDeclarationList>, RandomState>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, direct: bool, goal: ParseGoal) -> Self {
        Self { source, direct, goal, ..Default::default() }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Default)]
pub struct Span {
    pub starting_index: usize,
    pub length: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub struct Location {
    pub starting_line: u32,
    pub starting_column: u32,
    pub span: Span,
}

impl Default for Location {
    fn default() -> Self {
        Location { starting_line: 1, starting_column: 1, span: Span::default() }
    }
}

impl From<&Scanner> for Location {
    fn from(src: &Scanner) -> Location {
        Location::from(*src)
    }
}

#[cfg(test)]
impl From<u32> for Location {
    fn from(src: u32) -> Self {
        Location { starting_line: 1, starting_column: src, span: Span { starting_index: src as usize - 1, length: 0 } }
    }
}
#[cfg(test)]
impl From<(u32, u32)> for Location {
    fn from(src: (u32, u32)) -> Self {
        let (line, column) = src;
        // This "all previous lines are 256 chars" is a bit unrealistic, but it makes for unsurprising tests. (We can't
        // guarantee, in a test context, that the values of line & column are consistent with starting index. Line 20,
        // column 10 is definitely after line 10 column 50, but if all we're doing is comparing starting indexes, how
        // do we know? Making lines really long helps that intuition make better tests.)
        Location { starting_line: line, starting_column: column, span: Span { starting_index: (line - 1) as usize * 256 + column as usize, length: 0 } }
    }
}

impl From<Scanner> for Location {
    fn from(src: Scanner) -> Location {
        Location { starting_line: src.line, starting_column: src.column, span: Span { starting_index: src.start_idx, length: 0 } }
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.span.starting_index.cmp(&other.span.starting_index))
    }
}
impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        self.span.starting_index.cmp(&other.span.starting_index)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PECode {
    Generic,
    EoFExpected,
    ImproperNewline,
    InvalidIdentifier,
    KeywordExpected(Keyword),
    KeywordUsedAsIdentifier(Keyword),
    OneOfKeywordExpected(Vec<Keyword>),
    OneOfPunctuatorExpected(Vec<Punctuator>),
    PunctuatorExpected(Punctuator),
    AssignmentExpressionOrSpreadElementExpected,
    CommaLeftBracketElementListExpected,
    IdentifierStringNumberExpected,
    ExpressionSpreadOrRPExpected,
    BindingIdOrPatternExpected,
    NewOrMEExpected,
    ChainFailed,
    IdOrFormalsExpected,
    ObjectAssignmentPatternEndFailure,
    ArrayAssignmentPatternEndFailure,
    IdRefOrPropertyNameExpected,
    InvalidCoalesceExpression,
    ImproperExpression,
    DeclarationOrStatementExpected,
    ParseNodeExpected(ParseNodeKind),
    OpenOrIdentExpected,
    ForStatementDefinitionError,
    ForInOfDefinitionError,
    CaseBlockCloseExpected,
    TryBlockError,
}

impl fmt::Display for PECode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PECode::Generic => f.write_str("error"),
            PECode::EoFExpected => f.write_str("end-of-file expected"),
            PECode::ImproperNewline => f.write_str("newline not allowed here"),
            PECode::InvalidIdentifier => f.write_str("not an identifier"),
            PECode::KeywordExpected(kwd) => write!(f, "‘{}’ expected", kwd),
            PECode::KeywordUsedAsIdentifier(kwd) => write!(f, "‘{}’ is a reserved word and may not be used as an identifier", kwd),
            PECode::OneOfKeywordExpected(kwd_set) => write!(f, "one of [{}] expected", itertools::join(kwd_set.iter().map(|&kwd| format!("‘{}’", kwd)), ", ")),
            PECode::OneOfPunctuatorExpected(punct_set) => write!(f, "one of [{}] expected", itertools::join(punct_set.iter().map(|&p| format!("‘{}’", p)), ", ")),
            PECode::PunctuatorExpected(p) => write!(f, "‘{}’ expected", p),
            PECode::AssignmentExpressionOrSpreadElementExpected => f.write_str("AssignmentExpression or SpreadElement expected"),
            PECode::CommaLeftBracketElementListExpected => f.write_str("‘,’, ‘]’, or an ElementList expected"),
            PECode::IdentifierStringNumberExpected => f.write_str("Identifier, String, or Number expected"),
            PECode::ExpressionSpreadOrRPExpected => f.write_str("Expression, spread pattern, or closing paren expected"),
            PECode::BindingIdOrPatternExpected => f.write_str("BindingIdentifier or BindingPattern expected"),
            PECode::NewOrMEExpected => f.write_str("‘new’ or MemberExpression expected"),
            PECode::ChainFailed => f.write_str("‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)"),
            PECode::IdOrFormalsExpected => f.write_str("Identifier or Formal Parameters expected"),
            PECode::ObjectAssignmentPatternEndFailure => f.write_str("‘}’, an AssignmentRestProperty, or an AssignmentPropertyList expected"),
            PECode::ArrayAssignmentPatternEndFailure => f.write_str("‘,’, ‘]’, or an AssignmentElementList expected"),
            PECode::IdRefOrPropertyNameExpected => f.write_str("IdentifierReference or PropertyName expected"),
            PECode::InvalidCoalesceExpression => f.write_str("Invalid Coalesce Expression"),
            PECode::ImproperExpression => f.write_str("Improper Expression"),
            PECode::DeclarationOrStatementExpected => f.write_str("Declaration or Statement expected"),
            PECode::ParseNodeExpected(pn) => write!(f, "{} expected", pn),
            PECode::OpenOrIdentExpected => f.write_str("‘[’, ‘{’, or an identifier expected"),
            PECode::ForStatementDefinitionError => f.write_str("‘var’, LexicalDeclaration, or Expression expected"),
            PECode::ForInOfDefinitionError => f.write_str("‘let’, ‘var’, or a LeftHandSideExpression expected"),
            PECode::CaseBlockCloseExpected => f.write_str("‘}’, ‘case’, or ‘default’ expected"),
            PECode::TryBlockError => f.write_str("Catch or Finally block expected"),
        }
    }
}
impl Default for PECode {
    fn default() -> Self {
        PECode::Generic
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ParseError {
    code: PECode,
    location: Location,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.code.fmt(f)
    }
}
impl Error for ParseError {}
impl ParseError {
    pub fn compare(left: &ParseError, right: &ParseError) -> Ordering {
        left.location.cmp(&right.location)
    }
    pub fn new(code: PECode, location: impl Into<Location>) -> Self {
        Self { code, location: location.into() }
    }
    pub fn compare_option(left: &Option<Self>, right: &Option<Self>) -> Ordering {
        let location_left = left.as_ref().map(|pe| &pe.location);
        let location_right = right.as_ref().map(|pe| &pe.location);

        location_left.cmp(&location_right)
    }
    #[cfg(test)]
    pub fn unpack(&self, loc: impl Into<Location>) -> (PECode, i32) {
        let expected_loc = loc.into();
        let spot = self.location.starting_column as i32 - expected_loc.starting_column as i32;
        (self.code.clone(), spot)
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
pub trait Otherwise<T, E> {
    fn otherwise<O>(self, f: O) -> Result<T, E>
    where
        O: FnOnce() -> Result<T, E>;
}

impl<T> Otherwise<T, ParseError> for Result<T, ParseError> {
    fn otherwise<O>(self, f: O) -> Self
    where
        O: FnOnce() -> Result<T, ParseError>,
    {
        self.or_else(|err1| f().map_err(|err2| cmp::max_by(err2, err1, ParseError::compare)))
    }
}

pub trait IsFunctionDefinition {
    fn is_function_definition(&self) -> bool;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ATTKind {
    Invalid,
    Simple,
}

pub fn scan_for_punct(scanner: Scanner, src: &str, goal: ScanGoal, punct: Punctuator) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(punct) {
        Ok(after_tok)
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(punct), scanner))
    }
}

pub fn scan_for_punct_set(scanner: Scanner, src: &str, goal: ScanGoal, punct_set: &[Punctuator]) -> Result<(Punctuator, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&p) = punct_set.iter().find(|&p| tok.matches_punct(*p)) {
        Ok((p, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfPunctuatorExpected(punct_set.to_vec()), scanner))
    }
}

pub fn scan_for_auto_semi(scanner: Scanner, src: &str, goal: ScanGoal) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(Punctuator::Semicolon) {
        Ok(after_tok)
    } else if tok.matches_punct(Punctuator::RightBrace) || tok == Token::Eof || after_tok.line > scanner.line {
        // @@@ This is checking the end of the token, not the start of the token, so this is broken for multi-line token parsing
        Ok(scanner)
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), scanner))
    }
}

pub fn scan_for_keyword(scanner: Scanner, src: &str, goal: ScanGoal, kwd: Keyword) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_keyword(kwd) {
        Ok(after_tok)
    } else {
        Err(ParseError::new(PECode::KeywordExpected(kwd), scanner))
    }
}

pub fn scan_for_keywords(scanner: Scanner, src: &str, goal: ScanGoal, kwds: &[Keyword]) -> Result<(Keyword, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&k) = kwds.iter().find(|&k| tok.matches_keyword(*k)) {
        Ok((k, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfKeywordExpected(kwds.to_vec()), scanner))
    }
}

pub fn scan_for_identifiername(scanner: Scanner, src: &str, goal: ScanGoal) -> Result<(IdentifierData, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Token::Identifier(id) = tok {
        Ok((id, after_tok))
    } else {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::IdentifierName), scanner))
    }
}

pub fn scan_for_private_identifier(scanner: Scanner, src: &str, goal: ScanGoal) -> Result<(IdentifierData, Scanner), ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, goal);
    if let Token::PrivateIdentifier(id) = tok {
        Ok((id, after_tok))
    } else {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrivateIdentifier), scanner))
    }
}

pub fn scan_for_eof(scanner: Scanner, src: &str) -> Result<Scanner, ParseError> {
    let (tok, after_tok) = scan_token(&scanner, src, ScanGoal::InputElementDiv);
    if tok == Token::Eof {
        Ok(after_tok)
    } else {
        Err(ParseError::new(PECode::EoFExpected, scanner))
    }
}

//no_line_terminator(after_cont, parser.source)?;
// If there is no newline sequence between the scanner's spot and the _end_ of the next token, return Ok
// else return Err.
//
// (Note that this is really supposed to be between the current spot and the _start_ of the next token,
// but the scanner doesn't support that yet. Some tokens span more than one line.)
pub fn no_line_terminator(scanner: Scanner, src: &str) -> Result<(), ParseError> {
    let (_, after_tok) = scan_token(&scanner, src, ScanGoal::InputElementDiv);
    if after_tok.line == scanner.line {
        Ok(())
    } else {
        Err(ParseError::new(PECode::ImproperNewline, scanner))
    }
}

// 11.1.6 Static Semantics: ParseText ( sourceText, goalSymbol )
//
// The abstract operation ParseText takes arguments sourceText (a sequence of Unicode code points) and goalSymbol (a
// nonterminal in one of the ECMAScript gramma =rs). It performs the following steps when called:
//
// 1. Attempt to parse sourceText using goalSymbol as the goal symbol, and analyse the parse result for any early error
//    conditions. Parsing and early error detection may be interleaved in an implementation-defined manner.
// 2. If the parse succeeded and no early errors were found, return the Parse Node (an instance of goalSymbol) at the
//    root of the parse tree resulting from the parse.
// 3. Otherwise, return a List of one or more SyntaxError objects representing the parsing errors and/or early errors.
//    If more than one parsing error or early error is present, the number and ordering of error objects in the list is
//    implementation-defined, but at least one must be present.
use super::agent::Agent;
use super::errors::create_syntax_error_object;
use super::object::Object;
use scripts::Script;

pub enum ParsedText {
    Errors(Vec<Object>),
    Script(Rc<Script>),
    // ... more to come
}

pub fn parse_text(agent: &mut Agent, src: &str, goal_symbol: ParseGoal) -> ParsedText {
    let mut parser = Parser::new(src, false, goal_symbol);
    match goal_symbol {
        ParseGoal::Script => {
            let potential_script = Script::parse(&mut parser, Scanner::new());
            match potential_script {
                Err(pe) => {
                    let syntax_error = create_syntax_error_object(agent, format!("{}:{}: {}", pe.location.starting_line, pe.location.starting_column, pe).as_str());
                    ParsedText::Errors(vec![syntax_error])
                }
                Ok((node, _)) => {
                    let mut errs = vec![];
                    node.early_errors(agent, &mut errs);
                    if errs.is_empty() {
                        ParsedText::Script(node)
                    } else {
                        ParsedText::Errors(errs)
                    }
                }
            }
        }
        _ => todo!(),
    }
}

pub fn duplicates(idents: &[JSString]) -> Vec<&JSString> {
    // Given a vector of strings (probably identifiers), produce a vector of all the duplicates. Each item in the
    // return value is unique (there are no duplicates there!), and ordered by the location of the _second_ occurence
    // within the source array.
    //
    // Example: duplicates(&["a", "b", "c", "b", "a"]) -> vec!["b", "a"]
    fn second_spot(needle: &JSString, src: &[JSString]) -> Option<usize> {
        let mut iter = src.iter().enumerate().filter(|&(_, s)| s == needle);
        iter.next();
        iter.next().map(|(n, _)| n)
    }
    let mut duplicates = idents.iter().collect::<Counter<_>>().into_iter().filter(|&(_, n)| n > 1).map(|(s, _)| (s, second_spot(s, idents).unwrap())).collect::<Vec<_>>();

    duplicates.sort_unstable_by_key(|&(_, n)| n);

    duplicates.into_iter().map(|(s, _)| s).collect::<Vec<_>>()
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
pub mod testhelp;

#[cfg(test)]
mod tests;

use super::*;
use ahash::AHashMap;
use ahash::RandomState;
use anyhow::anyhow;
use counter::Counter;
use std::cmp;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum YieldAllowed {
    Yes,
    #[default]
    No,
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub enum AwaitAllowed {
    Yes,
    #[default]
    No,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub enum ParseGoal {
    #[default]
    Script,
    Module,
    FormalParameters(YieldAllowed, AwaitAllowed),
    FunctionBody(YieldAllowed, AwaitAllowed),
    GeneratorBody,
    AsyncFunctionBody,
    AsyncGeneratorBody,
    FunctionExpression,
    GeneratorExpression,
    AsyncFunctionExpression,
    AsyncGeneratorExpression,
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
    Elisions,
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
            ParseNodeKind::Elisions => "Elisions",
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
    pub cover_call_expression_and_async_arrow_head_cache:
        AHashMap<YieldAwaitKey, ParseResult<CoverCallExpressionAndAsyncArrowHead>, RandomState>,
    pub cpeaapl_cache:
        AHashMap<YieldAwaitKey, ParseResult<CoverParenthesizedExpressionAndArrowParameterList>, RandomState>,
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

impl From<Scanner> for Location {
    fn from(src: Scanner) -> Location {
        Location {
            starting_line: src.line,
            starting_column: src.column,
            span: Span { starting_index: src.start_idx, length: 0 },
        }
    }
}

impl From<(&Scanner, &Scanner)> for Location {
    fn from(start_end_pair: (&Scanner, &Scanner)) -> Self {
        let (start, end) = start_end_pair;
        Location {
            starting_line: start.line,
            starting_column: start.column,
            span: Span { starting_index: start.start_idx, length: end.start_idx - start.start_idx },
        }
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        self.span.starting_index.cmp(&other.span.starting_index)
    }
}

impl Location {
    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        assert!(self.span.starting_index <= other.span.starting_index + other.span.length);
        Location {
            starting_line: self.starting_line,
            starting_column: self.starting_column,
            span: Span {
                starting_index: self.span.starting_index,
                length: other.span.starting_index + other.span.length - self.span.starting_index,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Default)]
pub enum PECode {
    #[default]
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
            PECode::KeywordExpected(kwd) => write!(f, "‘{kwd}’ expected"),
            PECode::KeywordUsedAsIdentifier(kwd) => {
                write!(f, "‘{kwd}’ is a reserved word and may not be used as an identifier")
            }
            PECode::OneOfKeywordExpected(kwd_set) => {
                write!(f, "one of [{}] expected", itertools::join(kwd_set.iter().map(|&kwd| format!("‘{kwd}’")), ", "))
            }
            PECode::OneOfPunctuatorExpected(punct_set) => {
                write!(f, "one of [{}] expected", itertools::join(punct_set.iter().map(|&p| format!("‘{p}’")), ", "))
            }
            PECode::PunctuatorExpected(p) => write!(f, "‘{p}’ expected"),
            PECode::AssignmentExpressionOrSpreadElementExpected => {
                f.write_str("AssignmentExpression or SpreadElement expected")
            }
            PECode::CommaLeftBracketElementListExpected => f.write_str("‘,’, ‘]’, or an ElementList expected"),
            PECode::IdentifierStringNumberExpected => f.write_str("Identifier, String, or Number expected"),
            PECode::ExpressionSpreadOrRPExpected => {
                f.write_str("Expression, spread pattern, or closing paren expected")
            }
            PECode::BindingIdOrPatternExpected => f.write_str("BindingIdentifier or BindingPattern expected"),
            PECode::NewOrMEExpected => f.write_str("‘new’ or MemberExpression expected"),
            PECode::ChainFailed => {
                f.write_str("‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)")
            }
            PECode::IdOrFormalsExpected => f.write_str("Identifier or Formal Parameters expected"),
            PECode::ObjectAssignmentPatternEndFailure => {
                f.write_str("‘}’, an AssignmentRestProperty, or an AssignmentPropertyList expected")
            }
            PECode::ArrayAssignmentPatternEndFailure => f.write_str("‘,’, ‘]’, or an AssignmentElementList expected"),
            PECode::IdRefOrPropertyNameExpected => f.write_str("IdentifierReference or PropertyName expected"),
            PECode::InvalidCoalesceExpression => f.write_str("Invalid Coalesce Expression"),
            PECode::ImproperExpression => f.write_str("Improper Expression"),
            PECode::DeclarationOrStatementExpected => f.write_str("Declaration or Statement expected"),
            PECode::ParseNodeExpected(pn) => write!(f, "{pn} expected"),
            PECode::OpenOrIdentExpected => f.write_str("‘[’, ‘{’, or an identifier expected"),
            PECode::ForStatementDefinitionError => f.write_str("‘var’, LexicalDeclaration, or Expression expected"),
            PECode::ForInOfDefinitionError => f.write_str("‘let’, ‘var’, or a LeftHandSideExpression expected"),
            PECode::CaseBlockCloseExpected => f.write_str("‘}’, ‘case’, or ‘default’ expected"),
            PECode::TryBlockError => f.write_str("Catch or Finally block expected"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
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
    pub fn compare_option(left: Option<&Self>, right: Option<&Self>) -> Ordering {
        let location_left = left.map(|pe| &pe.location);
        let location_right = right.map(|pe| &pe.location);

        location_left.cmp(&location_right)
    }
}

// =======
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ATTKind {
    Invalid,
    Simple,
}

pub fn scan_for_punct(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
    punct: Punctuator,
) -> Result<(Location, Scanner), ParseError> {
    let (tok, token_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(punct) {
        Ok((token_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(punct), token_loc))
    }
}

pub fn scan_for_punct_set(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
    punct_set: &[Punctuator],
) -> Result<(Punctuator, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&p) = punct_set.iter().find(|&p| tok.matches_punct(*p)) {
        Ok((p, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfPunctuatorExpected(punct_set.to_vec()), tok_loc))
    }
}

pub fn scan_for_auto_semi(scanner: Scanner, src: &str, goal: ScanGoal) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(Punctuator::Semicolon) {
        Ok((tok_loc, after_tok))
    } else if tok.matches_punct(Punctuator::RightBrace) || tok == Token::Eof || tok_loc.starting_line > scanner.line {
        Ok((Location::from(&scanner), scanner))
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), tok_loc))
    }
}

pub fn scan_for_keyword(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
    kwd: Keyword,
) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_keyword(kwd) {
        Ok((tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::KeywordExpected(kwd), tok_loc))
    }
}

pub fn scan_for_keywords(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
    kwds: &[Keyword],
) -> Result<(Keyword, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&k) = kwds.iter().find(|&k| tok.matches_keyword(*k)) {
        Ok((k, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfKeywordExpected(kwds.to_vec()), tok_loc))
    }
}

pub fn scan_for_identifiername(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
) -> Result<(IdentifierData, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Token::Identifier(id) = tok {
        Ok((id, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::IdentifierName), tok_loc))
    }
}

pub fn scan_for_private_identifier(
    scanner: Scanner,
    src: &str,
    goal: ScanGoal,
) -> Result<(IdentifierData, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Token::PrivateIdentifier(id) = tok {
        Ok((id, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrivateIdentifier), tok_loc))
    }
}

pub fn scan_for_eof(scanner: Scanner, src: &str) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, ScanGoal::InputElementDiv);
    if tok == Token::Eof { Ok((tok_loc, after_tok)) } else { Err(ParseError::new(PECode::EoFExpected, tok_loc)) }
}

//no_line_terminator(after_cont, parser.source)?;
// If there is no newline sequence between the scanner's spot and the start of the next token, return Ok
// else return Err.
pub fn no_line_terminator(scanner: Scanner, src: &str) -> Result<(), ParseError> {
    let (_, tok_loc, _) = scan_token(&scanner, src, ScanGoal::InputElementDiv);
    if tok_loc.starting_line == scanner.line { Ok(()) } else { Err(ParseError::new(PECode::ImproperNewline, scanner)) }
}

// 11.1.6 Static Semantics: ParseText ( sourceText, goalSymbol )
//
// The abstract operation ParseText takes arguments sourceText (a sequence of Unicode code points) and goalSymbol (a
// nonterminal in one of the ECMAScript grammars). It performs the following steps when called:
//
// 1. Attempt to parse sourceText using goalSymbol as the goal symbol, and analyse the parse result for any early error
//    conditions. Parsing and early error detection may be interleaved in an implementation-defined manner.
// 2. If the parse succeeded and no early errors were found, return the Parse Node (an instance of goalSymbol) at the
//    root of the parse tree resulting from the parse.
// 3. Otherwise, return a List of one or more SyntaxError objects representing the parsing errors and/or early errors.
//    If more than one parsing error or early error is present, the number and ordering of error objects in the list is
//    implementation-defined, but at least one must be present.

#[derive(Debug)]
pub enum ParsedText {
    Errors(Vec<Object>),
    Script(Rc<Script>),
    FormalParameters(Rc<FormalParameters>),
    FunctionBody(Rc<FunctionBody>),
    GeneratorBody(Rc<GeneratorBody>),
    AsyncFunctionBody(Rc<AsyncFunctionBody>),
    AsyncGeneratorBody(Rc<AsyncGeneratorBody>),
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorExpression(Rc<GeneratorExpression>),
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
    // ... more to come
}

impl TryFrom<ParsedText> for Result<Rc<Script>, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::Script(sr) => Ok(Ok(sr)),
            _ => Err(anyhow!("Expected a Script or Syntax Errors")),
        }
    }
}

impl TryFrom<ParsedText> for Result<Rc<FormalParameters>, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::FormalParameters(fp) => Ok(Ok(fp)),
            _ => Err(anyhow!("Expected FormalParameters or Syntax Errors")),
        }
    }
}

pub enum ParsedBody {
    FunctionBody(Rc<FunctionBody>),
    GeneratorBody(Rc<GeneratorBody>),
    AsyncFunctionBody(Rc<AsyncFunctionBody>),
    AsyncGeneratorBody(Rc<AsyncGeneratorBody>),
}
impl TryFrom<ParsedText> for Result<ParsedBody, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::FunctionBody(node) => Ok(Ok(ParsedBody::FunctionBody(node))),
            ParsedText::GeneratorBody(node) => Ok(Ok(ParsedBody::GeneratorBody(node))),
            ParsedText::AsyncFunctionBody(node) => Ok(Ok(ParsedBody::AsyncFunctionBody(node))),
            ParsedText::AsyncGeneratorBody(node) => Ok(Ok(ParsedBody::AsyncGeneratorBody(node))),
            _ => Err(anyhow!("Expected Some kind of function body or Syntax Errors")),
        }
    }
}
impl TryFrom<BodySource> for ParsedBody {
    type Error = anyhow::Error;

    fn try_from(value: BodySource) -> Result<Self, Self::Error> {
        match value {
            BodySource::Function(function_body) => Ok(Self::FunctionBody(function_body)),
            BodySource::Generator(generator_body) => Ok(Self::GeneratorBody(generator_body)),
            BodySource::AsyncFunction(async_function_body) => Ok(Self::AsyncFunctionBody(async_function_body)),
            BodySource::AsyncGenerator(async_generator_body) => Ok(Self::AsyncGeneratorBody(async_generator_body)),
            _ => Err(anyhow!("Some kind of normal function body expected")),
        }
    }
}

pub enum ParsedFunctionExpression {
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorExpression(Rc<GeneratorExpression>),
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
}
impl TryFrom<ParsedText> for Result<ParsedFunctionExpression, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::FunctionExpression(node) => Ok(Ok(ParsedFunctionExpression::FunctionExpression(node))),
            ParsedText::GeneratorExpression(node) => Ok(Ok(ParsedFunctionExpression::GeneratorExpression(node))),
            ParsedText::AsyncFunctionExpression(node) => {
                Ok(Ok(ParsedFunctionExpression::AsyncFunctionExpression(node)))
            }
            ParsedText::AsyncGeneratorExpression(node) => {
                Ok(Ok(ParsedFunctionExpression::AsyncGeneratorExpression(node)))
            }
            _ => Err(anyhow!("Expected Some kind of function expression or Syntax Errors")),
        }
    }
}

enum ParsedItem {
    Script(Rc<Script>),
    FormalParameters(Rc<FormalParameters>),
    FunctionBody(Rc<FunctionBody>),
    GeneratorBody(Rc<GeneratorBody>),
    AsyncFunctionBody(Rc<AsyncFunctionBody>),
    AsyncGeneratorBody(Rc<AsyncGeneratorBody>),
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorExpression(Rc<GeneratorExpression>),
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
}

impl ParsedItem {
    fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ParsedItem::Script(s) => s.early_errors(errs, strict),
            ParsedItem::FormalParameters(fp) => fp.early_errors(errs, strict, false),
            ParsedItem::FunctionBody(fb) => fb.early_errors(errs, strict),
            ParsedItem::GeneratorBody(gb) => gb.early_errors(errs, strict),
            ParsedItem::AsyncFunctionBody(body) => body.early_errors(errs, strict),
            ParsedItem::AsyncGeneratorBody(body) => body.early_errors(errs, strict),
            ParsedItem::FunctionExpression(exp) => exp.early_errors(errs, strict),
            ParsedItem::GeneratorExpression(exp) => exp.early_errors(errs, strict),
            ParsedItem::AsyncFunctionExpression(exp) => exp.early_errors(errs, strict),
            ParsedItem::AsyncGeneratorExpression(exp) => exp.early_errors(errs, strict),
        }
    }
}

impl From<ParsedItem> for ParsedText {
    fn from(value: ParsedItem) -> Self {
        match value {
            ParsedItem::Script(s) => ParsedText::Script(s),
            ParsedItem::FormalParameters(fp) => ParsedText::FormalParameters(fp),
            ParsedItem::FunctionBody(fb) => ParsedText::FunctionBody(fb),
            ParsedItem::GeneratorBody(gb) => ParsedText::GeneratorBody(gb),
            ParsedItem::AsyncFunctionBody(body) => ParsedText::AsyncFunctionBody(body),
            ParsedItem::AsyncGeneratorBody(body) => ParsedText::AsyncGeneratorBody(body),
            ParsedItem::FunctionExpression(exp) => ParsedText::FunctionExpression(exp),
            ParsedItem::GeneratorExpression(exp) => ParsedText::GeneratorExpression(exp),
            ParsedItem::AsyncFunctionExpression(exp) => ParsedText::AsyncFunctionExpression(exp),
            ParsedItem::AsyncGeneratorExpression(exp) => ParsedText::AsyncGeneratorExpression(exp),
        }
    }
}

type ParseClosure = Box<dyn FnOnce(&mut Parser) -> Result<(ParsedItem, Scanner), ParseError>>;

fn direct_parse(parser: &mut Parser, strict: bool, parse_fn: ParseClosure, name: &str) -> ParsedText {
    match parse_fn(parser) {
        Ok((item, scan)) => {
            // advance scanner past trailing whitespace.
            let scan = match skip_skippables(&scan, parser.source) {
                Err(pe) => {
                    return ParsedText::Errors(vec![create_syntax_error_object(pe, None)]);
                }
                Ok(scan) => scan,
            };
            if scan.start_idx == parser.source.len() {
                // input text was completely consumed
                let mut errs = vec![];
                item.early_errors(&mut errs, strict);
                if errs.is_empty() { ParsedText::from(item) } else { ParsedText::Errors(errs) }
            } else {
                ParsedText::Errors(vec![create_syntax_error_object(format!("{name} had unparsed trailing text"), None)])
            }
        }
        Err(pe) => {
            let syntax_error = create_syntax_error_object(
                format!("{}:{}: {}", pe.location.starting_line, pe.location.starting_column, pe).as_str(),
                Some(pe.location),
            );
            ParsedText::Errors(vec![syntax_error])
        }
    }
}

pub fn parse_text(src: &str, goal_symbol: ParseGoal, strict: bool, direct: bool) -> ParsedText {
    let mut parser = Parser::new(src, direct, goal_symbol);
    let (closure, name): (ParseClosure, &str) = match goal_symbol {
        ParseGoal::Script => (
            Box::new(|parser: &mut Parser| {
                Script::parse(parser, Scanner::new()).map(|(item, scanner)| (ParsedItem::Script(item), scanner))
            }),
            "script",
        ),
        ParseGoal::Module => todo!(),
        ParseGoal::FormalParameters(yield_state, await_state) => (
            Box::new(move |parser: &mut Parser| {
                let (fp, scan) = FormalParameters::parse(
                    parser,
                    Scanner::new(),
                    yield_state == YieldAllowed::Yes,
                    await_state == AwaitAllowed::Yes,
                );
                Ok((ParsedItem::FormalParameters(fp), scan))
            }),
            "parameters",
        ),
        ParseGoal::FunctionBody(yield_state, await_state) => (
            Box::new(move |parser: &mut Parser| {
                let (body, scanner) = FunctionBody::parse(
                    parser,
                    Scanner::new(),
                    yield_state == YieldAllowed::Yes,
                    await_state == AwaitAllowed::Yes,
                );
                Ok((ParsedItem::FunctionBody(body), scanner))
            }),
            "function body",
        ),
        ParseGoal::GeneratorBody => (
            Box::new(|parser: &mut Parser| {
                let (body, scanner) = GeneratorBody::parse(parser, Scanner::new());
                Ok((ParsedItem::GeneratorBody(body), scanner))
            }),
            "generator body",
        ),
        ParseGoal::AsyncFunctionBody => (
            Box::new(|parser: &mut Parser| {
                let (body, scanner) = AsyncFunctionBody::parse(parser, Scanner::new());
                Ok((ParsedItem::AsyncFunctionBody(body), scanner))
            }),
            "function body",
        ),
        ParseGoal::AsyncGeneratorBody => (
            Box::new(|parser: &mut Parser| {
                let (body, scanner) = AsyncGeneratorBody::parse(parser, Scanner::new());
                Ok((ParsedItem::AsyncGeneratorBody(body), scanner))
            }),
            "generator body",
        ),
        ParseGoal::FunctionExpression => (
            Box::new(|parser: &mut Parser| {
                FunctionExpression::parse(parser, Scanner::new())
                    .map(|(item, scanner)| (ParsedItem::FunctionExpression(item), scanner))
            }),
            "function expression",
        ),
        ParseGoal::GeneratorExpression => (
            Box::new(|parser: &mut Parser| {
                GeneratorExpression::parse(parser, Scanner::new())
                    .map(|(item, scanner)| (ParsedItem::GeneratorExpression(item), scanner))
            }),
            "generator expression",
        ),
        ParseGoal::AsyncFunctionExpression => (
            Box::new(|parser: &mut Parser| {
                AsyncFunctionExpression::parse(parser, Scanner::new())
                    .map(|(item, scanner)| (ParsedItem::AsyncFunctionExpression(item), scanner))
            }),
            "function expression",
        ),
        ParseGoal::AsyncGeneratorExpression => (
            Box::new(|parser: &mut Parser| {
                AsyncGeneratorExpression::parse(parser, Scanner::new())
                    .map(|(item, scanner)| (ParsedItem::AsyncGeneratorExpression(item), scanner))
            }),
            "generator expression",
        ),
    };
    direct_parse(&mut parser, strict, closure, name)
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
    let mut duplicates = idents
        .iter()
        .collect::<Counter<_>>()
        .into_iter()
        .filter(|&(_, n)| n > 1)
        .map(|(s, _)| (s, second_spot(s, idents).unwrap()))
        .collect::<Vec<_>>();

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

pub use additive_operators::*;
pub use arrow_function_definitions::*;
pub use assignment_operators::*;
pub use async_arrow_function_definitions::*;
pub use async_function_definitions::*;
pub use async_generator_function_definitions::*;
pub use binary_bitwise_operators::*;
pub use binary_logical_operators::*;
pub use bitwise_shift_operators::*;
pub use block::*;
pub use break_statement::*;
pub use class_definitions::*;
pub use comma_operator::*;
pub use conditional_operator::*;
pub use continue_statement::*;
pub use debugger_statement::*;
pub use declarations_and_variables::*;
pub use empty_statement::*;
pub use equality_operators::*;
pub use exponentiation_operator::*;
pub use expression_statement::*;
pub use function_definitions::*;
pub use generator_function_definitions::*;
pub use identifiers::*;
pub use if_statement::*;
pub use iteration_statements::*;
pub use labelled_statements::*;
pub use left_hand_side_expressions::*;
pub use method_definitions::*;
pub use multiplicative_operators::*;
pub use parameter_lists::*;
pub use primary_expressions::*;
pub use relational_operators::*;
pub use return_statement::*;
pub use scripts::*;
pub use statements_and_declarations::*;
pub use switch_statement::*;
pub use throw_statement::*;
pub use try_statement::*;
pub use unary_operators::*;
pub use update_expressions::*;
pub use with_statement::*;

#[cfg(test)]
pub mod testhelp;

#[cfg(test)]
mod tests;

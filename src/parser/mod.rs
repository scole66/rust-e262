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
pub(crate) enum YieldAllowed {
    Yes,
    #[default]
    No,
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub(crate) enum AwaitAllowed {
    Yes,
    #[default]
    No,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
pub(crate) enum ParseGoal {
    #[default]
    Script,
    Module,
    FormalParameters(YieldAllowed, AwaitAllowed),
    FunctionBody(YieldAllowed, AwaitAllowed),
    GeneratorBody,
    FunctionExpression,
    GeneratorExpression,
    // These are all as-yet-to-be-used, so I'm hiding them behind #[cfg(test)] for the time being
    #[cfg(test)]
    AsyncFunctionBody,
    #[cfg(test)]
    AsyncGeneratorBody,
    #[cfg(test)]
    AsyncFunctionExpression,
    #[cfg(test)]
    AsyncGeneratorExpression,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub(crate) enum ParseNodeKind {
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
    ClassElement,
    ClassElementName,
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

    // Still unused, these are getting hidden behind #[cfg(test)], if in the tests, else commented out
    // AsyncFunctionBody,
    // AsyncGeneratorBody,
    #[cfg(test)]
    ClassBody,
    #[cfg(test)]
    ClassHeritage,
    //GeneratorBody,
    #[cfg(test)]
    ScriptBody,
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
            //ParseNodeKind::AsyncFunctionBody => "AsyncFunctionBody",
            //ParseNodeKind::AsyncGeneratorBody => "AsyncGeneratorBody",
            ParseNodeKind::AwaitExpression => "AwaitExpression",
            ParseNodeKind::BindingElement => "BindingElement",
            ParseNodeKind::BindingPattern => "BindingPattern",
            ParseNodeKind::BindingProperty => "BindingProperty",
            ParseNodeKind::BlockStatement => "BlockStatement",
            ParseNodeKind::BreakableStatement => "BreakableStatement",
            ParseNodeKind::BreakStatement => "BreakStatement",
            ParseNodeKind::CallExpression => "CallExpression",
            ParseNodeKind::CatchParameter => "CatchParameter",
            #[cfg(test)]
            ParseNodeKind::ClassBody => "ClassBody",
            ParseNodeKind::ClassElement => "ClassElement",
            ParseNodeKind::ClassElementName => "ClassElementName",
            #[cfg(test)]
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
            //ParseNodeKind::GeneratorBody => "GeneratorBody",
            ParseNodeKind::HoistableDeclaration => "HoistableDeclaration",
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
            #[cfg(test)]
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
pub(crate) struct YieldAwaitKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) struct YieldAwaitTaggedKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    tagged_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) struct InYieldAwaitKey {
    scanner: Scanner,
    in_flag: bool,
    yield_flag: bool,
    await_flag: bool,
}

//#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
//pub(crate) struct InKey {
//    scanner: Scanner,
//    in_flag: bool,
//}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) struct InAwaitKey {
    scanner: Scanner,
    in_flag: bool,
    await_flag: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) struct YieldAwaitReturnKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    return_flag: bool,
}

//#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
//pub(crate) struct YieldKey {
//    scanner: Scanner,
//    yield_flag: bool,
//}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub(crate) struct YieldAwaitDefaultKey {
    scanner: Scanner,
    yield_flag: bool,
    await_flag: bool,
    default_flag: bool,
}

type ParseResult<T> = Result<(Rc<T>, Scanner), ParseError>;

#[derive(Default)]
pub(crate) struct Parser<'a> {
    pub(crate) source: &'a str,
    pub(crate) direct: bool,
    pub(crate) goal: ParseGoal,
    pub(crate) arguments_cache: AHashMap<YieldAwaitKey, ParseResult<Arguments>, RandomState>,
    pub(crate) arrow_formal_parameters_cache: AHashMap<YieldAwaitKey, ParseResult<ArrowFormalParameters>, RandomState>,
    pub(crate) assignment_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<AssignmentExpression>, RandomState>,
    pub(crate) async_function_body_cache: AHashMap<Scanner, (Rc<AsyncFunctionBody>, Scanner), RandomState>,
    pub(crate) async_generator_body_cache: AHashMap<Scanner, (Rc<AsyncGeneratorBody>, Scanner), RandomState>,
    pub(crate) binding_element_cache: AHashMap<YieldAwaitKey, ParseResult<BindingElement>, RandomState>,
    pub(crate) binding_identifier_cache: AHashMap<YieldAwaitKey, ParseResult<BindingIdentifier>, RandomState>,
    pub(crate) binding_pattern_cache: AHashMap<YieldAwaitKey, ParseResult<BindingPattern>, RandomState>,
    pub(crate) binding_rest_element_cache: AHashMap<YieldAwaitKey, ParseResult<BindingRestElement>, RandomState>,
    pub(crate) binding_rest_property_cache: AHashMap<YieldAwaitKey, ParseResult<BindingRestProperty>, RandomState>,
    pub(crate) bitwise_or_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<BitwiseORExpression>, RandomState>,
    pub(crate) block_cache: AHashMap<YieldAwaitReturnKey, ParseResult<Block>, RandomState>,
    pub(crate) call_expression_cache: AHashMap<YieldAwaitKey, ParseResult<CallExpression>, RandomState>,
    pub(crate) catch_parameter_cache: AHashMap<YieldAwaitKey, ParseResult<CatchParameter>, RandomState>,
    pub(crate) class_tail_cache: AHashMap<YieldAwaitKey, ParseResult<ClassTail>, RandomState>,
    pub(crate) coalesce_expression_cache: AHashMap<InYieldAwaitKey, ParseResult<CoalesceExpression>, RandomState>,
    pub(crate) cover_call_expression_and_async_arrow_head_cache:
        AHashMap<YieldAwaitKey, ParseResult<CoverCallExpressionAndAsyncArrowHead>, RandomState>,
    pub(crate) cpeaapl_cache:
        AHashMap<YieldAwaitKey, ParseResult<CoverParenthesizedExpressionAndArrowParameterList>, RandomState>,
    pub(crate) elision_cache: AHashMap<Scanner, ParseResult<Elisions>, RandomState>,
    pub(crate) expression_body_cache: AHashMap<InAwaitKey, ParseResult<ExpressionBody>, RandomState>,
    pub(crate) expression_cache: AHashMap<InYieldAwaitKey, ParseResult<Expression>, RandomState>,
    pub(crate) for_binding_cache: AHashMap<YieldAwaitKey, ParseResult<ForBinding>, RandomState>,
    pub(crate) formal_parameter_cache: AHashMap<YieldAwaitKey, ParseResult<FormalParameter>, RandomState>,
    pub(crate) formal_parameters_cache: AHashMap<YieldAwaitKey, (Rc<FormalParameters>, Scanner), RandomState>,
    pub(crate) function_body_cache: AHashMap<YieldAwaitKey, (Rc<FunctionBody>, Scanner), RandomState>,
    pub(crate) function_declaration_cache:
        AHashMap<YieldAwaitDefaultKey, ParseResult<FunctionDeclaration>, RandomState>,
    pub(crate) generator_body_cache: AHashMap<Scanner, (Rc<GeneratorBody>, Scanner), RandomState>,
    pub(crate) identifier_cache: AHashMap<Scanner, ParseResult<Identifier>, RandomState>,
    pub(crate) identifier_reference_cache: AHashMap<YieldAwaitKey, ParseResult<IdentifierReference>, RandomState>,
    pub(crate) initializer_cache: AHashMap<InYieldAwaitKey, ParseResult<Initializer>, RandomState>,
    pub(crate) label_identifier_cache: AHashMap<YieldAwaitKey, ParseResult<LabelIdentifier>, RandomState>,
    pub(crate) lexical_declaration_cache: AHashMap<InYieldAwaitKey, ParseResult<LexicalDeclaration>, RandomState>,
    pub(crate) lhs_cache: AHashMap<YieldAwaitKey, ParseResult<LeftHandSideExpression>, RandomState>,
    //pub(crate) lpn_cache: AHashMap<Scanner, ParseResult<LiteralPropertyName>, RandomState>,
    pub(crate) member_expression_cache: AHashMap<YieldAwaitKey, ParseResult<MemberExpression>, RandomState>,
    //pub(crate) meta_property_cache: AHashMap<Scanner, ParseResult<MetaProperty>, RandomState>,
    pub(crate) method_definition_cache: AHashMap<YieldAwaitKey, ParseResult<MethodDefinition>, RandomState>,
    pub(crate) property_name_cache: AHashMap<YieldAwaitKey, ParseResult<PropertyName>, RandomState>,
    pub(crate) single_name_binding_cache: AHashMap<YieldAwaitKey, ParseResult<SingleNameBinding>, RandomState>,
    pub(crate) statement_cache: AHashMap<YieldAwaitReturnKey, ParseResult<Statement>, RandomState>,
    pub(crate) statement_list_cache: AHashMap<YieldAwaitReturnKey, ParseResult<StatementList>, RandomState>,
    pub(crate) template_literal_cache: AHashMap<YieldAwaitTaggedKey, ParseResult<TemplateLiteral>, RandomState>,
    pub(crate) unary_expression_cache: AHashMap<YieldAwaitKey, ParseResult<UnaryExpression>, RandomState>,
    pub(crate) unique_formal_parameters_cache:
        AHashMap<YieldAwaitKey, (Rc<UniqueFormalParameters>, Scanner), RandomState>,
    pub(crate) update_expression_cache: AHashMap<YieldAwaitKey, ParseResult<UpdateExpression>, RandomState>,
    pub(crate) variable_declaration_list_cache:
        AHashMap<InYieldAwaitKey, ParseResult<VariableDeclarationList>, RandomState>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(source: &'a str, direct: bool, goal: ParseGoal) -> Self {
        Self { source, direct, goal, ..Default::default() }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Default)]
pub(crate) struct Span {
    pub(crate) starting_index: usize,
    pub(crate) length: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub(crate) struct Location {
    pub(crate) starting_line: u32,
    pub(crate) starting_column: u32,
    pub(crate) span: Span,
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
    pub(crate) fn merge(&self, other: &Self) -> Self {
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

    pub(crate) fn contains(&self, other: &Self) -> bool {
        self.span.starting_index <= other.span.starting_index
            && (self.span.starting_index + self.span.length) >= (other.span.starting_index + other.span.length)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Default)]
pub(crate) enum PECode {
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
    IdentifierNameExpected,
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
            PECode::IdentifierNameExpected => f.write_str("identifier name expected"),
            PECode::OpenOrIdentExpected => f.write_str("‘[’, ‘{’, or an identifier expected"),
            PECode::ForStatementDefinitionError => f.write_str("‘var’, LexicalDeclaration, or Expression expected"),
            PECode::ForInOfDefinitionError => f.write_str("‘let’, ‘var’, or a LeftHandSideExpression expected"),
            PECode::CaseBlockCloseExpected => f.write_str("‘}’, ‘case’, or ‘default’ expected"),
            PECode::TryBlockError => f.write_str("Catch or Finally block expected"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct ParseError {
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
    pub(crate) fn compare(left: &ParseError, right: &ParseError) -> Ordering {
        left.location.cmp(&right.location)
    }
    pub(crate) fn new(code: PECode, location: impl Into<Location>) -> Self {
        Self { code, location: location.into() }
    }
    pub(crate) fn compare_option(left: Option<&Self>, right: Option<&Self>) -> Ordering {
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
pub(crate) trait Otherwise<T, E> {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum ATTKind {
    Invalid,
    Simple,
}

pub(crate) fn scan_for_punct(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
    punct: Punctuator,
) -> Result<(Location, Scanner), ParseError> {
    let (tok, token_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(punct) {
        Ok((token_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(punct), token_loc))
    }
}

pub(crate) fn scan_for_punct_set(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
    punct_set: &[Punctuator],
) -> Result<(Punctuator, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&p) = punct_set.iter().find(|&p| tok.matches_punct(*p)) {
        Ok((p, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfPunctuatorExpected(punct_set.to_vec()), tok_loc))
    }
}

pub(crate) fn scan_for_auto_semi(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_punct(Punctuator::Semicolon) {
        Ok((tok_loc, after_tok))
    } else if tok.matches_punct(Punctuator::RightBrace) || tok == Token::Eof || tok_loc.starting_line > scanner.line {
        Ok((Location::from(&scanner), scanner))
    } else {
        Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Semicolon), tok_loc))
    }
}

pub(crate) fn scan_for_keyword(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
    kwd: Keyword,
) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if tok.matches_keyword(kwd) {
        Ok((tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::KeywordExpected(kwd), tok_loc))
    }
}

pub(crate) fn scan_for_keywords(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
    kwds: &[Keyword],
) -> Result<(Keyword, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Some(&k) = kwds.iter().find(|&k| tok.matches_keyword(*k)) {
        Ok((k, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::OneOfKeywordExpected(kwds.to_vec()), tok_loc))
    }
}

pub(crate) fn scan_for_identifiername(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
) -> Result<(IdentifierData, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Token::Identifier(id) = tok {
        Ok((id, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::IdentifierNameExpected, tok_loc))
    }
}

pub(crate) fn scan_for_private_identifier(
    scanner: Scanner,
    src: &str,
    goal: InputElementGoal,
) -> Result<(IdentifierData, Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, goal);
    if let Token::PrivateIdentifier(id) = tok {
        Ok((id, tok_loc, after_tok))
    } else {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrivateIdentifier), tok_loc))
    }
}

pub(crate) fn scan_for_eof(scanner: Scanner, src: &str) -> Result<(Location, Scanner), ParseError> {
    let (tok, tok_loc, after_tok) = scan_token(&scanner, src, InputElementGoal::Div);
    if tok == Token::Eof { Ok((tok_loc, after_tok)) } else { Err(ParseError::new(PECode::EoFExpected, tok_loc)) }
}

//no_line_terminator(after_cont, parser.source)?;
// If there is no newline sequence between the scanner's spot and the start of the next token, return Ok
// else return Err.
pub(crate) fn no_line_terminator(scanner: Scanner, src: &str) -> Result<(), ParseError> {
    let (_, tok_loc, _) = scan_token(&scanner, src, InputElementGoal::Div);
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

#[derive(Debug, Clone)]
pub(crate) enum ParsedText {
    Errors(Vec<Object>),
    Empty,
    FormalParameters(Rc<FormalParameters>),
    FunctionBody(Rc<FunctionBody>),
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorBody(Rc<GeneratorBody>),
    GeneratorExpression(Rc<GeneratorExpression>),
    Script(Rc<Script>),

    // The following are all used in testing scenarios, not the actual runtime, so they all get the #[cfg(test)]
    // treatment
    #[cfg(test)]
    AsyncFunctionBody(Rc<AsyncFunctionBody>),
    #[cfg(test)]
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    #[cfg(test)]
    AsyncGeneratorBody(Rc<AsyncGeneratorBody>),
    #[cfg(test)]
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
    #[cfg(test)]
    AdditiveExpression(Rc<AdditiveExpression>),
    #[cfg(test)]
    ArgumentList(Rc<ArgumentList>),
    #[cfg(test)]
    Arguments(Rc<Arguments>),
    #[cfg(test)]
    ArrayAssignmentPattern(Rc<ArrayAssignmentPattern>),
    #[cfg(test)]
    ArrayBindingPattern(Rc<ArrayBindingPattern>),
    #[cfg(test)]
    ArrayLiteral(Rc<ArrayLiteral>),
    #[cfg(test)]
    ArrowFormalParameters(Rc<ArrowFormalParameters>),
    #[cfg(test)]
    ArrowFunction(Rc<ArrowFunction>),
    #[cfg(test)]
    ArrowParameters(Rc<ArrowParameters>),
    #[cfg(test)]
    AssignmentElement(Rc<AssignmentElement>),
    #[cfg(test)]
    AssignmentElementList(Rc<AssignmentElementList>),
    #[cfg(test)]
    AssignmentElisionElement(Rc<AssignmentElisionElement>),
    #[cfg(test)]
    AssignmentExpression(Rc<AssignmentExpression>),
    #[cfg(test)]
    AssignmentPattern(Rc<AssignmentPattern>),
    #[cfg(test)]
    AssignmentProperty(Rc<AssignmentProperty>),
    #[cfg(test)]
    AssignmentPropertyList(Rc<AssignmentPropertyList>),
    #[cfg(test)]
    AssignmentRestElement(Rc<AssignmentRestElement>),
    #[cfg(test)]
    AssignmentRestProperty(Rc<AssignmentRestProperty>),
    #[cfg(test)]
    AsyncArrowBindingIdentifier(Rc<AsyncArrowBindingIdentifier>),
    //#[cfg(test)]
    //AsyncArrowFunction(Rc<AsyncArrowFunction>),
    //#[cfg(test)]
    //AsyncArrowHead(Rc<AsyncArrowHead>),
    //#[cfg(test)]
    //AsyncConciseBody(Rc<AsyncConciseBody>),
    #[cfg(test)]
    AsyncFunctionDeclaration(Rc<AsyncFunctionDeclaration>),
    #[cfg(test)]
    AsyncGeneratorDeclaration(Rc<AsyncGeneratorDeclaration>),
    //#[cfg(test)]
    //AsyncGeneratorMethod(Rc<AsyncGeneratorMethod>),
    //#[cfg(test)]
    //AsyncMethod(Rc<AsyncMethod>),
    //#[cfg(test)]
    //AwaitExpression(Rc<AwaitExpression>),
    #[cfg(test)]
    BindingElement(Rc<BindingElement>),
    #[cfg(test)]
    BindingElementList(Rc<BindingElementList>),
    #[cfg(test)]
    BindingElisionElement(Rc<BindingElisionElement>),
    //#[cfg(test)]
    //BindingIdentifier(Rc<BindingIdentifier>),
    #[cfg(test)]
    BindingList(Rc<BindingList>),
    #[cfg(test)]
    BindingPattern(Rc<BindingPattern>),
    #[cfg(test)]
    BindingProperty(Rc<BindingProperty>),
    #[cfg(test)]
    BindingPropertyList(Rc<BindingPropertyList>),
    #[cfg(test)]
    BindingRestElement(Rc<BindingRestElement>),
    //    #[cfg(test)]
    //    BindingRestProperty(Rc<BindingRestProperty>),
    #[cfg(test)]
    BitwiseANDExpression(Rc<BitwiseANDExpression>),
    #[cfg(test)]
    BitwiseORExpression(Rc<BitwiseORExpression>),
    #[cfg(test)]
    BitwiseXORExpression(Rc<BitwiseXORExpression>),
    #[cfg(test)]
    Block(Rc<Block>),
    #[cfg(test)]
    BlockStatement(Rc<BlockStatement>),
    #[cfg(test)]
    BreakableStatement(Rc<BreakableStatement>),
    //    #[cfg(test)]
    //    BreakStatement(Rc<BreakStatement>),
    #[cfg(test)]
    CallExpression(Rc<CallExpression>),
    #[cfg(test)]
    CallMemberExpression(Rc<CallMemberExpression>),
    #[cfg(test)]
    CaseBlock(Rc<CaseBlock>),
    #[cfg(test)]
    CaseClause(Rc<CaseClause>),
    #[cfg(test)]
    //    CaseClauses(Rc<CaseClauses>),
    //    #[cfg(test)]
    Catch(Rc<Catch>),
    #[cfg(test)]
    CatchParameter(Rc<CatchParameter>),
    //    #[cfg(test)]
    //    ClassBody(Rc<ClassBody>),
    #[cfg(test)]
    ClassDeclaration(Rc<ClassDeclaration>),
    //    #[cfg(test)]
    //    ClassElement(Rc<ClassElement>),
    //    #[cfg(test)]
    //    ClassElementList(Rc<ClassElementList>),
    #[cfg(test)]
    ClassElementName(Rc<ClassElementName>),
    //    #[cfg(test)]
    //    ClassExpression(Rc<ClassExpression>),
    //    #[cfg(test)]
    //    ClassHeritage(Rc<ClassHeritage>),
    //    #[cfg(test)]
    //    ClassStaticBlock(Rc<ClassStaticBlock>),
    #[cfg(test)]
    ClassStaticBlockBody(Rc<ClassStaticBlockBody>),
    #[cfg(test)]
    ClassStaticBlockStatementList(Rc<ClassStaticBlockStatementList>),
    //    #[cfg(test)]
    //    ClassTail(Rc<ClassTail>),
    #[cfg(test)]
    CoalesceExpression(Rc<CoalesceExpression>),
    #[cfg(test)]
    CoalesceExpressionHead(Rc<CoalesceExpressionHead>),
    #[cfg(test)]
    ComputedPropertyName(Rc<ComputedPropertyName>),
    //    #[cfg(test)]
    //    ConciseBody(Rc<ConciseBody>),
    #[cfg(test)]
    ConditionalExpression(Rc<ConditionalExpression>),
    //    #[cfg(test)]
    //    ContinueStatement(Rc<ContinueStatement>),
    //    #[cfg(test)]
    //    CoverInitializedName(Rc<CoverInitializedName>),
    //    #[cfg(test)]
    //    CoverParenthesizedExpressionAndArrowParameterList(Rc<CoverParenthesizedExpressionAndArrowParameterList>),
    //    #[cfg(test)]
    //    DebuggerStatement(Rc<DebuggerStatement>),
    #[cfg(test)]
    Declaration(Rc<Declaration>),
    #[cfg(test)]
    DefaultClause(Rc<DefaultClause>),
    #[cfg(test)]
    DestructuringAssignmentTarget(Rc<DestructuringAssignmentTarget>),
    #[cfg(test)]
    DoWhileStatement(Rc<DoWhileStatement>),
    #[cfg(test)]
    ElementList(Rc<ElementList>),
    //    #[cfg(test)]
    //    Elisions(Rc<Elisions>),
    //    #[cfg(test)]
    //    EmptyStatement(Rc<EmptyStatement>),
    #[cfg(test)]
    EqualityExpression(Rc<EqualityExpression>),
    #[cfg(test)]
    ExponentiationExpression(Rc<ExponentiationExpression>),
    #[cfg(test)]
    Expression(Rc<Expression>),
    #[cfg(test)]
    ExpressionBody(Rc<ExpressionBody>),
    #[cfg(test)]
    ExpressionStatement(Rc<ExpressionStatement>),
    #[cfg(test)]
    FieldDefinition(Rc<FieldDefinition>),
    #[cfg(test)]
    Finally(Rc<Finally>),
    #[cfg(test)]
    ForBinding(Rc<ForBinding>),
    #[cfg(test)]
    ForDeclaration(Rc<ForDeclaration>),
    #[cfg(test)]
    ForInOfStatement(Rc<ForInOfStatement>),
    #[cfg(test)]
    FormalParameter(Rc<FormalParameter>),
    #[cfg(test)]
    FormalParameterList(Rc<FormalParameterList>),
    #[cfg(test)]
    ForStatement(Rc<ForStatement>),
    #[cfg(test)]
    FunctionDeclaration(Rc<FunctionDeclaration>),
    #[cfg(test)]
    FunctionRestParameter(Rc<FunctionRestParameter>),
    #[cfg(test)]
    FunctionStatementList(Rc<FunctionStatementList>),
    #[cfg(test)]
    GeneratorDeclaration(Rc<GeneratorDeclaration>),
    //    #[cfg(test)]
    //    GeneratorMethod(Rc<GeneratorMethod>),
    #[cfg(test)]
    IfStatement(Rc<IfStatement>),
    #[cfg(test)]
    Initializer(Rc<Initializer>),
    #[cfg(test)]
    IterationStatement(Rc<IterationStatement>),
    #[cfg(test)]
    LabelledItem(Rc<LabelledItem>),
    #[cfg(test)]
    LabelledStatement(Rc<LabelledStatement>),
    #[cfg(test)]
    LeftHandSideExpression(Rc<LeftHandSideExpression>),
    #[cfg(test)]
    LexicalBinding(Rc<LexicalBinding>),
    #[cfg(test)]
    LexicalDeclaration(Rc<LexicalDeclaration>),
    //    #[cfg(test)]
    //    Literal(Rc<Literal>),
    //    #[cfg(test)]
    //    LiteralPropertyName(Rc<LiteralPropertyName>),
    #[cfg(test)]
    LogicalANDExpression(Rc<LogicalANDExpression>),
    #[cfg(test)]
    LogicalORExpression(Rc<LogicalORExpression>),
    #[cfg(test)]
    MemberExpression(Rc<MemberExpression>),
    //    #[cfg(test)]
    //    MetaProperty(Rc<MetaProperty>),
    #[cfg(test)]
    MethodDefinition(Rc<MethodDefinition>),
    #[cfg(test)]
    MultiplicativeExpression(Rc<MultiplicativeExpression>),
    #[cfg(test)]
    NewExpression(Rc<NewExpression>),
    #[cfg(test)]
    ObjectAssignmentPattern(Rc<ObjectAssignmentPattern>),
    #[cfg(test)]
    ObjectBindingPattern(Rc<ObjectBindingPattern>),
    #[cfg(test)]
    ObjectLiteral(Rc<ObjectLiteral>),
    #[cfg(test)]
    OptionalChain(Rc<OptionalChain>),
    #[cfg(test)]
    OptionalExpression(Rc<OptionalExpression>),
    #[cfg(test)]
    ParenthesizedExpression(Rc<ParenthesizedExpression>),
    #[cfg(test)]
    PrimaryExpression(Rc<PrimaryExpression>),
    #[cfg(test)]
    PropertyDefinition(Rc<PropertyDefinition>),
    #[cfg(test)]
    PropertyDefinitionList(Rc<PropertyDefinitionList>),
    #[cfg(test)]
    PropertyName(Rc<PropertyName>),
    //    #[cfg(test)]
    //    PropertySetParameterList(Rc<PropertySetParameterList>),
    #[cfg(test)]
    RelationalExpression(Rc<RelationalExpression>),
    #[cfg(test)]
    ReturnStatement(Rc<ReturnStatement>),
    #[cfg(test)]
    ScriptBody(Rc<ScriptBody>),
    #[cfg(test)]
    ShiftExpression(Rc<ShiftExpression>),
    #[cfg(test)]
    ShortCircuitExpression(Rc<ShortCircuitExpression>),
    #[cfg(test)]
    SingleNameBinding(Rc<SingleNameBinding>),
    #[cfg(test)]
    SpreadElement(Rc<SpreadElement>),
    #[cfg(test)]
    Statement(Rc<Statement>),
    #[cfg(test)]
    StatementList(Rc<StatementList>),
    #[cfg(test)]
    StatementListItem(Rc<StatementListItem>),
    #[cfg(test)]
    SubstitutionTemplate(Rc<SubstitutionTemplate>),
    //    #[cfg(test)]
    //    SuperCall(Rc<SuperCall>),
    //    #[cfg(test)]
    //    SuperProperty(Rc<SuperProperty>),
    #[cfg(test)]
    SwitchStatement(Rc<SwitchStatement>),
    #[cfg(test)]
    TemplateLiteral(Rc<TemplateLiteral>),
    #[cfg(test)]
    TemplateMiddleList(Rc<TemplateMiddleList>),
    #[cfg(test)]
    TemplateSpans(Rc<TemplateSpans>),
    #[cfg(test)]
    ThrowStatement(Rc<ThrowStatement>),
    #[cfg(test)]
    TryStatement(Rc<TryStatement>),
    #[cfg(test)]
    UnaryExpression(Rc<UnaryExpression>),
    #[cfg(test)]
    UniqueFormalParameters(Rc<UniqueFormalParameters>),
    #[cfg(test)]
    UpdateExpression(Rc<UpdateExpression>),
    #[cfg(test)]
    VariableDeclaration(Rc<VariableDeclaration>),
    #[cfg(test)]
    VariableDeclarationList(Rc<VariableDeclarationList>),
    #[cfg(test)]
    VariableStatement(Rc<VariableStatement>),
    #[cfg(test)]
    WhileStatement(Rc<WhileStatement>),
    //    #[cfg(test)]
    //    WithStatement(Rc<WithStatement>),
    //    #[cfg(test)]
    //    YieldExpression(Rc<YieldExpression>),
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

pub(crate) enum ParsedBody {
    Function(Rc<FunctionBody>),
    Generator(Rc<GeneratorBody>),
    AsyncFunction(Rc<AsyncFunctionBody>),
    AsyncGenerator(Rc<AsyncGeneratorBody>),
}
impl TryFrom<ParsedText> for Result<ParsedBody, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::FunctionBody(node) => Ok(Ok(ParsedBody::Function(node))),
            ParsedText::GeneratorBody(node) => Ok(Ok(ParsedBody::Generator(node))),
            #[cfg(test)]
            ParsedText::AsyncFunctionBody(node) => Ok(Ok(ParsedBody::AsyncFunction(node))),
            #[cfg(test)]
            ParsedText::AsyncGeneratorBody(node) => Ok(Ok(ParsedBody::AsyncGenerator(node))),
            _ => Err(anyhow!("Expected Some kind of function body or Syntax Errors")),
        }
    }
}
impl TryFrom<BodySource> for ParsedBody {
    type Error = anyhow::Error;

    fn try_from(value: BodySource) -> Result<Self, Self::Error> {
        match value {
            BodySource::Function(function_body) => Ok(Self::Function(function_body)),
            BodySource::Generator(generator_body) => Ok(Self::Generator(generator_body)),
            BodySource::AsyncFunction(async_function_body) => Ok(Self::AsyncFunction(async_function_body)),
            BodySource::AsyncGenerator(async_generator_body) => Ok(Self::AsyncGenerator(async_generator_body)),
            _ => Err(anyhow!("Some kind of normal function body expected")),
        }
    }
}

pub(crate) enum ParsedFunctionExpression {
    Function(Rc<FunctionExpression>),
    Generator(Rc<GeneratorExpression>),
    #[cfg(test)]
    AsyncFunction(Rc<AsyncFunctionExpression>),
    #[cfg(test)]
    AsyncGenerator(Rc<AsyncGeneratorExpression>),
}
impl TryFrom<ParsedText> for Result<ParsedFunctionExpression, Vec<Object>> {
    type Error = anyhow::Error;

    fn try_from(value: ParsedText) -> Result<Self, Self::Error> {
        match value {
            ParsedText::Errors(errs) => Ok(Err(errs)),
            ParsedText::FunctionExpression(node) => Ok(Ok(ParsedFunctionExpression::Function(node))),
            ParsedText::GeneratorExpression(node) => Ok(Ok(ParsedFunctionExpression::Generator(node))),
            #[cfg(test)]
            ParsedText::AsyncFunctionExpression(node) => Ok(Ok(ParsedFunctionExpression::AsyncFunction(node))),
            #[cfg(test)]
            ParsedText::AsyncGeneratorExpression(node) => Ok(Ok(ParsedFunctionExpression::AsyncGenerator(node))),
            _ => Err(anyhow!("Expected Some kind of function expression or Syntax Errors")),
        }
    }
}

impl ParsedText {
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        match self {
            ParsedText::Errors(_) | ParsedText::Empty => None,
            ParsedText::Script(script) => script.body_containing_location(location),
            ParsedText::FormalParameters(formal_parameters) => formal_parameters.body_containing_location(location),
            ParsedText::FunctionBody(function_body) => function_body.body_containing_location(location),
            ParsedText::GeneratorBody(generator_body) => generator_body.body_containing_location(location),
            #[cfg(test)]
            ParsedText::AsyncFunctionBody(_) => {
                //async_function_body.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AsyncGeneratorBody(_) => {
                //async_generator_body.body_containing_location(location)
                None
            }
            ParsedText::FunctionExpression(function_expression) => {
                function_expression.body_containing_location(location)
            }
            ParsedText::GeneratorExpression(generator_expression) => {
                generator_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AsyncFunctionExpression(async_function_expression) => {
                async_function_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AsyncGeneratorExpression(async_generator_expression) => {
                async_generator_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::FunctionDeclaration(function_declaration) => {
                function_declaration.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AsyncFunctionDeclaration(async_function_declaration) => {
                async_function_declaration.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AsyncGeneratorDeclaration(async_generator_declaration) => {
                async_generator_declaration.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::GeneratorDeclaration(generator_declaration) => {
                generator_declaration.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::PrimaryExpression(primary_expression) => primary_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::FieldDefinition(field_definition) => field_definition.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ParenthesizedExpression(parenthesized_expression) => {
                parenthesized_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ObjectLiteral(object_literal) => object_literal.body_containing_location(location),
            #[cfg(test)]
            ParsedText::PropertyDefinitionList(property_definition_list) => {
                property_definition_list.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::PropertyDefinition(property_definition) => {
                property_definition.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ArrowFunction(arrow_function) => arrow_function.body_containing_location(location),
            #[cfg(test)]
            ParsedText::PropertyName(property_name) => property_name.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ComputedPropertyName(computed_property_name) => {
                computed_property_name.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::MemberExpression(member_expression) => member_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::NewExpression(new_expression) => new_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CallExpression(call_expression) => call_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CallMemberExpression(call_member_expression) => {
                call_member_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::LeftHandSideExpression(left_hand_side_expression) => {
                left_hand_side_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::Arguments(arguments) => arguments.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ArgumentList(argument_list) => argument_list.body_containing_location(location),
            #[cfg(test)]
            ParsedText::AdditiveExpression(additive_expression) => {
                additive_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ArrayAssignmentPattern(_) => {
                //array_assignment_pattern.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ArrayBindingPattern(_) => {
                //array_binding_pattern.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ArrayLiteral(array_literal) => array_literal.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ArrowFormalParameters(_) => {
                //arrow_formal_parameters.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ArrowParameters(_) => {
                //arrow_parameters.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AssignmentElement(_) => {
                //assignment_element.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AssignmentElementList(_) => {
                None
                //assignment_element_list.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AssignmentElisionElement(_) => {
                None
                //assignment_elision_element.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AssignmentExpression(assignment_expression) => {
                assignment_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::AssignmentPattern(assignment_pattern) => assignment_pattern.body_containing_location(location),
            #[cfg(test)]
            ParsedText::AssignmentProperty(_) => {
                //assignment_property.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AssignmentPropertyList(_) => {
                //assignment_property_list.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AssignmentRestElement(_) => {
                //assignment_rest_element.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AssignmentRestProperty(_) => {
                //assignment_rest_property.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::AsyncArrowBindingIdentifier(_) => {
                //async_arrow_binding_identifier.body_containing_location(location)
                None
            }
            //#[cfg(test)]
            //ParsedText::AsyncArrowFunction(async_arrow_function) => {
            //    async_arrow_function.body_containing_location(location)
            //}
            //#[cfg(test)]
            //ParsedText::AsyncArrowHead(async_arrow_head) => {
            //    //async_arrow_head.body_containing_location(location)
            //    None
            //}
            //#[cfg(test)]
            //ParsedText::AsyncConciseBody(async_concise_body) => {
            //    //async_concise_body.body_containing_location(location)
            //    None
            //}
            //#[cfg(test)]
            //ParsedText::AsyncGeneratorMethod(async_generator_method) => {
            //    async_generator_method.body_containing_location(location)
            //}
            //#[cfg(test)]
            //ParsedText::AsyncMethod(async_method) => async_method.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::AwaitExpression(await_expression) => await_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BindingElement(binding_element) => binding_element.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BindingElementList(_) => {
                //binding_element_list.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::BindingElisionElement(_) => {
                //binding_elision_element.body_containing_location(location)
                None
            }
            //#[cfg(test)]
            //ParsedText::BindingIdentifier(binding_identifier) => binding_identifier.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BindingList(binding_list) => binding_list.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BindingPattern(binding_pattern) => binding_pattern.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BindingProperty(_) => {
                //binding_property.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::BindingPropertyList(_) => {
                //binding_property_list.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::BindingRestElement(_) => {
                //binding_rest_element.body_containing_location(location)
                None
            }
            //#[cfg(test)]
            //ParsedText::BindingRestProperty(binding_rest_property) => {
            //    //binding_rest_property.body_containing_location(location)
            //    None
            //}
            #[cfg(test)]
            ParsedText::BitwiseANDExpression(bitwise_andexpression) => {
                bitwise_andexpression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::BitwiseORExpression(bitwise_orexpression) => {
                bitwise_orexpression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::BitwiseXORExpression(bitwise_xorexpression) => {
                bitwise_xorexpression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::Block(block) => block.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BlockStatement(block_statement) => block_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::BreakableStatement(breakable_statement) => {
                breakable_statement.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::BreakStatement(break_statement) => break_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CaseBlock(case_block) => case_block.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CaseClause(case_clause) => case_clause.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::CaseClauses(case_clauses) => case_clauses.body_containing_location(location),
            #[cfg(test)]
            ParsedText::Catch(catch) => catch.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CatchParameter(catch_parameter) => catch_parameter.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::ClassBody(class_body) => class_body.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ClassDeclaration(class_declaration) => class_declaration.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::ClassElement(class_element) => class_element.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::ClassElementList(class_element_list) => class_element_list.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ClassElementName(class_element_name) => class_element_name.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::ClassExpression(class_expression) => class_expression.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::ClassHeritage(class_heritage) => {
            //    //class_heritage.body_containing_location(location)
            //    None
            //}
            //#[cfg(test)]
            //ParsedText::ClassStaticBlock(class_static_block) => class_static_block.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ClassStaticBlockBody(_) => {
                //class_static_block_body.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ClassStaticBlockStatementList(_) => {
                //class_static_block_statement_list.body_containing_location(location)
                None
            }
            //#[cfg(test)]
            //ParsedText::ClassTail(class_tail) => class_tail.body_containing_location(location),
            #[cfg(test)]
            ParsedText::CoalesceExpression(coalesce_expression) => {
                coalesce_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::CoalesceExpressionHead(coalesce_expression_head) => {
                coalesce_expression_head.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::ConciseBody(concise_body) => {
            //    //concise_body.body_containing_location(location)
            //    None
            //}
            #[cfg(test)]
            ParsedText::ConditionalExpression(conditional_expression) => {
                conditional_expression.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::ContinueStatement(continue_statement) => continue_statement.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::CoverInitializedName(cover_initialized_name) => {
            //    cover_initialized_name.body_containing_location(location)
            //}
            //#[cfg(test)]
            //ParsedText::CoverParenthesizedExpressionAndArrowParameterList(
            //    cover_parenthesized_expression_and_arrow_parameter_list,
            //) => cover_parenthesized_expression_and_arrow_parameter_list.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::DebuggerStatement(debugger_statement) => debugger_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::Declaration(declaration) => declaration.body_containing_location(location),
            #[cfg(test)]
            ParsedText::DefaultClause(default_clause) => default_clause.body_containing_location(location),
            #[cfg(test)]
            ParsedText::DestructuringAssignmentTarget(_) => {
                //destructuring_assignment_target.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::DoWhileStatement(do_while_statement) => do_while_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ElementList(element_list) => element_list.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::Elisions(elisions) => elisions.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::EmptyStatement(empty_statement) => empty_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::EqualityExpression(equality_expression) => {
                equality_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ExponentiationExpression(exponentiation_expression) => {
                exponentiation_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::Expression(expression) => expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ExpressionBody(_) => {
                //expression_body.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ExpressionStatement(expression_statement) => {
                expression_statement.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::Finally(finally) => finally.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ForBinding(_) => {
                //for_binding.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ForDeclaration(_) => {
                //for_declaration.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ForInOfStatement(for_in_of_statement) => for_in_of_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::FormalParameter(formal_parameter) => formal_parameter.body_containing_location(location),
            #[cfg(test)]
            ParsedText::FormalParameterList(formal_parameter_list) => {
                formal_parameter_list.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ForStatement(for_statement) => for_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::FunctionRestParameter(function_rest_parameter) => {
                function_rest_parameter.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::FunctionStatementList(function_statement_list) => {
                function_statement_list.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::GeneratorMethod(generator_method) => generator_method.body_containing_location(location),
            #[cfg(test)]
            ParsedText::IfStatement(if_statement) => if_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::Initializer(initializer) => initializer.body_containing_location(location),
            #[cfg(test)]
            ParsedText::IterationStatement(iteration_statement) => {
                iteration_statement.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::LabelledItem(labelled_item) => labelled_item.body_containing_location(location),
            #[cfg(test)]
            ParsedText::LabelledStatement(labelled_statement) => labelled_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::LexicalBinding(lexical_binding) => lexical_binding.body_containing_location(location),
            #[cfg(test)]
            ParsedText::LexicalDeclaration(lexical_declaration) => {
                lexical_declaration.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::Literal(literal) => literal.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::LiteralPropertyName(literal_property_name) => {
            //    literal_property_name.body_containing_location(location)
            //}
            #[cfg(test)]
            ParsedText::LogicalANDExpression(logical_andexpression) => {
                logical_andexpression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::LogicalORExpression(logical_orexpression) => {
                logical_orexpression.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::MetaProperty(meta_property) => meta_property.body_containing_location(location),
            #[cfg(test)]
            ParsedText::MethodDefinition(method_definition) => method_definition.body_containing_location(location),
            #[cfg(test)]
            ParsedText::MultiplicativeExpression(multiplicative_expression) => {
                multiplicative_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ObjectAssignmentPattern(_) => {
                //object_assignment_pattern.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::ObjectBindingPattern(_) => {
                //object_binding_pattern.body_containing_location(location)
                None
            }
            #[cfg(test)]
            ParsedText::OptionalChain(optional_chain) => optional_chain.body_containing_location(location),
            #[cfg(test)]
            ParsedText::OptionalExpression(optional_expression) => {
                optional_expression.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::PropertySetParameterList(property_set_parameter_list) => {
            //    property_set_parameter_list.body_containing_location(location)
            //}
            #[cfg(test)]
            ParsedText::RelationalExpression(relational_expression) => {
                relational_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::ReturnStatement(return_statement) => return_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ScriptBody(script_body) => script_body.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ShiftExpression(shift_expression) => shift_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ShortCircuitExpression(short_circuit_expression) => {
                short_circuit_expression.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::SingleNameBinding(single_name_binding) => {
                single_name_binding.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::SpreadElement(spread_element) => spread_element.body_containing_location(location),
            #[cfg(test)]
            ParsedText::Statement(statement) => statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::StatementList(statement_list) => statement_list.body_containing_location(location),
            #[cfg(test)]
            ParsedText::StatementListItem(statement_list_item) => {
                statement_list_item.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::SubstitutionTemplate(substitution_template) => {
                substitution_template.body_containing_location(location)
            }
            //#[cfg(test)]
            //ParsedText::SuperCall(super_call) => super_call.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::SuperProperty(super_property) => super_property.body_containing_location(location),
            #[cfg(test)]
            ParsedText::SwitchStatement(switch_statement) => switch_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::TemplateLiteral(template_literal) => template_literal.body_containing_location(location),
            #[cfg(test)]
            ParsedText::TemplateMiddleList(template_middle_list) => {
                template_middle_list.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::TemplateSpans(template_spans) => template_spans.body_containing_location(location),
            #[cfg(test)]
            ParsedText::ThrowStatement(throw_statement) => throw_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::TryStatement(try_statement) => try_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::UnaryExpression(unary_expression) => unary_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::UniqueFormalParameters(unique_formal_parameters) => {
                unique_formal_parameters.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::UpdateExpression(update_expression) => update_expression.body_containing_location(location),
            #[cfg(test)]
            ParsedText::VariableDeclaration(variable_declaration) => {
                variable_declaration.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::VariableDeclarationList(variable_declaration_list) => {
                variable_declaration_list.body_containing_location(location)
            }
            #[cfg(test)]
            ParsedText::VariableStatement(variable_statement) => variable_statement.body_containing_location(location),
            #[cfg(test)]
            ParsedText::WhileStatement(while_statement) => while_statement.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::WithStatement(with_statement) => with_statement.body_containing_location(location),
            //#[cfg(test)]
            //ParsedText::YieldExpression(yield_expression) => yield_expression.body_containing_location(location),
        }
    }
}

enum ParsedItem {
    Script(Rc<Script>),
    FormalParameters(Rc<FormalParameters>),
    FunctionBody(Rc<FunctionBody>),
    GeneratorBody(Rc<GeneratorBody>),
    FunctionExpression(Rc<FunctionExpression>),
    GeneratorExpression(Rc<GeneratorExpression>),

    // Not currently constructed; relegated to test.
    #[cfg(test)]
    AsyncFunctionBody(Rc<AsyncFunctionBody>),
    #[cfg(test)]
    AsyncGeneratorBody(Rc<AsyncGeneratorBody>),
    #[cfg(test)]
    AsyncFunctionExpression(Rc<AsyncFunctionExpression>),
    #[cfg(test)]
    AsyncGeneratorExpression(Rc<AsyncGeneratorExpression>),
}

impl ParsedItem {
    fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ParsedItem::Script(s) => s.early_errors(errs, strict),
            ParsedItem::FormalParameters(fp) => fp.early_errors(errs, strict, false),
            ParsedItem::FunctionBody(fb) => fb.early_errors(errs, strict),
            ParsedItem::GeneratorBody(gb) => gb.early_errors(errs, strict),
            ParsedItem::FunctionExpression(exp) => exp.early_errors(errs, strict),
            ParsedItem::GeneratorExpression(exp) => exp.early_errors(errs, strict),
            #[cfg(test)]
            ParsedItem::AsyncFunctionBody(body) => body.early_errors(errs, strict),
            #[cfg(test)]
            ParsedItem::AsyncGeneratorBody(body) => body.early_errors(errs, strict),
            #[cfg(test)]
            ParsedItem::AsyncFunctionExpression(exp) => exp.early_errors(errs, strict),
            #[cfg(test)]
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
            #[cfg(test)]
            ParsedItem::AsyncFunctionBody(body) => ParsedText::AsyncFunctionBody(body),
            #[cfg(test)]
            ParsedItem::AsyncGeneratorBody(body) => ParsedText::AsyncGeneratorBody(body),
            ParsedItem::FunctionExpression(exp) => ParsedText::FunctionExpression(exp),
            ParsedItem::GeneratorExpression(exp) => ParsedText::GeneratorExpression(exp),
            #[cfg(test)]
            ParsedItem::AsyncFunctionExpression(exp) => ParsedText::AsyncFunctionExpression(exp),
            #[cfg(test)]
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

pub(crate) fn parse_text(src: &str, goal_symbol: ParseGoal, strict: bool, direct: bool) -> ParsedText {
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
                    FunctionBodyParent::Function,
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
        #[cfg(test)]
        ParseGoal::AsyncFunctionBody => (
            Box::new(|parser: &mut Parser| {
                let (body, scanner) = AsyncFunctionBody::parse(parser, Scanner::new());
                Ok((ParsedItem::AsyncFunctionBody(body), scanner))
            }),
            "function body",
        ),
        #[cfg(test)]
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
        #[cfg(test)]
        ParseGoal::AsyncFunctionExpression => (
            Box::new(|parser: &mut Parser| {
                AsyncFunctionExpression::parse(parser, Scanner::new())
                    .map(|(item, scanner)| (ParsedItem::AsyncFunctionExpression(item), scanner))
            }),
            "function expression",
        ),
        #[cfg(test)]
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

pub(crate) fn duplicates(idents: &[JSString]) -> Vec<&JSString> {
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

#[derive(Debug, Clone)]
pub(crate) enum ContainingBody {
    FunctionBody(Rc<FunctionBody>),
    ConciseBody(Rc<ConciseBody>),
    //AsyncConciseBody(Rc<AsyncConciseBody>), // Add back when Async goes in
}

pub(crate) mod additive_operators;
pub(crate) mod arrow_function_definitions;
pub(crate) mod assignment_operators;
pub(crate) mod async_arrow_function_definitions;
pub(crate) mod async_function_definitions;
pub(crate) mod async_generator_function_definitions;
pub(crate) mod binary_bitwise_operators;
pub(crate) mod binary_logical_operators;
pub(crate) mod bitwise_shift_operators;
pub(crate) mod block;
pub(crate) mod break_statement;
pub(crate) mod class_definitions;
pub(crate) mod comma_operator;
pub(crate) mod conditional_operator;
pub(crate) mod continue_statement;
pub(crate) mod debugger_statement;
pub(crate) mod declarations_and_variables;
pub(crate) mod empty_statement;
pub(crate) mod equality_operators;
pub(crate) mod exponentiation_operator;
pub(crate) mod expression_statement;
pub(crate) mod function_definitions;
pub(crate) mod generator_function_definitions;
pub(crate) mod identifiers;
pub(crate) mod if_statement;
pub(crate) mod iteration_statements;
pub(crate) mod labelled_statements;
pub(crate) mod left_hand_side_expressions;
pub(crate) mod method_definitions;
pub(crate) mod multiplicative_operators;
pub(crate) mod parameter_lists;
pub(crate) mod primary_expressions;
pub(crate) mod relational_operators;
pub(crate) mod return_statement;
pub(crate) mod scripts;
pub(crate) mod statements_and_declarations;
pub(crate) mod switch_statement;
pub(crate) mod throw_statement;
pub(crate) mod try_statement;
pub(crate) mod unary_operators;
pub(crate) mod update_expressions;
pub(crate) mod with_statement;

pub(crate) use additive_operators::*;
pub(crate) use arrow_function_definitions::*;
pub(crate) use assignment_operators::*;
pub(crate) use async_arrow_function_definitions::*;
pub(crate) use async_function_definitions::*;
pub(crate) use async_generator_function_definitions::*;
pub(crate) use binary_bitwise_operators::*;
pub(crate) use binary_logical_operators::*;
pub(crate) use bitwise_shift_operators::*;
pub(crate) use block::*;
pub(crate) use break_statement::*;
pub(crate) use class_definitions::*;
pub(crate) use comma_operator::*;
pub(crate) use conditional_operator::*;
pub(crate) use continue_statement::*;
pub(crate) use debugger_statement::*;
pub(crate) use declarations_and_variables::*;
pub(crate) use empty_statement::*;
pub(crate) use equality_operators::*;
pub(crate) use exponentiation_operator::*;
pub(crate) use expression_statement::*;
pub(crate) use function_definitions::*;
pub(crate) use generator_function_definitions::*;
pub(crate) use identifiers::*;
pub(crate) use if_statement::*;
pub(crate) use iteration_statements::*;
pub(crate) use labelled_statements::*;
pub(crate) use left_hand_side_expressions::*;
pub(crate) use method_definitions::*;
pub(crate) use multiplicative_operators::*;
pub(crate) use parameter_lists::*;
pub(crate) use primary_expressions::*;
pub(crate) use relational_operators::*;
pub(crate) use return_statement::*;
pub(crate) use scripts::*;
pub(crate) use statements_and_declarations::*;
pub(crate) use switch_statement::*;
pub(crate) use throw_statement::*;
pub(crate) use try_statement::*;
pub(crate) use unary_operators::*;
pub(crate) use update_expressions::*;
pub(crate) use with_statement::*;

#[cfg(test)]
pub(crate) mod testhelp;

#[cfg(test)]
mod tests;

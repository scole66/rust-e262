use super::*;
use ahash::AHashSet;
use std::fmt;

pub fn check<T>(res: ParseResult<T>) -> (Rc<T>, Scanner) {
    assert!(res.is_ok());
    res.unwrap()
}
pub fn check_err<T>(res: ParseResult<T>, msg: &str, line: u32, column: u32)
where
    T: fmt::Debug,
{
    let err = res.unwrap_err();
    assert_eq!(err.location.starting_line, line);
    assert_eq!(err.location.starting_column, column);
    assert_eq!(format!("{}", err), String::from(msg));
}
pub fn expected_scan(count: u32) -> Scanner {
    // Expected Scanner for tests. (The real world will be more varied.)
    Scanner { line: 1, column: count + 1, start_idx: count as usize }
}
pub fn sv(strings: &[&str]) -> Vec<String> {
    strings.iter().map(|s| String::from(*s)).collect()
}
pub fn chk_scan(scanner: &Scanner, count: u32) {
    assert_eq!(*scanner, expected_scan(count));
}
pub fn newparser(text: &str) -> Parser {
    Parser::new(text, false, ParseGoal::Script)
}
pub fn check_parse_error<T, U>(result: ParseResult<T>, msg: U, token_len: usize)
where
    T: fmt::Debug,
    U: Into<String>,
{
    let pe = result.unwrap_err();
    assert_eq!(
        pe.location,
        Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: token_len } }
    );
    assert_eq!(format!("{}", pe), msg.into());
}
pub fn sset(items: &[&str]) -> AHashSet<String> {
    AHashSet::from_iter(items.iter().map(|&x| String::from(x)))
}

pub fn svec(items: &[&str]) -> Vec<String> {
    items.iter().map(|&s| String::from(s)).collect::<Vec<_>>()
}

/// Parse Node builder for test cases
///
/// This struct holds options used when parsing text to make Parse Nodes, towered together in a builder pattern. Not all
/// options are used for all Parse Nodes, and the defaults are all opinions, but the hope is that this makes test cases
/// easier to write.
///
/// Example: If we're trying to create a [`ForBinding`], note that the offical production looks like:
/// ```plain
/// ForBinding[Yield, Await] :
///      BindingIdentifier[?Yield, ?Await]
///      BindingPattern[?Yield, ?Await]
/// ```
///
/// Note that is has `Yield` and `Await` parameters, but no others. If the source text for a `ForBinding` is in the
/// string `source`, then you could use this code like:
/// ```rust
///   let source = "{ name = banjo }";
///   let fb = Maker::new(source).yield_ok(true).await_ok(false).for_binding();
///   assert!(matches!(*fb, ForBinding::BindingPattern(_)));
/// ```
///
/// Default values are used for all the flags; if you don't need to change them from the default (which is typical), you
/// don't need to call the associated builder function.
///
/// Parsing Flag | Default Value
/// -------------|--------------
/// Await        | `true`
/// Yield        | `true`
/// Return       | `true`
/// Tagged       | `false`
/// In           | `true`
/// Default      | `true`
pub struct Maker<'a> {
    source: &'a str,
    yield_flag: bool,
    await_flag: bool,
    return_flag: bool,
    tagged_flag: bool,
    in_flag: bool,
    default_flag: bool,
}
impl<'a> Default for Maker<'a> {
    fn default() -> Self {
        Maker {
            source: "",
            yield_flag: true,
            await_flag: true,
            return_flag: true,
            tagged_flag: false,
            in_flag: true,
            default_flag: true,
        }
    }
}

impl<'a> Maker<'a> {
    /// Construct a new Maker object with the given source.
    pub fn new(src: &'a str) -> Self {
        Maker { source: src, ..Default::default() }
    }
    /// Set the `yield_flag` in the maker object.
    ///
    /// `true` means that yield expressions are allowed; `false` means that the `yield` keyword can be used as an
    /// identifier.
    pub fn yield_ok(self, yield_flag: bool) -> Self {
        Self { yield_flag, ..self }
    }
    /// Set the `await_flag` in the maker object.
    ///
    /// `true` means that await expressions are allowed; `false` means that the `await` keyword can be used as an
    /// identifier.
    pub fn await_ok(self, await_flag: bool) -> Self {
        Self { await_flag, ..self }
    }
    /// Set the `in_flag` in the maker object.
    ///
    /// `true` means that the `in` keyword may be used in a [`RelationalExpression`]; `false` means that it may not.
    /// (This is the case in some `for` statements.)
    pub fn in_ok(self, in_flag: bool) -> Self {
        Self { in_flag, ..self }
    }
    /// Set the `return_flag` in the maker object.
    ///
    /// `true` means that `return` statements are currently allowed (generally within function bodies); `false` means
    /// they are not.
    pub fn return_ok(self, return_flag: bool) -> Self {
        Self { return_flag, ..self }
    }
    /// Set the `tagged` flag in the maker object.
    ///
    /// `true` means that a template literal is being treated as a [tagged template][1]; `false` means that is is not.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-tagged-templates
    pub fn tagged_ok(self, tagged_flag: bool) -> Self {
        Self { tagged_flag, ..self }
    }
    /// Set the `default_flag` in the maker object.
    ///
    /// `true` means that nameless functions are allowed.
    pub fn default_ok(self, default_flag: bool) -> Self {
        Self { default_flag, ..self }
    }
    /// Use the configs in the [`Maker`] object to make a [`AdditiveExpression`] parse node.
    pub fn additive_expression(self) -> Rc<AdditiveExpression> {
        AdditiveExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArgumentList`] parse node.
    pub fn argument_list(self) -> Rc<ArgumentList> {
        ArgumentList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Arguments`] parse node.
    pub fn arguments(self) -> Rc<Arguments> {
        Arguments::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrowFormalParameters`] parse node.
    pub fn arrow_formal_parameters(self) -> Rc<ArrowFormalParameters> {
        ArrowFormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayAssignmentPattern`] parse node.
    pub fn array_assignment_pattern(self) -> Rc<ArrayAssignmentPattern> {
        ArrayAssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayBindingPattern`] parse node.
    pub fn array_binding_pattern(self) -> Rc<ArrayBindingPattern> {
        ArrayBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayLiteral`] parse node.
    pub fn array_literal(self) -> Rc<ArrayLiteral> {
        ArrayLiteral::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrowFunction`] parse node.
    pub fn arrow_function(self) -> Rc<ArrowFunction> {
        ArrowFunction::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrowParameters`] parse node.
    pub fn arrow_parameters(self) -> Rc<ArrowParameters> {
        ArrowParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentExpression`] parse node.
    pub fn assignment_expression(self) -> Rc<AssignmentExpression> {
        AssignmentExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentPattern`] parse node.
    pub fn assignment_pattern(self) -> Rc<AssignmentPattern> {
        AssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncArrowBindingIdentifier`] parse node.
    pub fn async_arrow_binding_identifier(self) -> Rc<AsyncArrowBindingIdentifier> {
        AsyncArrowBindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncArrowFunction`] parse node.
    pub fn async_arrow_function(self) -> Rc<AsyncArrowFunction> {
        AsyncArrowFunction::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncArrowHead`] parse node.
    pub fn async_arrow_head(self) -> Rc<AsyncArrowHead> {
        AsyncArrowHead::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncConciseBody`] parse node.
    pub fn async_concise_body(self) -> Rc<AsyncConciseBody> {
        AsyncConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncFunctionBody`] parse node.
    pub fn async_function_body(self) -> Rc<AsyncFunctionBody> {
        AsyncFunctionBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncFunctionDeclaration`] parse node.
    pub fn async_function_declaration(self) -> Rc<AsyncFunctionDeclaration> {
        AsyncFunctionDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncFunctionExpression`] parse node.
    pub fn async_function_expression(self) -> Rc<AsyncFunctionExpression> {
        AsyncFunctionExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorBody`] parse node.
    pub fn async_generator_body(self) -> Rc<AsyncGeneratorBody> {
        AsyncGeneratorBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorDeclaration`] parse node.
    pub fn async_generator_declaration(self) -> Rc<AsyncGeneratorDeclaration> {
        AsyncGeneratorDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorExpression`] parse node.
    pub fn async_generator_expression(self) -> Rc<AsyncGeneratorExpression> {
        AsyncGeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorMethod`] parse node.
    pub fn async_generator_method(self) -> Rc<AsyncGeneratorMethod> {
        AsyncGeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncMethod`] parse node.
    pub fn async_method(self) -> Rc<AsyncMethod> {
        AsyncMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`AwaitExpression`] parse node.
    pub fn await_expression(self) -> Rc<AwaitExpression> {
        AwaitExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElement`] parse node.
    pub fn binding_element(self) -> Rc<BindingElement> {
        BindingElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElementList`] parse node.
    pub fn binding_element_list(self) -> Rc<BindingElementList> {
        BindingElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElisionElement`] parse node.
    pub fn binding_elision_element(self) -> Rc<BindingElisionElement> {
        BindingElisionElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingIdentifier`] parse node.
    pub fn binding_identifier(self) -> Rc<BindingIdentifier> {
        BindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingList`] parse node.
    pub fn binding_list(self) -> Rc<BindingList> {
        BindingList::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingPattern`] parse node.
    pub fn binding_pattern(self) -> Rc<BindingPattern> {
        BindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingProperty`] parse node.
    pub fn binding_property(self) -> Rc<BindingProperty> {
        BindingProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingPropertyList`] parse node.
    pub fn binding_property_list(self) -> Rc<BindingPropertyList> {
        BindingPropertyList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingRestElement`] parse node.
    pub fn binding_rest_element(self) -> Rc<BindingRestElement> {
        BindingRestElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BitwiseANDExpression`] parse node.
    pub fn bitwise_and_expression(self) -> Rc<BitwiseANDExpression> {
        BitwiseANDExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BitwiseXORExpression`] parse node.
    pub fn bitwise_xor_expression(self) -> Rc<BitwiseXORExpression> {
        BitwiseXORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BitwiseORExpression`] parse node.
    pub fn bitwise_or_expression(self) -> Rc<BitwiseORExpression> {
        BitwiseORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Block`] parse node.
    pub fn block(self) -> Rc<Block> {
        Block::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BlockStatement`] parse node.
    pub fn block_statement(self) -> Rc<BlockStatement> {
        BlockStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`BreakStatement`] parse node.
    pub fn break_statement(self) -> Rc<BreakStatement> {
        BreakStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`BreakableStatement`] parse node.
    pub fn breakable_statement(self) -> Rc<BreakableStatement> {
        BreakableStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CallExpression`] parse node.
    pub fn call_expression(self) -> Rc<CallExpression> {
        CallExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`CallMemberExpression`] parse node.
    pub fn call_member_expression(self) -> Rc<CallMemberExpression> {
        CallMemberExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CaseBlock`] parse node.
    pub fn case_block(self) -> Rc<CaseBlock> {
        CaseBlock::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CaseClause`] parse node.
    pub fn case_clause(self) -> Rc<CaseClause> {
        CaseClause::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CaseClauses`] parse node.
    pub fn case_clauses(self) -> Rc<CaseClauses> {
        CaseClauses::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Catch`] parse node.
    pub fn catch(self) -> Rc<Catch> {
        Catch::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CatchParameter`] parse node.
    pub fn catch_parameter(self) -> Rc<CatchParameter> {
        CatchParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassBody`] parse node.
    pub fn class_body(self) -> Rc<ClassBody> {
        ClassBody::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassDeclaration`] parse node.
    pub fn class_declaration(self) -> Rc<ClassDeclaration> {
        ClassDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElement`] parse node.
    pub fn class_element(self) -> Rc<ClassElement> {
        ClassElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElementList`] parse node.
    pub fn class_element_list(self) -> Rc<ClassElementList> {
        ClassElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElementName`] parse node.
    pub fn class_element_name(self) -> Rc<ClassElementName> {
        ClassElementName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassExpression`] parse node.
    pub fn class_expression(self) -> Rc<ClassExpression> {
        ClassExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassHeritage`] parse node.
    pub fn class_heritage(self) -> Rc<ClassHeritage> {
        ClassHeritage::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlock`] parse node.
    pub fn class_static_block(self) -> Rc<ClassStaticBlock> {
        ClassStaticBlock::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlockBody`] parse node.
    pub fn class_static_block_body(self) -> Rc<ClassStaticBlockBody> {
        ClassStaticBlockBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlockStatementList`] parse node.
    pub fn class_static_block_statement_list(self) -> Rc<ClassStaticBlockStatementList> {
        ClassStaticBlockStatementList::parse(&mut newparser(self.source), Scanner::new()).0
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassTail`] parse node.
    pub fn class_tail(self) -> Rc<ClassTail> {
        ClassTail::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`CoalesceExpression`] parse node.
    pub fn coalesce_expression(self) -> Rc<CoalesceExpression> {
        CoalesceExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CoalesceExpressionHead`] parse node.
    ///
    /// Note that unlike the other `Maker` creation helpers, the source string provided for a `CoalesceExpressionHead`
    /// creator must actually be the source for a complete `CoalesceExpression`. The parsing for the two structures is
    /// tightly joined.
    pub fn coalesce_expression_head(self) -> Rc<CoalesceExpressionHead> {
        CoalesceExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
        .head
        .clone()
    }
    /// Use the configs in the [`Maker`] object to make a [`ComputedPropertyName`] parse node.
    pub fn computed_property_name(self) -> Rc<ComputedPropertyName> {
        ComputedPropertyName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ConciseBody`] parse node.
    pub fn concise_body(self) -> Rc<ConciseBody> {
        ConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ConditionalExpression`] parse node.
    pub fn conditional_expression(self) -> Rc<ConditionalExpression> {
        ConditionalExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ContinueStatement`] parse node.
    pub fn continue_statement(self) -> Rc<ContinueStatement> {
        ContinueStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CoverInitializedName`] parse node.
    pub fn cover_initialized_name(self) -> Rc<CoverInitializedName> {
        CoverInitializedName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`CoverParenthesizedExpressionAndArrowParameterList`] parse node.
    pub fn cover_parenthesized_expression_and_arrow_parameter_list(
        self,
    ) -> Rc<CoverParenthesizedExpressionAndArrowParameterList> {
        CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`DebuggerStatement`] parse node.
    pub fn debugger_statement(self) -> Rc<DebuggerStatement> {
        DebuggerStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Declaration`] parse node.
    pub fn declaration(self) -> Rc<Declaration> {
        Declaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`DefaultClause`] parse node.
    pub fn default_clause(self) -> Rc<DefaultClause> {
        DefaultClause::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`DoWhileStatement`] parse node.
    pub fn do_while_statement(self) -> Rc<DoWhileStatement> {
        DoWhileStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Elisions`] parse node.
    pub fn elision(self) -> Rc<Elisions> {
        Elisions::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ElementList`] parse node.
    pub fn element_list(self) -> Rc<ElementList> {
        ElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`EmptyStatement`] parse node.
    pub fn empty_statement(self) -> Rc<EmptyStatement> {
        EmptyStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`EqualityExpression`] parse node.
    pub fn equality_expression(self) -> Rc<EqualityExpression> {
        EqualityExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Expression`] parse node.
    pub fn expression(self) -> Rc<Expression> {
        Expression::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ExpressionBody`] parse node.
    pub fn expression_body(self) -> Rc<ExpressionBody> {
        ExpressionBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ExpressionStatement`] parse node.
    pub fn expression_statement(self) -> Rc<ExpressionStatement> {
        ExpressionStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ExponentiationExpression`] parse node.
    pub fn exponentiation_expression(self) -> Rc<ExponentiationExpression> {
        ExponentiationExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`FieldDefinition`] parse node.
    pub fn field_definition(self) -> Rc<FieldDefinition> {
        FieldDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Finally`] parse node.
    pub fn finally(self) -> Rc<Finally> {
        Finally::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ForBinding`] parse node.
    pub fn for_binding(self) -> Rc<ForBinding> {
        ForBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ForDeclaration`] parse node.
    pub fn for_declaration(self) -> Rc<ForDeclaration> {
        ForDeclaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameter`] parse node.
    pub fn formal_parameter(self) -> Rc<FormalParameter> {
        FormalParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameterList`] parse node.
    pub fn formal_parameter_list(self) -> Rc<FormalParameterList> {
        FormalParameterList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameters`] parse node.
    pub fn formal_parameters(self) -> Rc<FormalParameters> {
        FormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    /// Use the configs in the [`Maker`] object to make a [`ForInOfStatement`] parse node.
    pub fn for_in_of_statement(self) -> Rc<ForInOfStatement> {
        ForInOfStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ForStatement`] parse node.
    pub fn for_statement(self) -> Rc<ForStatement> {
        ForStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionBody`] parse node.
    pub fn function_body(self) -> Rc<FunctionBody> {
        FunctionBody::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionDeclaration`] parse node.
    pub fn function_declaration(self) -> Rc<FunctionDeclaration> {
        FunctionDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionExpression`] parse node.
    pub fn function_expression(self) -> Rc<FunctionExpression> {
        FunctionExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionRestParameter`] parse node.
    pub fn function_rest_parameter(self) -> Rc<FunctionRestParameter> {
        FunctionRestParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionStatementList`] parse node.
    pub fn function_statement_list(self) -> Rc<FunctionStatementList> {
        FunctionStatementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorBody`] parse node.
    pub fn generator_body(self) -> Rc<GeneratorBody> {
        GeneratorBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorDeclaration`] parse node.
    pub fn generator_declaration(self) -> Rc<GeneratorDeclaration> {
        GeneratorDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorExpression`] parse node.
    pub fn generator_expression(self) -> Rc<GeneratorExpression> {
        GeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorMethod`] parse node.
    pub fn generator_method(self) -> Rc<GeneratorMethod> {
        GeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`HoistableDeclaration`] parse node.
    pub fn hoistable_declaration(self) -> Rc<HoistableDeclaration> {
        HoistableDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Identifier`] parse node.
    pub fn identifier(self) -> Rc<Identifier> {
        Identifier::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`IdentifierReference`] parse node.
    pub fn identifier_reference(self) -> Rc<IdentifierReference> {
        IdentifierReference::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`IfStatement`] parse node.
    pub fn if_statement(self) -> Rc<IfStatement> {
        IfStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ImportCall`] parse node.
    pub fn import_call(self) -> Rc<ImportCall> {
        ImportCall::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Initializer`] parse node.
    pub fn initializer(self) -> Rc<Initializer> {
        Initializer::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`IterationStatement`] parse node.
    pub fn iteration_statement(self) -> Rc<IterationStatement> {
        IterationStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LabelIdentifier`] parse node.
    pub fn label_identifier(self) -> Rc<LabelIdentifier> {
        LabelIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`LabelledItem`] parse node.
    pub fn labelled_item(self) -> Rc<LabelledItem> {
        LabelledItem::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LabelledStatement`] parse node.
    pub fn labelled_statement(self) -> Rc<LabelledStatement> {
        LabelledStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LeftHandSideExpression`] parse node.
    pub fn left_hand_side_expression(self) -> Rc<LeftHandSideExpression> {
        LeftHandSideExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LexicalBinding`] parse node.
    pub fn lexical_binding(self) -> Rc<LexicalBinding> {
        LexicalBinding::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LexicalDeclaration`] parse node.
    pub fn lexical_declaration(self) -> Rc<LexicalDeclaration> {
        LexicalDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`Literal`] parse node.
    pub fn literal(self) -> Rc<Literal> {
        Literal::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`LiteralPropertyName`] parse node.
    pub fn literal_property_name(self) -> Rc<LiteralPropertyName> {
        LiteralPropertyName::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`LogicalANDExpression`] parse node.
    pub fn logical_and_expression(self) -> Rc<LogicalANDExpression> {
        LogicalANDExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`LogicalORExpression`] parse node.
    pub fn logical_or_expression(self) -> Rc<LogicalORExpression> {
        LogicalORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`MemberExpression`] parse node.
    pub fn member_expression(self) -> Rc<MemberExpression> {
        MemberExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`MetaProperty`] parse node.
    pub fn meta_property(self) -> Rc<MetaProperty> {
        MetaProperty::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`MethodDefinition`] parse node.
    pub fn method_definition(self) -> Rc<MethodDefinition> {
        MethodDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`MultiplicativeExpression`] parse node.
    pub fn multiplicative_expression(self) -> Rc<MultiplicativeExpression> {
        MultiplicativeExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`NewExpression`] parse node.
    pub fn new_expression(self) -> Rc<NewExpression> {
        NewExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectAssignmentPattern`] parse node.
    pub fn object_assignment_pattern(self) -> Rc<ObjectAssignmentPattern> {
        ObjectAssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectBindingPattern`] parse node.
    pub fn object_binding_pattern(self) -> Rc<ObjectBindingPattern> {
        ObjectBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectLiteral`] parse node.
    pub fn object_literal(self) -> Rc<ObjectLiteral> {
        ObjectLiteral::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`OptionalChain`] parse node.
    pub fn optional_chain(self) -> Rc<OptionalChain> {
        OptionalChain::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`OptionalExpression`] parse node.
    pub fn optional_expression(self) -> Rc<OptionalExpression> {
        OptionalExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ParenthesizedExpression`] parse node.
    pub fn parenthesized_expression(self) -> Rc<ParenthesizedExpression> {
        ParenthesizedExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`PrimaryExpression`] parse node.
    pub fn primary_expression(self) -> Rc<PrimaryExpression> {
        PrimaryExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyDefinition`] parse node.
    pub fn property_definition(self) -> Rc<PropertyDefinition> {
        PropertyDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyDefinitionList`] parse node.
    pub fn property_definition_list(self) -> Rc<PropertyDefinitionList> {
        PropertyDefinitionList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyName`] parse node.
    pub fn property_name(self) -> Rc<PropertyName> {
        PropertyName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertySetParameterList`] parse node.
    pub fn property_set_parameter_list(self) -> Rc<PropertySetParameterList> {
        PropertySetParameterList::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`RelationalExpression`] parse node.
    pub fn relational_expression(self) -> Rc<RelationalExpression> {
        RelationalExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ReturnStatement`] parse node.
    pub fn return_statement(self) -> Rc<ReturnStatement> {
        ReturnStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ScriptBody`] parse node.
    pub fn script_body(self) -> Rc<ScriptBody> {
        ScriptBody::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Script`] parse node.
    pub fn script(self) -> Rc<Script> {
        Script::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ShiftExpression`] parse node.
    pub fn shift_expression(self) -> Rc<ShiftExpression> {
        ShiftExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ShortCircuitExpression`] parse node.
    pub fn short_circuit_expression(self) -> Rc<ShortCircuitExpression> {
        ShortCircuitExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`SingleNameBinding`] parse node.
    pub fn single_name_binding(self) -> Rc<SingleNameBinding> {
        SingleNameBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`SpreadElement`] parse node.
    pub fn spread_element(self) -> Rc<SpreadElement> {
        SpreadElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`Statement`] parse node.
    pub fn statement(self) -> Rc<Statement> {
        Statement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`StatementList`] parse node.
    pub fn statement_list(self) -> Rc<StatementList> {
        StatementList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`StatementListItem`] parse node.
    pub fn statement_list_item(self) -> Rc<StatementListItem> {
        StatementListItem::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`SubstitutionTemplate`] parse node.
    pub fn substitution_template(self) -> Rc<SubstitutionTemplate> {
        SubstitutionTemplate::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`SuperCall`] parse node.
    pub fn super_call(self) -> Rc<SuperCall> {
        SuperCall::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`SuperProperty`] parse node.
    pub fn super_property(self) -> Rc<SuperProperty> {
        SuperProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`SwitchStatement`] parse node.
    pub fn switch_statement(self) -> Rc<SwitchStatement> {
        SwitchStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`TemplateLiteral`] parse node.
    pub fn template_literal(self) -> Rc<TemplateLiteral> {
        TemplateLiteral::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`TemplateSpans`] parse node.
    pub fn template_spans(self) -> Rc<TemplateSpans> {
        TemplateSpans::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`ThrowStatement`] parse node.
    pub fn throw_statement(self) -> Rc<ThrowStatement> {
        ThrowStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`TryStatement`] parse node.
    pub fn try_statement(self) -> Rc<TryStatement> {
        TryStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`UniqueFormalParameters`] parse node.
    pub fn unique_formal_parameters(self) -> Rc<UniqueFormalParameters> {
        UniqueFormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    /// Use the configs in the [`Maker`] object to make a [`UpdateExpression`] parse node.
    pub fn update_expression(self) -> Rc<UpdateExpression> {
        UpdateExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`UnaryExpression`] parse node.
    pub fn unary_expression(self) -> Rc<UnaryExpression> {
        UnaryExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`VariableDeclaration`] parse node.
    pub fn variable_declaration(self) -> Rc<VariableDeclaration> {
        VariableDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`VariableDeclarationList`] parse node.
    pub fn variable_declaration_list(self) -> Rc<VariableDeclarationList> {
        VariableDeclarationList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`VariableStatement`] parse node.
    pub fn variable_statement(self) -> Rc<VariableStatement> {
        VariableStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    /// Use the configs in the [`Maker`] object to make a [`WhileStatement`] parse node.
    pub fn while_statement(self) -> Rc<WhileStatement> {
        WhileStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`WithStatement`] parse node.
    pub fn with_statement(self) -> Rc<WithStatement> {
        WithStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0
    }
    /// Use the configs in the [`Maker`] object to make a [`YieldExpression`] parse node.
    pub fn yield_expression(self) -> Rc<YieldExpression> {
        YieldExpression::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.await_flag).unwrap().0
    }
}

pub const PACKAGE_NOT_ALLOWED: &str = "‘package’ not allowed as an identifier in strict mode";
pub const INTERFACE_NOT_ALLOWED: &str = "‘interface’ not allowed as an identifier in strict mode";
pub const IMPLEMENTS_NOT_ALLOWED: &str = "‘implements’ not allowed as an identifier in strict mode";
pub const CONTINUE_ITER: &str = "Continue statements must lie within iteration statements.";
pub const DUPLICATE_LEXICAL: &str = "Duplicate lexically declared names";
pub const LEX_DUPED_BY_VAR: &str = "Name defined both lexically and var-style";
pub const WITH_NOT_ALLOWED: &str = "'with' statements not allowed in strict mode";
pub const PRIVATE_NOT_ALLOWED: &str = "‘private’ not allowed as an identifier in strict mode";
pub const UNDEFINED_BREAK: &str = "undefined break target detected";
pub const DUPLICATE_LABELS: &str = "duplicate labels detected";
pub const UNEXPECTED_ARGS: &str = "‘arguments’ not expected here";
pub const UNEXPECTED_SUPER: &str = "Calls to ‘super’ not allowed here";
pub const UNEXPECTED_SUPER2: &str = "‘super’ not allowed here";
pub const UNDEF_CONT_TGT: &str = "undefined continue target detected";
pub const A_ALREADY_DEFN: &str = "‘a’ already defined";
pub const PRIVATE_CONSTRUCTOR: &str = "#constructor is an invalid private id";
pub const BAD_SUPER: &str = "super only allowed for constructors";
pub const CONSTRUCTOR_FIELD: &str = "constructors may not be defined as class fields";
pub const STATIC_PROTO: &str = "prototypes cannot be static";
pub const SPECIAL_CONSTRUCTOR: &str = "special methods not allowed for constructors";
pub const DUPLICATE_CONSTRUCTOR: &str = "Classes may have only one constructor";
pub const PRIVATE_A_ALREADY_DEFN: &str = "‘#a’ already defined";
pub const PREV_STATIC_GETTER: &str = "‘#a’ was previously defined as a static getter method.";
pub const PREV_GETTER: &str = "‘#a’ was previously defined as a getter method.";
pub const PREV_STATIC_SETTER: &str = "‘#a’ was previously defined as a static setter method.";
pub const PREV_SETTER: &str = "‘#a’ was previously defined as a setter method.";
pub const PARENTLESS_SUPER: &str = "Cannot use super in a constructor with no parent class";
pub const BAD_USE_STRICT: &str = "Illegal 'use strict' directive in function with non-simple parameter list";
pub const UNEXPECTED_AWAIT: &str = "await expressions not expected here";
pub const ILLEGAL_ASYNC_AWAIT: &str = "Illegal await-expression in formal parameters of async function";
pub const YIELD_IN_GENPARAM: &str = "Yield expressions can't be parameter initializers in generators";

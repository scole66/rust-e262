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
    assert_eq!(format!("{err}"), String::from(msg));
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
    assert_eq!(format!("{pe}"), msg.into());
}
pub fn sset(items: &[&str]) -> AHashSet<String> {
    items.iter().map(|&x| String::from(x)).collect()
}

pub fn svec(items: &[&str]) -> Vec<String> {
    items.iter().map(|&s| String::from(s)).collect::<Vec<_>>()
}
pub fn ssome(item: &str) -> Option<String> {
    Some(String::from(item))
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
#[expect(clippy::struct_excessive_bools)]
pub struct Maker<'a> {
    source: &'a str,
    yield_flag: bool,
    await_flag: bool,
    return_flag: bool,
    tagged_flag: bool,
    in_flag: bool,
    default_flag: bool,
    parent: FunctionBodyParent,
}
impl Default for Maker<'_> {
    fn default() -> Self {
        Maker {
            source: "",
            yield_flag: true,
            await_flag: true,
            return_flag: true,
            tagged_flag: false,
            in_flag: true,
            default_flag: true,
            parent: FunctionBodyParent::FunctionBody,
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
    #[must_use]
    pub fn yield_ok(self, yield_flag: bool) -> Self {
        Self { yield_flag, ..self }
    }
    /// Set the `await_flag` in the maker object.
    ///
    /// `true` means that await expressions are allowed; `false` means that the `await` keyword can be used as an
    /// identifier.
    #[must_use]
    pub fn await_ok(self, await_flag: bool) -> Self {
        Self { await_flag, ..self }
    }
    /// Set the `in_flag` in the maker object.
    ///
    /// `true` means that the `in` keyword may be used in a [`RelationalExpression`]; `false` means that it may not.
    /// (This is the case in some `for` statements.)
    #[must_use]
    pub fn in_ok(self, in_flag: bool) -> Self {
        Self { in_flag, ..self }
    }
    /// Set the `return_flag` in the maker object.
    ///
    /// `true` means that `return` statements are currently allowed (generally within function bodies); `false` means
    /// they are not.
    #[must_use]
    pub fn return_ok(self, return_flag: bool) -> Self {
        Self { return_flag, ..self }
    }
    /// Set the `tagged` flag in the maker object.
    ///
    /// `true` means that a template literal is being treated as a [tagged template][1]; `false` means that is is not.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-tagged-templates
    #[must_use]
    pub fn tagged_ok(self, tagged_flag: bool) -> Self {
        Self { tagged_flag, ..self }
    }
    /// Set the `default_flag` in the maker object.
    ///
    /// `true` means that nameless functions are allowed.
    #[must_use]
    pub fn default_ok(self, default_flag: bool) -> Self {
        Self { default_flag, ..self }
    }
    #[must_use]
    pub fn parent(self, parent: FunctionBodyParent) -> Self {
        Self { parent, ..self }
    }
    /// Use the configs in the [`Maker`] object to make a [`AdditiveExpression`] parse node.
    pub fn additive_expression(self) -> Rc<AdditiveExpression> {
        AdditiveExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn additive_expression_ast(self) -> (Rc<AdditiveExpression>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AdditiveExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AdditiveExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArgumentList`] parse node.
    pub fn argument_list(self) -> Rc<ArgumentList> {
        ArgumentList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn argument_list_ast(self) -> (Rc<ArgumentList>, SourceTree) {
        let source = self.source.to_string();
        let node = ArgumentList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArgumentList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Arguments`] parse node.
    pub fn arguments(self) -> Rc<Arguments> {
        Arguments::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn arguments_ast(self) -> (Rc<Arguments>, SourceTree) {
        let source = self.source.to_string();
        let node =
            Arguments::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Arguments(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrowFormalParameters`] parse node.
    pub fn arrow_formal_parameters(self) -> Rc<ArrowFormalParameters> {
        ArrowFormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn arrow_formal_parameters_ast(self) -> (Rc<ArrowFormalParameters>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ArrowFormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrowFormalParameters(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayAssignmentPattern`] parse node.
    pub fn array_assignment_pattern(self) -> Rc<ArrayAssignmentPattern> {
        ArrayAssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn array_assignment_pattern_ast(self) -> (Rc<ArrayAssignmentPattern>, SourceTree) {
        let source = self.source.to_string();
        let node = ArrayAssignmentPattern::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrayAssignmentPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayBindingPattern`] parse node.
    pub fn array_binding_pattern(self) -> Rc<ArrayBindingPattern> {
        ArrayBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn array_binding_pattern_ast(self) -> (Rc<ArrayBindingPattern>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ArrayBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrayBindingPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrayLiteral`] parse node.
    pub fn array_literal(self) -> Rc<ArrayLiteral> {
        ArrayLiteral::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn array_literal_ast(self) -> (Rc<ArrayLiteral>, SourceTree) {
        let source = self.source.to_string();
        let node = ArrayLiteral::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrayLiteral(node) })
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
    pub fn arrow_function_ast(self) -> (Rc<ArrowFunction>, SourceTree) {
        let source = self.source.to_string();
        let node = ArrowFunction::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrowFunction(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ArrowParameters`] parse node.
    pub fn arrow_parameters(self) -> Rc<ArrowParameters> {
        ArrowParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn arrow_parameters_ast(self) -> (Rc<ArrowParameters>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ArrowParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ArrowParameters(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentElement`] parse node.
    pub fn assignment_element(self) -> Rc<AssignmentElement> {
        AssignmentElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_element_ast(self) -> (Rc<AssignmentElement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AssignmentElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentElementList`] parse node.
    pub fn assignment_element_list(self) -> Rc<AssignmentElementList> {
        AssignmentElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_element_list_ast(self) -> (Rc<AssignmentElementList>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AssignmentElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentElementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentElisionElement`] parse node.
    pub fn assignment_elision_element(self) -> Rc<AssignmentElisionElement> {
        AssignmentElisionElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_elision_element_ast(self) -> (Rc<AssignmentElisionElement>, SourceTree) {
        let source = self.source.to_string();
        let node = AssignmentElisionElement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentElisionElement(node) })
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
    pub fn assignment_expression_ast(self) -> (Rc<AssignmentExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = AssignmentExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentPattern`] parse node.
    pub fn assignment_pattern(self) -> Rc<AssignmentPattern> {
        AssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_pattern_ast(self) -> (Rc<AssignmentPattern>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentProperty`] parse node.
    pub fn assignment_property(self) -> Rc<AssignmentProperty> {
        AssignmentProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_property_ast(self) -> (Rc<AssignmentProperty>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AssignmentProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentProperty(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentPropertyList`] parse node.
    pub fn assignment_property_list(self) -> Rc<AssignmentPropertyList> {
        AssignmentPropertyList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_property_list_ast(self) -> (Rc<AssignmentPropertyList>, SourceTree) {
        let source = self.source.to_string();
        let node = AssignmentPropertyList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentPropertyList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentRestElement`] parse node.
    pub fn assignment_rest_element(self) -> Rc<AssignmentRestElement> {
        AssignmentRestElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_rest_element_ast(self) -> (Rc<AssignmentRestElement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AssignmentRestElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentRestElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AssignmentRestProperty`] parse node.
    pub fn assignment_rest_property(self) -> Rc<AssignmentRestProperty> {
        AssignmentRestProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn assignment_rest_property_ast(self) -> (Rc<AssignmentRestProperty>, SourceTree) {
        let source = self.source.to_string();
        let node = AssignmentRestProperty::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AssignmentRestProperty(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncArrowBindingIdentifier`] parse node.
    pub fn async_arrow_binding_identifier(self) -> Rc<AsyncArrowBindingIdentifier> {
        AsyncArrowBindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0
    }
    pub fn async_arrow_binding_identifier_ast(self) -> (Rc<AsyncArrowBindingIdentifier>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AsyncArrowBindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncArrowBindingIdentifier(node) })
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
    pub fn async_arrow_function_ast(self) -> (Rc<AsyncArrowFunction>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncArrowFunction::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncArrowFunction(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncArrowHead`] parse node.
    pub fn async_arrow_head(self) -> Rc<AsyncArrowHead> {
        AsyncArrowHead::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn async_arrow_head_ast(self) -> (Rc<AsyncArrowHead>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncArrowHead::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncArrowHead(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncConciseBody`] parse node.
    pub fn async_concise_body(self) -> Rc<AsyncConciseBody> {
        AsyncConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0
    }
    pub fn async_concise_body_ast(self) -> (Rc<AsyncConciseBody>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncConciseBody(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncFunctionBody`] parse node.
    pub fn async_function_body(self) -> Rc<AsyncFunctionBody> {
        AsyncFunctionBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    pub fn async_function_body_ast(self) -> (Rc<AsyncFunctionBody>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncFunctionBody::parse(&mut newparser(self.source), Scanner::new()).0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncFunctionBody(node) })
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
    pub fn async_function_declaration_ast(self) -> (Rc<AsyncFunctionDeclaration>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncFunctionDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::AsyncFunctionDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncFunctionExpression`] parse node.
    pub fn async_function_expression(self) -> Rc<AsyncFunctionExpression> {
        AsyncFunctionExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn async_function_expression_ast(self) -> (Rc<AsyncFunctionExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncFunctionExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncFunctionExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorBody`] parse node.
    pub fn async_generator_body(self) -> Rc<AsyncGeneratorBody> {
        AsyncGeneratorBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    pub fn async_generator_body_ast(self) -> (Rc<AsyncGeneratorBody>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncGeneratorBody::parse(&mut newparser(self.source), Scanner::new()).0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncGeneratorBody(node) })
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
    pub fn async_generator_declaration_ast(self) -> (Rc<AsyncGeneratorDeclaration>, SourceTree) {
        let source = self.source;
        let node = AsyncGeneratorDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::AsyncGeneratorDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorExpression`] parse node.
    pub fn async_generator_expression(self) -> Rc<AsyncGeneratorExpression> {
        AsyncGeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn async_generator_expression_ast(self) -> (Rc<AsyncGeneratorExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncGeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncGeneratorExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncGeneratorMethod`] parse node.
    pub fn async_generator_method(self) -> Rc<AsyncGeneratorMethod> {
        AsyncGeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn async_generator_method_ast(self) -> (Rc<AsyncGeneratorMethod>, SourceTree) {
        let source = self.source.to_string();
        let node =
            AsyncGeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncGeneratorMethod(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AsyncMethod`] parse node.
    pub fn async_method(self) -> Rc<AsyncMethod> {
        AsyncMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn async_method_ast(self) -> (Rc<AsyncMethod>, SourceTree) {
        let source = self.source.to_string();
        let node = AsyncMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AsyncMethod(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`AwaitExpression`] parse node.
    pub fn await_expression(self) -> Rc<AwaitExpression> {
        AwaitExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0
    }
    pub fn await_expression_ast(self) -> (Rc<AwaitExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = AwaitExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::AwaitExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElement`] parse node.
    pub fn binding_element(self) -> Rc<BindingElement> {
        BindingElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn binding_element_ast(self) -> (Rc<BindingElement>, SourceTree) {
        let source = self.source.to_string();
        let node = BindingElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElementList`] parse node.
    pub fn binding_element_list(self) -> Rc<BindingElementList> {
        BindingElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_element_list_ast(self) -> (Rc<BindingElementList>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingElementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingElisionElement`] parse node.
    pub fn binding_elision_element(self) -> Rc<BindingElisionElement> {
        BindingElisionElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_elision_element_ast(self) -> (Rc<BindingElisionElement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingElisionElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingElisionElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingIdentifier`] parse node.
    pub fn binding_identifier(self) -> Rc<BindingIdentifier> {
        BindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_identifier_ast(self) -> (Rc<BindingIdentifier>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingIdentifier::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingIdentifier(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingList`] parse node.
    pub fn binding_list(self) -> Rc<BindingList> {
        BindingList::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_list_ast(self) -> (Rc<BindingList>, SourceTree) {
        let source = self.source.to_string();
        let node = BindingList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingPattern`] parse node.
    pub fn binding_pattern(self) -> Rc<BindingPattern> {
        BindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn binding_pattern_ast(self) -> (Rc<BindingPattern>, SourceTree) {
        let source = self.source.to_string();
        let node = BindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingProperty`] parse node.
    pub fn binding_property(self) -> Rc<BindingProperty> {
        BindingProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn binding_property_ast(self) -> (Rc<BindingProperty>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingProperty(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingPropertyList`] parse node.
    pub fn binding_property_list(self) -> Rc<BindingPropertyList> {
        BindingPropertyList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_property_list_ast(self) -> (Rc<BindingPropertyList>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingPropertyList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingPropertyList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingRestElement`] parse node.
    pub fn binding_rest_element(self) -> Rc<BindingRestElement> {
        BindingRestElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_rest_element_ast(self) -> (Rc<BindingRestElement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingRestElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingRestElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BindingRestProperty`] parse node.
    pub fn binding_rest_property(self) -> Rc<BindingRestProperty> {
        BindingRestProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn binding_rest_property_ast(self) -> (Rc<BindingRestProperty>, SourceTree) {
        let source = self.source.to_string();
        let node =
            BindingRestProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BindingRestProperty(node) })
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
    pub fn bitwise_and_expression_ast(self) -> (Rc<BitwiseANDExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = BitwiseANDExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BitwiseANDExpression(node) })
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
    pub fn bitwise_xor_expression_ast(self) -> (Rc<BitwiseXORExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = BitwiseXORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BitwiseXORExpression(node) })
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
    pub fn bitwise_or_expression_ast(self) -> (Rc<BitwiseORExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = BitwiseORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BitwiseORExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Block`] parse node.
    pub fn block(self) -> Rc<Block> {
        Block::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    pub fn block_ast(self) -> (Rc<Block>, SourceTree) {
        let source = self.source.to_string();
        let node = Block::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Block(node) })
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
    pub fn block_statement_ast(self) -> (Rc<BlockStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = BlockStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BlockStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`BreakStatement`] parse node.
    pub fn break_statement(self) -> Rc<BreakStatement> {
        BreakStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn break_statement_ast(self) -> (Rc<BreakStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = BreakStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BreakStatement(node) })
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
    pub fn breakable_statement_ast(self) -> (Rc<BreakableStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = BreakableStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::BreakableStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`CallExpression`] parse node.
    pub fn call_expression(self) -> Rc<CallExpression> {
        CallExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn call_expression_ast(self) -> (Rc<CallExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = CallExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CallExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`CallMemberExpression`] parse node.
    pub fn call_member_expression(self) -> Rc<CallMemberExpression> {
        CallMemberExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn call_member_expression_ast(self) -> (Rc<CallMemberExpression>, SourceTree) {
        let source = self.source.to_string();
        let node =
            CallMemberExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CallMemberExpression(node) })
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
    pub fn case_block_ast(self) -> (Rc<CaseBlock>, SourceTree) {
        let source = self.source.to_string();
        let node = CaseBlock::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CaseBlock(node) })
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
    pub fn case_clause_ast(self) -> (Rc<CaseClause>, SourceTree) {
        let source = self.source.to_string();
        let node = CaseClause::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CaseClause(node) })
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
    pub fn case_clauses_ast(self) -> (Rc<CaseClauses>, SourceTree) {
        let source = self.source.to_string();
        let node = CaseClauses::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CaseClauses(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Catch`] parse node.
    pub fn catch(self) -> Rc<Catch> {
        Catch::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    pub fn catch_ast(self) -> (Rc<Catch>, SourceTree) {
        let source = self.source.to_string();
        let node = Catch::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Catch(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`CatchParameter`] parse node.
    pub fn catch_parameter(self) -> Rc<CatchParameter> {
        CatchParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn catch_parameter_ast(self) -> (Rc<CatchParameter>, SourceTree) {
        let source = self.source.to_string();
        let node = CatchParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CatchParameter(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassBody`] parse node.
    pub fn class_body(self) -> Rc<ClassBody> {
        ClassBody::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn class_body_ast(self) -> (Rc<ClassBody>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ClassBody::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassBody(node) })
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
    pub fn class_declaration_ast(self) -> (Rc<ClassDeclaration>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElement`] parse node.
    pub fn class_element(self) -> Rc<ClassElement> {
        ClassElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn class_element_ast(self) -> (Rc<ClassElement>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassElement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElementList`] parse node.
    pub fn class_element_list(self) -> Rc<ClassElementList> {
        ClassElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn class_element_list_ast(self) -> (Rc<ClassElementList>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ClassElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassElementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassElementName`] parse node.
    pub fn class_element_name(self) -> Rc<ClassElementName> {
        ClassElementName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn class_element_name_ast(self) -> (Rc<ClassElementName>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ClassElementName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassElementName(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassExpression`] parse node.
    pub fn class_expression(self) -> Rc<ClassExpression> {
        ClassExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn class_expression_ast(self) -> (Rc<ClassExpression>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ClassExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassHeritage`] parse node.
    pub fn class_heritage(self) -> Rc<ClassHeritage> {
        ClassHeritage::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn class_heritage_ast(self) -> (Rc<ClassHeritage>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassHeritage::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassHeritage(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlock`] parse node.
    pub fn class_static_block(self) -> Rc<ClassStaticBlock> {
        ClassStaticBlock::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn class_static_block_ast(self) -> (Rc<ClassStaticBlock>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassStaticBlock::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassStaticBlock(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlockBody`] parse node.
    pub fn class_static_block_body(self) -> Rc<ClassStaticBlockBody> {
        ClassStaticBlockBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    pub fn class_static_block_body_ast(self) -> (Rc<ClassStaticBlockBody>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassStaticBlockBody::parse(&mut newparser(self.source), Scanner::new()).0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassStaticBlockBody(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassStaticBlockStatementList`] parse node.
    pub fn class_static_block_statement_list(self) -> Rc<ClassStaticBlockStatementList> {
        ClassStaticBlockStatementList::parse(&mut newparser(self.source), Scanner::new()).0
    }
    pub fn class_static_block_statement_list_ast(self) -> (Rc<ClassStaticBlockStatementList>, SourceTree) {
        let source = self.source.to_string();
        let node = ClassStaticBlockStatementList::parse(&mut newparser(self.source), Scanner::new()).0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassStaticBlockStatementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ClassTail`] parse node.
    pub fn class_tail(self) -> Rc<ClassTail> {
        ClassTail::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn class_tail_ast(self) -> (Rc<ClassTail>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ClassTail::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ClassTail(node) })
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
    pub fn coalesce_expression_ast(self) -> (Rc<CoalesceExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = CoalesceExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CoalesceExpression(node) })
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
    pub fn coalesce_expression_head_ast(self) -> (Rc<CoalesceExpressionHead>, SourceTree) {
        let source = self.source.to_string();
        let node = CoalesceExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
        .head
        .clone();
        (node.clone(), SourceTree { text: source, ast: ParsedText::CoalesceExpressionHead(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ComputedPropertyName`] parse node.
    pub fn computed_property_name(self) -> Rc<ComputedPropertyName> {
        ComputedPropertyName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn computed_property_name_ast(self) -> (Rc<ComputedPropertyName>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ComputedPropertyName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ComputedPropertyName(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ConciseBody`] parse node.
    pub fn concise_body(self) -> Rc<ConciseBody> {
        ConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0
    }
    pub fn concise_body_ast(self) -> (Rc<ConciseBody>, SourceTree) {
        let source = self.source.to_string();
        let node = ConciseBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ConciseBody(node) })
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
    pub fn conditional_expression_ast(self) -> (Rc<ConditionalExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = ConditionalExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ConditionalExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ContinueStatement`] parse node.
    pub fn continue_statement(self) -> Rc<ContinueStatement> {
        ContinueStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn continue_statement_ast(self) -> (Rc<ContinueStatement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ContinueStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ContinueStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`CoverInitializedName`] parse node.
    pub fn cover_initialized_name(self) -> Rc<CoverInitializedName> {
        CoverInitializedName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn cover_initialized_name_ast(self) -> (Rc<CoverInitializedName>, SourceTree) {
        let source = self.source.to_string();
        let node =
            CoverInitializedName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::CoverInitializedName(node) })
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
    pub fn cover_parenthesized_expression_and_arrow_parameter_list_ast(
        self,
    ) -> (Rc<CoverParenthesizedExpressionAndArrowParameterList>, SourceTree) {
        let source = self.source.to_string();
        let node = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (
            node.clone(),
            SourceTree { text: source, ast: ParsedText::CoverParenthesizedExpressionAndArrowParameterList(node) },
        )
    }
    /// Use the configs in the [`Maker`] object to make a [`DebuggerStatement`] parse node.
    pub fn debugger_statement(self) -> Rc<DebuggerStatement> {
        DebuggerStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn debugger_statement_ast(self) -> (Rc<DebuggerStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = DebuggerStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::DebuggerStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Declaration`] parse node.
    pub fn declaration(self) -> Rc<Declaration> {
        Declaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn declaration_ast(self) -> (Rc<Declaration>, SourceTree) {
        let source = self.source.to_string();
        let node = Declaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Declaration(node) })
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
    pub fn default_clause_ast(self) -> (Rc<DefaultClause>, SourceTree) {
        let source = self.source.to_string();
        let node = DefaultClause::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::DefaultClause(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`DestructuringAssignmentTarget`] parse node.
    pub fn destructuring_assignment_target(self) -> Rc<DestructuringAssignmentTarget> {
        DestructuringAssignmentTarget::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0
    }
    pub fn destructuring_assignment_target_ast(self) -> (Rc<DestructuringAssignmentTarget>, SourceTree) {
        let source = self.source.to_string();
        let node = DestructuringAssignmentTarget::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::DestructuringAssignmentTarget(node) })
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
    pub fn do_while_statement_ast(self) -> (Rc<DoWhileStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = DoWhileStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::DoWhileStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Elisions`] parse node.
    pub fn elision(self) -> Rc<Elisions> {
        Elisions::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn elision_ast(self) -> (Rc<Elisions>, SourceTree) {
        let source = self.source.to_string();
        let node = Elisions::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Elisions(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ElementList`] parse node.
    pub fn element_list(self) -> Rc<ElementList> {
        ElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn element_list_ast(self) -> (Rc<ElementList>, SourceTree) {
        let source = self.source.to_string();
        let node = ElementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ElementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`EmptyStatement`] parse node.
    pub fn empty_statement(self) -> Rc<EmptyStatement> {
        EmptyStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn empty_statement_ast(self) -> (Rc<EmptyStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = EmptyStatement::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::EmptyStatement(node) })
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
    pub fn equality_expression_ast(self) -> (Rc<EqualityExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = EqualityExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::EqualityExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Expression`] parse node.
    pub fn expression(self) -> Rc<Expression> {
        Expression::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn expression_ast(self) -> (Rc<Expression>, SourceTree) {
        let source = self.source.to_string();
        let node = Expression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Expression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ExpressionBody`] parse node.
    pub fn expression_body(self) -> Rc<ExpressionBody> {
        ExpressionBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.await_flag).unwrap().0
    }
    pub fn expression_body_ast(self) -> (Rc<ExpressionBody>, SourceTree) {
        let source = self.source.to_string();
        let node = ExpressionBody::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.yield_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ExpressionBody(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ExpressionStatement`] parse node.
    pub fn expression_statement(self) -> Rc<ExpressionStatement> {
        ExpressionStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn expression_statement_ast(self) -> (Rc<ExpressionStatement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ExpressionStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ExpressionStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ExponentiationExpression`] parse node.
    pub fn exponentiation_expression(self) -> Rc<ExponentiationExpression> {
        ExponentiationExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn exponentiation_expression_ast(self) -> (Rc<ExponentiationExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = ExponentiationExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ExponentiationExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FieldDefinition`] parse node.
    pub fn field_definition(self) -> Rc<FieldDefinition> {
        FieldDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn field_definition_ast(self) -> (Rc<FieldDefinition>, SourceTree) {
        let source = self.source;
        let node =
            FieldDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::FieldDefinition(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Finally`] parse node.
    pub fn finally(self) -> Rc<Finally> {
        Finally::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.return_flag)
            .unwrap()
            .0
    }
    pub fn finally_ast(self) -> (Rc<Finally>, SourceTree) {
        let source = self.source.to_string();
        let node = Finally::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Finally(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ForBinding`] parse node.
    pub fn for_binding(self) -> Rc<ForBinding> {
        ForBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn for_binding_ast(self) -> (Rc<ForBinding>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ForBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ForBinding(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ForDeclaration`] parse node.
    pub fn for_declaration(self) -> Rc<ForDeclaration> {
        ForDeclaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn for_declaration_ast(self) -> (Rc<ForDeclaration>, SourceTree) {
        let source = self.source.to_string();
        let node = ForDeclaration::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ForDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameter`] parse node.
    pub fn formal_parameter(self) -> Rc<FormalParameter> {
        FormalParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn formal_parameter_ast(self) -> (Rc<FormalParameter>, SourceTree) {
        let source = self.source.to_string();
        let node =
            FormalParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::FormalParameter(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameterList`] parse node.
    pub fn formal_parameter_list(self) -> Rc<FormalParameterList> {
        FormalParameterList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn formal_parameter_list_ast(self) -> (Rc<FormalParameterList>, SourceTree) {
        let source = self.source.to_string();
        let node =
            FormalParameterList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::FormalParameterList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FormalParameters`] parse node.
    pub fn formal_parameters(self) -> Rc<FormalParameters> {
        FormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    pub fn formal_parameters_ast(self) -> (Rc<FormalParameters>, SourceTree) {
        let source = self.source.to_string();
        let node =
            FormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::FormalParameters(node) })
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
    pub fn for_in_of_statement_ast(self) -> (Rc<ForInOfStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = ForInOfStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ForInOfStatement(node) })
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
    pub fn for_statement_ast(self) -> (Rc<ForStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = ForStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ForStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionBody`] parse node.
    pub fn function_body(self) -> Rc<FunctionBody> {
        FunctionBody::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag, self.parent)
            .0
    }
    pub fn function_body_ast(self) -> (Rc<FunctionBody>, SourceTree) {
        let source = self.source.to_string();
        let node = FunctionBody::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.parent,
        )
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::FunctionBody(node) })
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
    pub fn function_declaration_ast(self) -> (Rc<FunctionDeclaration>, SourceTree) {
        let source = self.source;
        let node = FunctionDeclaration::parse(
            &mut newparser(source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::FunctionDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionExpression`] parse node.
    pub fn function_expression(self) -> Rc<FunctionExpression> {
        FunctionExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn function_expression_ast(self) -> (Rc<FunctionExpression>, SourceTree) {
        let source = self.source;
        let node = FunctionExpression::parse(&mut newparser(source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::FunctionExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionRestParameter`] parse node.
    pub fn function_rest_parameter(self) -> Rc<FunctionRestParameter> {
        FunctionRestParameter::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn function_rest_parameter_ast(self) -> (Rc<FunctionRestParameter>, SourceTree) {
        let source = self.source;
        let node =
            FunctionRestParameter::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::FunctionRestParameter(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`FunctionStatementList`] parse node.
    pub fn function_statement_list(self) -> Rc<FunctionStatementList> {
        FunctionStatementList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    pub fn function_statement_list_ast(self) -> (Rc<FunctionStatementList>, SourceTree) {
        let source = self.source;
        let node =
            FunctionStatementList::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag).0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::FunctionStatementList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorBody`] parse node.
    pub fn generator_body(self) -> Rc<GeneratorBody> {
        GeneratorBody::parse(&mut newparser(self.source), Scanner::new()).0
    }
    pub fn generator_body_ast(self) -> (Rc<GeneratorBody>, SourceTree) {
        let source = self.source;
        let node = GeneratorBody::parse(&mut newparser(source), Scanner::new()).0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::GeneratorBody(node) })
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
    pub fn generator_declaration_ast(self) -> (Rc<GeneratorDeclaration>, SourceTree) {
        let source = self.source;
        let node = GeneratorDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.default_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::GeneratorDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorExpression`] parse node.
    pub fn generator_expression(self) -> Rc<GeneratorExpression> {
        GeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn generator_expression_ast(self) -> (Rc<GeneratorExpression>, SourceTree) {
        let node = GeneratorExpression::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::GeneratorExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`GeneratorMethod`] parse node.
    pub fn generator_method(self) -> Rc<GeneratorMethod> {
        GeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn generator_method_ast(self) -> (Rc<GeneratorMethod>, SourceTree) {
        let node =
            GeneratorMethod::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::GeneratorMethod(node) })
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
    pub fn if_statement_ast(self) -> (Rc<IfStatement>, SourceTree) {
        let node = IfStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::IfStatement(node) })
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
    pub fn initializer_ast(self) -> (Rc<Initializer>, SourceTree) {
        let node = Initializer::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::Initializer(node) })
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
    pub fn iteration_statement_ast(self) -> (Rc<IterationStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = IterationStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::IterationStatement(node) })
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
    pub fn labelled_item_ast(self) -> (Rc<LabelledItem>, SourceTree) {
        let source = self.source.to_string();
        let node = LabelledItem::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LabelledItem(node) })
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
    pub fn labelled_statement_ast(self) -> (Rc<LabelledStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = LabelledStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LabelledStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`LeftHandSideExpression`] parse node.
    pub fn left_hand_side_expression(self) -> Rc<LeftHandSideExpression> {
        LeftHandSideExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn left_hand_side_expression_ast(self) -> (Rc<LeftHandSideExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = LeftHandSideExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LeftHandSideExpression(node) })
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
    pub fn lexical_binding_ast(self) -> (Rc<LexicalBinding>, SourceTree) {
        let node = LexicalBinding::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::LexicalBinding(node) })
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
    pub fn lexical_declaration_ast(self) -> (Rc<LexicalDeclaration>, SourceTree) {
        let node = LexicalDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::LexicalDeclaration(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Literal`] parse node.
    pub fn literal(self) -> Rc<Literal> {
        Literal::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn literal_ast(self) -> (Rc<Literal>, SourceTree) {
        let node = Literal::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: self.source.to_string(), ast: ParsedText::Literal(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`LiteralPropertyName`] parse node.
    pub fn literal_property_name(self) -> Rc<LiteralPropertyName> {
        LiteralPropertyName::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn literal_property_name_ast(self) -> (Rc<LiteralPropertyName>, SourceTree) {
        let source = self.source.to_string();
        let node = LiteralPropertyName::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LiteralPropertyName(node) })
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
    pub fn logical_and_expression_ast(self) -> (Rc<LogicalANDExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = LogicalANDExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LogicalANDExpression(node) })
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
    pub fn logical_or_expression_ast(self) -> (Rc<LogicalORExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = LogicalORExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::LogicalORExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`MemberExpression`] parse node.
    pub fn member_expression(self) -> Rc<MemberExpression> {
        MemberExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn member_expression_ast(self) -> (Rc<MemberExpression>, SourceTree) {
        let source = self.source;
        let node = MemberExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::MemberExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`MetaProperty`] parse node.
    pub fn meta_property(self) -> Rc<MetaProperty> {
        MetaProperty::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn meta_property_ast(self) -> (Rc<MetaProperty>, SourceTree) {
        let source = self.source.to_string();
        let node = MetaProperty::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::MetaProperty(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`MethodDefinition`] parse node.
    pub fn method_definition(self) -> Rc<MethodDefinition> {
        MethodDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn method_definition_ast(self) -> (Rc<MethodDefinition>, SourceTree) {
        let source = self.source.to_string();
        let node =
            MethodDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::MethodDefinition(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`MultiplicativeExpression`] parse node.
    pub fn multiplicative_expression(self) -> Rc<MultiplicativeExpression> {
        MultiplicativeExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn multiplicative_expression_ast(self) -> (Rc<MultiplicativeExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = MultiplicativeExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::MultiplicativeExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`NewExpression`] parse node.
    pub fn new_expression(self) -> Rc<NewExpression> {
        NewExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn new_expression_ast(self) -> (Rc<NewExpression>, SourceTree) {
        let source = self.source;
        let node =
            NewExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::NewExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectAssignmentPattern`] parse node.
    pub fn object_assignment_pattern(self) -> Rc<ObjectAssignmentPattern> {
        ObjectAssignmentPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn object_assignment_pattern_ast(self) -> (Rc<ObjectAssignmentPattern>, SourceTree) {
        let source = self.source.to_string();
        let node = ObjectAssignmentPattern::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ObjectAssignmentPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectBindingPattern`] parse node.
    pub fn object_binding_pattern(self) -> Rc<ObjectBindingPattern> {
        ObjectBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn object_binding_pattern_ast(self) -> (Rc<ObjectBindingPattern>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ObjectBindingPattern::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ObjectBindingPattern(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ObjectLiteral`] parse node.
    pub fn object_literal(self) -> Rc<ObjectLiteral> {
        ObjectLiteral::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn object_literal_ast(self) -> (Rc<ObjectLiteral>, SourceTree) {
        let source = self.source;
        let node =
            ObjectLiteral::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::ObjectLiteral(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`OptionalChain`] parse node.
    pub fn optional_chain(self) -> Rc<OptionalChain> {
        OptionalChain::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn optional_chain_ast(self) -> (Rc<OptionalChain>, SourceTree) {
        let source = self.source.to_string();
        let node = OptionalChain::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::OptionalChain(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`OptionalExpression`] parse node.
    pub fn optional_expression(self) -> Rc<OptionalExpression> {
        OptionalExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn optional_expression_ast(self) -> (Rc<OptionalExpression>, SourceTree) {
        let source = self.source.to_string();
        let node =
            OptionalExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::OptionalExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ParenthesizedExpression`] parse node.
    pub fn parenthesized_expression(self) -> Rc<ParenthesizedExpression> {
        ParenthesizedExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn parenthesized_expression_ast(self) -> (Rc<ParenthesizedExpression>, SourceTree) {
        let source = self.source;
        let node =
            ParenthesizedExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::ParenthesizedExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`PrimaryExpression`] parse node.
    pub fn primary_expression(self) -> Rc<PrimaryExpression> {
        PrimaryExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn primary_expression_ast(self) -> (Rc<PrimaryExpression>, SourceTree) {
        let source = self.source;
        let node = PrimaryExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::PrimaryExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyDefinition`] parse node.
    pub fn property_definition(self) -> Rc<PropertyDefinition> {
        PropertyDefinition::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn property_definition_ast(self) -> (Rc<PropertyDefinition>, SourceTree) {
        let source = self.source;
        let node = PropertyDefinition::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::PropertyDefinition(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyDefinitionList`] parse node.
    pub fn property_definition_list(self) -> Rc<PropertyDefinitionList> {
        PropertyDefinitionList::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn property_definition_list_ast(self) -> (Rc<PropertyDefinitionList>, SourceTree) {
        let source = self.source;
        let node =
            PropertyDefinitionList::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::PropertyDefinitionList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertyName`] parse node.
    pub fn property_name(self) -> Rc<PropertyName> {
        PropertyName::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn property_name_ast(self) -> (Rc<PropertyName>, SourceTree) {
        let source = self.source;
        let node =
            PropertyName::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::PropertyName(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`PropertySetParameterList`] parse node.
    pub fn property_set_parameter_list(self) -> Rc<PropertySetParameterList> {
        PropertySetParameterList::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn property_set_parameter_list_ast(self) -> (Rc<PropertySetParameterList>, SourceTree) {
        let source = self.source.to_string();
        let node = PropertySetParameterList::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::PropertySetParameterList(node) })
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
    pub fn relational_expression_ast(self) -> (Rc<RelationalExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = RelationalExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::RelationalExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ReturnStatement`] parse node.
    pub fn return_statement(self) -> Rc<ReturnStatement> {
        ReturnStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn return_statement_ast(self) -> (Rc<ReturnStatement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ReturnStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ReturnStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ScriptBody`] parse node.
    pub fn script_body(self) -> Rc<ScriptBody> {
        ScriptBody::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn script_body_ast(self) -> (Rc<ScriptBody>, SourceTree) {
        let source = self.source.to_string();
        let node = ScriptBody::parse(&mut newparser(self.source), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ScriptBody(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`Script`] parse node.
    pub fn script(self) -> Rc<Script> {
        Script::parse(&mut newparser(self.source), Scanner::new()).unwrap().0
    }
    pub fn script_ast(self) -> (Rc<Script>, SourceTree) {
        let src = self.source;
        let node = Script::parse(&mut newparser(src), Scanner::new()).unwrap().0;
        (node.clone(), SourceTree { ast: ParsedText::Script(node), text: String::from(src) })
    }

    /// Use the configs in the [`Maker`] object to make a [`ShiftExpression`] parse node.
    pub fn shift_expression(self) -> Rc<ShiftExpression> {
        ShiftExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn shift_expression_ast(self) -> (Rc<ShiftExpression>, SourceTree) {
        let source = self.source.to_string();
        let node =
            ShiftExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ShiftExpression(node) })
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
    pub fn short_circuit_expression_ast(self) -> (Rc<ShortCircuitExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = ShortCircuitExpression::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ShortCircuitExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`SingleNameBinding`] parse node.
    pub fn single_name_binding(self) -> Rc<SingleNameBinding> {
        SingleNameBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn single_name_binding_ast(self) -> (Rc<SingleNameBinding>, SourceTree) {
        let source = self.source.to_string();
        let node =
            SingleNameBinding::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SingleNameBinding(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`SpreadElement`] parse node.
    pub fn spread_element(self) -> Rc<SpreadElement> {
        SpreadElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn spread_element_ast(self) -> (Rc<SpreadElement>, SourceTree) {
        let source = self.source.to_string();
        let node = SpreadElement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SpreadElement(node) })
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
    pub fn statement_ast(self) -> (Rc<Statement>, SourceTree) {
        let source = self.source.to_string();
        let node = Statement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::Statement(node) })
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
    pub fn statement_list_ast(self) -> (Rc<StatementList>, SourceTree) {
        let source = self.source.to_string();
        let node = StatementList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::StatementList(node) })
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
    pub fn statement_list_item_ast(self) -> (Rc<StatementListItem>, SourceTree) {
        let source = self.source.to_string();
        let node = StatementListItem::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::StatementListItem(node) })
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
    pub fn substitution_template_ast(self) -> (Rc<SubstitutionTemplate>, SourceTree) {
        let source = self.source.to_string();
        let node = SubstitutionTemplate::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SubstitutionTemplate(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`SuperCall`] parse node.
    pub fn super_call(self) -> Rc<SuperCall> {
        SuperCall::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn super_call_ast(self) -> (Rc<SuperCall>, SourceTree) {
        let source = self.source.to_string();
        let node =
            SuperCall::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SuperCall(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`SuperProperty`] parse node.
    pub fn super_property(self) -> Rc<SuperProperty> {
        SuperProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn super_property_ast(self) -> (Rc<SuperProperty>, SourceTree) {
        let source = self.source.to_string();
        let node = SuperProperty::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SuperProperty(node) })
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
    pub fn switch_statement_ast(self) -> (Rc<SwitchStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = SwitchStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::SwitchStatement(node) })
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
    pub fn template_literal_ast(self) -> (Rc<TemplateLiteral>, SourceTree) {
        let source = self.source.to_string();
        let node = TemplateLiteral::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::TemplateLiteral(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`TemplateMiddleList`] parse node.
    pub fn template_middle_list(self) -> Rc<TemplateMiddleList> {
        TemplateMiddleList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0
    }
    pub fn template_middle_list_ast(self) -> (Rc<TemplateMiddleList>, SourceTree) {
        let source = self.source.to_string();
        let node = TemplateMiddleList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::TemplateMiddleList(node) })
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
    pub fn template_spans_ast(self) -> (Rc<TemplateSpans>, SourceTree) {
        let source = self.source.to_string();
        let node = TemplateSpans::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.tagged_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::TemplateSpans(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`ThrowStatement`] parse node.
    pub fn throw_statement(self) -> Rc<ThrowStatement> {
        ThrowStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn throw_statement_ast(self) -> (Rc<ThrowStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = ThrowStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::ThrowStatement(node) })
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
    pub fn try_statement_ast(self) -> (Rc<TryStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = TryStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::TryStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`UniqueFormalParameters`] parse node.
    pub fn unique_formal_parameters(self) -> Rc<UniqueFormalParameters> {
        UniqueFormalParameters::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).0
    }
    pub fn unique_formal_parameters_ast(self) -> (Rc<UniqueFormalParameters>, SourceTree) {
        let source = self.source.to_string();
        let node = UniqueFormalParameters::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
        )
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::UniqueFormalParameters(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`UpdateExpression`] parse node.
    pub fn update_expression(self) -> Rc<UpdateExpression> {
        UpdateExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn update_expression_ast(self) -> (Rc<UpdateExpression>, SourceTree) {
        let source = self.source;
        let node = UpdateExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::UpdateExpression(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`UnaryExpression`] parse node.
    pub fn unary_expression(self) -> Rc<UnaryExpression> {
        UnaryExpression::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    pub fn unary_expression_ast(self) -> (Rc<UnaryExpression>, SourceTree) {
        let source = self.source;
        let node =
            UnaryExpression::parse(&mut newparser(source), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0;
        (node.clone(), SourceTree { text: source.to_string(), ast: ParsedText::UnaryExpression(node) })
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
    pub fn variable_declaration_ast(self) -> (Rc<VariableDeclaration>, SourceTree) {
        let source = self.source.to_string();
        let node = VariableDeclaration::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::VariableDeclaration(node) })
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
    pub fn variable_declaration_list_ast(self) -> (Rc<VariableDeclarationList>, SourceTree) {
        let source = self.source.to_string();
        let node = VariableDeclarationList::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.in_flag,
            self.yield_flag,
            self.await_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::VariableDeclarationList(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`VariableStatement`] parse node.
    pub fn variable_statement(self) -> Rc<VariableStatement> {
        VariableStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
            .unwrap()
            .0
    }
    pub fn variable_statement_ast(self) -> (Rc<VariableStatement>, SourceTree) {
        let source = self.source.to_string();
        let node =
            VariableStatement::parse(&mut newparser(self.source), Scanner::new(), self.yield_flag, self.await_flag)
                .unwrap()
                .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::VariableStatement(node) })
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
    pub fn while_statement_ast(self) -> (Rc<WhileStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = WhileStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::WhileStatement(node) })
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
    pub fn with_statement_ast(self) -> (Rc<WithStatement>, SourceTree) {
        let source = self.source.to_string();
        let node = WithStatement::parse(
            &mut newparser(self.source),
            Scanner::new(),
            self.yield_flag,
            self.await_flag,
            self.return_flag,
        )
        .unwrap()
        .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::WithStatement(node) })
    }
    /// Use the configs in the [`Maker`] object to make a [`YieldExpression`] parse node.
    pub fn yield_expression(self) -> Rc<YieldExpression> {
        YieldExpression::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.await_flag).unwrap().0
    }
    pub fn yield_expression_ast(self) -> (Rc<YieldExpression>, SourceTree) {
        let source = self.source.to_string();
        let node = YieldExpression::parse(&mut newparser(self.source), Scanner::new(), self.in_flag, self.await_flag)
            .unwrap()
            .0;
        (node.clone(), SourceTree { text: source, ast: ParsedText::YieldExpression(node) })
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
pub const UNEXPECTED_AWAIT: &str = "Await expressions can't be parameter initializers in async functions";
pub const ILLEGAL_ASYNC_AWAIT: &str = "Illegal await-expression in formal parameters of async function";
pub const YIELD_IN_GENPARAM: &str = "Yield expressions can't be parameter initializers in generators";
pub const AWAIT_IN_CLASS_STATIC: &str = "Cannot use await in class static initialization block";
pub const BAD_EVAL: &str = "identifier not allowed in strict mode: eval";
pub const BAD_ARGUMENTS: &str = "identifier not allowed in strict mode: arguments";

use super::*;
use iteration_statements::{ForBinding, ForDeclaration};

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
    Parser::new(text, false, false, ParseGoal::Script)
}
pub fn strictparser(text: &str, strict: bool) -> Parser {
    Parser::new(text, strict, false, ParseGoal::Script)
}
pub fn check_parse_error<T, U>(result: ParseResult<T>, msg: U)
where
    T: fmt::Debug,
    U: Into<String>,
{
    let pe = result.unwrap_err();
    assert_eq!(pe.location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } });
    assert_eq!(format!("{}", pe), msg.into());
}
pub fn set(items: &[&str]) -> AHashSet<String> {
    AHashSet::from_iter(items.iter().map(|&x| String::from(x)))
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
pub struct Maker<'a> {
    source: &'a str,
    strict: bool,
    yield_flag: bool,
    await_flag: bool,
    return_flag: bool,
    tagged_flag: bool,
    in_flag: bool,
}
impl<'a> Default for Maker<'a> {
    fn default() -> Self {
        Maker { source: "", strict: false, yield_flag: true, await_flag: true, return_flag: true, tagged_flag: false, in_flag: true }
    }
}

impl<'a> Maker<'a> {
    /// Construct a new Maker object with the given source.
    pub fn new(src: &'a str) -> Self {
        Maker { source: src, ..Default::default() }
    }
    /// Set the "strict parsing" flag of the maker to the value given by `strict`.
    pub fn strict(self, strict: bool) -> Self {
        Self { strict, ..self }
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
    /// Use the configs in the [`Maker`] object to make a [`ForBinding`] parse node.
    pub fn for_binding(self) -> Rc<ForBinding> {
        ForBinding::parse(&mut strictparser(self.source, self.strict), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
    }
    /// Use the configs in the [`Maker`] object to make a [`ForDeclaration`] parse node.
    pub fn for_declaration(self) -> Rc<ForDeclaration> {
        ForDeclaration::parse(&mut strictparser(self.source, self.strict), Scanner::new(), self.yield_flag, self.await_flag).unwrap().0
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

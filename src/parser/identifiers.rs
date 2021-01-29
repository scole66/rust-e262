use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub enum Identifier {
    IdentifierName(scanner::IdentifierData),
}

impl StringValue for Identifier {
    fn string_value(&self) -> scanner::JSString {
        let Identifier::IdentifierName(identifier_name) = self;
        identifier_name.string_value.clone()
    }
}

impl PrettyPrint for Identifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}Identifier: {}", first, self.string_value())
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.pprint_with_leftpad(writer, pad, state)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_value())
    }
}

impl Identifier {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Identifier(id) => match id.keyword_id {
                Some(scanner::Keyword::Await)
                | Some(scanner::Keyword::Break)
                | Some(scanner::Keyword::Case)
                | Some(scanner::Keyword::Catch)
                | Some(scanner::Keyword::Class)
                | Some(scanner::Keyword::Const)
                | Some(scanner::Keyword::Continue)
                | Some(scanner::Keyword::Debugger)
                | Some(scanner::Keyword::Default)
                | Some(scanner::Keyword::Delete)
                | Some(scanner::Keyword::Do)
                | Some(scanner::Keyword::Else)
                | Some(scanner::Keyword::Enum)
                | Some(scanner::Keyword::Export)
                | Some(scanner::Keyword::Extends)
                | Some(scanner::Keyword::False)
                | Some(scanner::Keyword::Finally)
                | Some(scanner::Keyword::For)
                | Some(scanner::Keyword::Function)
                | Some(scanner::Keyword::If)
                | Some(scanner::Keyword::Import)
                | Some(scanner::Keyword::In)
                | Some(scanner::Keyword::Instanceof)
                | Some(scanner::Keyword::New)
                | Some(scanner::Keyword::Null)
                | Some(scanner::Keyword::Return)
                | Some(scanner::Keyword::Super)
                | Some(scanner::Keyword::Switch)
                | Some(scanner::Keyword::This)
                | Some(scanner::Keyword::Throw)
                | Some(scanner::Keyword::True)
                | Some(scanner::Keyword::Try)
                | Some(scanner::Keyword::Typeof)
                | Some(scanner::Keyword::Var)
                | Some(scanner::Keyword::Void)
                | Some(scanner::Keyword::While)
                | Some(scanner::Keyword::With)
                | Some(scanner::Keyword::Yield) => Ok(None),
                _ => {
                    if parser.strict
                        && (id.string_value == "implements"
                            || id.string_value == "interface"
                            || id.string_value == "let"
                            || id.string_value == "package"
                            || id.string_value == "private"
                            || id.string_value == "protected"
                            || id.string_value == "public"
                            || id.string_value == "static"
                            || id.string_value == "yield")
                    {
                        Err(format!(
                            "{}:{}: ‘{}’ not allowed as an identifier in strict mode",
                            id.line, id.column, id.string_value
                        ))
                    } else if parser.goal == ParseGoal::Module && id.string_value == "await" {
                        Err(format!(
                            "{}:{}: ‘await’ not allowed as an identifier in modules",
                            id.line, id.column
                        ))
                    } else if id.string_value == "break"
                        || id.string_value == "case"
                        || id.string_value == "catch"
                        || id.string_value == "class"
                        || id.string_value == "const"
                        || id.string_value == "continue"
                        || id.string_value == "debugger"
                        || id.string_value == "default"
                        || id.string_value == "delete"
                        || id.string_value == "do"
                        || id.string_value == "else"
                        || id.string_value == "enum"
                        || id.string_value == "export"
                        || id.string_value == "extends"
                        || id.string_value == "false"
                        || id.string_value == "finally"
                        || id.string_value == "for"
                        || id.string_value == "function"
                        || id.string_value == "if"
                        || id.string_value == "import"
                        || id.string_value == "in"
                        || id.string_value == "instanceof"
                        || id.string_value == "new"
                        || id.string_value == "null"
                        || id.string_value == "return"
                        || id.string_value == "super"
                        || id.string_value == "switch"
                        || id.string_value == "this"
                        || id.string_value == "throw"
                        || id.string_value == "true"
                        || id.string_value == "try"
                        || id.string_value == "typeof"
                        || id.string_value == "var"
                        || id.string_value == "void"
                        || id.string_value == "while"
                        || id.string_value == "with"
                    {
                        Err(format!(
                            "{}:{}: ‘{}’ is a reserved word and may not be used as an identifier",
                            id.line, id.column, id.string_value
                        ))
                    } else {
                        let node = Identifier::IdentifierName(id);
                        let boxed = Box::new(node);
                        Ok(Some((boxed, after_tok)))
                    }
                }
            },
            _ => Ok(None),
        }
    }
}

// IdentifierReference[Yield, Await]:
//      Identifier
//      [~Yield]yield
//      [~Await]await

#[derive(Debug)]
enum IdentifierReferenceKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct IdentifierReference {
    kind: IdentifierReferenceKind,
    strict: bool,
}

impl StringValue for IdentifierReference {
    fn string_value(&self) -> scanner::JSString {
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from("yield"),
            Await => scanner::JSString::from("await"),
        }
    }
}

impl AssignmentTargetType for IdentifierReference {
    fn assignment_target_type(&self) -> ATTKind {
        use ATTKind::*;
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => {
                if self.strict {
                    let sv = id.string_value();
                    if sv == "eval" || sv == "arguments" {
                        Invalid
                    } else {
                        Simple
                    }
                } else {
                    Simple
                }
            }
            Await | Yield => Simple,
        }
    }
}

impl PrettyPrint for IdentifierReference {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IdentifierReference: {}", first, self)?;
        if let IdentifierReferenceKind::Identifier(boxed) = &self.kind {
            boxed.pprint_with_leftpad(writer, &successive, Spot::Final)?;
        }
        Ok(())
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |tok: &str| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}IdentifierReference: {}", first, self)?;
            pprint_token(writer, tok, &successive, Spot::Final)
        };
        match &self.kind {
            IdentifierReferenceKind::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            IdentifierReferenceKind::Await => work("await"),
            IdentifierReferenceKind::Yield => work("yield"),
        }
    }
}

impl fmt::Display for IdentifierReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IdentifierReferenceKind::Identifier(boxed) => write!(f, "{}", *boxed),
            IdentifierReferenceKind::Yield => write!(f, "yield"),
            IdentifierReferenceKind::Await => write!(f, "await"),
        }
    }
}

impl IdentifierReference {
    pub fn parse(
        parser: &mut Parser,
        initial_scanner: Scanner,
        arg_yield: bool,
        arg_await: bool,
    ) -> Result<Option<(Box<IdentifierReference>, Scanner)>, String> {
        let production = Identifier::parse(parser, initial_scanner)?;
        match production {
            Some((ident, scanner)) => {
                let node = IdentifierReference {
                    kind: IdentifierReferenceKind::Identifier(ident),
                    strict: parser.strict,
                };
                let boxed = Box::new(node);
                Ok(Some((boxed, scanner)))
            }
            None => {
                let (token, scan) =
                    scanner::scan_token(&initial_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                match token {
                    scanner::Token::Identifier(id) if !arg_await && id.keyword_id == Some(scanner::Keyword::Await) => {
                        Ok(Some((
                            Box::new(IdentifierReference {
                                kind: IdentifierReferenceKind::Await,
                                strict: parser.strict,
                            }),
                            scan,
                        )))
                    }
                    scanner::Token::Identifier(id) if !arg_yield && id.keyword_id == Some(scanner::Keyword::Yield) => {
                        Ok(Some((
                            Box::new(IdentifierReference {
                                kind: IdentifierReferenceKind::Yield,
                                strict: parser.strict,
                            }),
                            scan,
                        )))
                    }
                    _ => Ok(None),
                }
            }
        }
    }
}

// BindingIdentifier[Yield, Await] :
//    Identifier
//    yield
//    await
#[derive(Debug)]
enum BindingIdentifierKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct BindingIdentifier {
    kind: BindingIdentifierKind,
    yield_flag: bool,
    await_flag: bool,
}

impl StringValue for BindingIdentifier {
    fn string_value(&self) -> scanner::JSString {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from("yield"),
            Await => scanner::JSString::from("await"),
        }
    }
}

impl BoundNames for BindingIdentifier {
    fn bound_names(&self) -> Vec<scanner::JSString> {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => vec![id.string_value()],
            Yield => vec![scanner::JSString::from("yield")],
            Await => vec![scanner::JSString::from("await")],
        }
    }
}

impl fmt::Display for BindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            BindingIdentifierKind::Await => write!(f, "await"),
            BindingIdentifierKind::Identifier(boxed) => write!(f, "{}", boxed),
            BindingIdentifierKind::Yield => write!(f, "yield"),
        }
    }
}

impl PrettyPrint for BindingIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingIdentifier: {}", first, self)?;
        if let BindingIdentifierKind::Identifier(boxed) = &self.kind {
            boxed.pprint_with_leftpad(writer, &successive, Spot::Final)?;
        }
        Ok(())
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |tok: &str| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}BindingIdentifier: {}", first, self)?;
            pprint_token(writer, tok, &successive, Spot::Final)
        };
        match &self.kind {
            BindingIdentifierKind::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            BindingIdentifierKind::Await => work("await"),
            BindingIdentifierKind::Yield => work("yield"),
        }
    }
}

impl BindingIdentifier {
    pub fn parse(
        parser: &mut Parser,
        starting_scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let production = Identifier::parse(parser, starting_scanner)?;
        match production {
            Some((ident, scanner)) => {
                let node = BindingIdentifier {
                    kind: BindingIdentifierKind::Identifier(ident),
                    yield_flag,
                    await_flag,
                };
                let boxed = Box::new(node);
                Ok(Some((boxed, scanner)))
            }
            None => {
                let (token, scan) =
                    scanner::scan_token(&starting_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                match token {
                    scanner::Token::Identifier(id) => match id.keyword_id {
                        Some(scanner::Keyword::Await) => Ok(Some((
                            Box::new(BindingIdentifier {
                                kind: BindingIdentifierKind::Await,
                                yield_flag,
                                await_flag,
                            }),
                            scan,
                        ))),
                        Some(scanner::Keyword::Yield) => Ok(Some((
                            Box::new(BindingIdentifier {
                                kind: BindingIdentifierKind::Yield,
                                yield_flag,
                                await_flag,
                            }),
                            scan,
                        ))),
                        _ => Ok(None),
                    },
                    _ => Ok(None),
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct IdentifierNameToken {
    pub value: scanner::Token,
}
impl fmt::Display for IdentifierNameToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let scanner::Token::Identifier(id) = &self.value {
            write!(f, "{}", id.string_value)
        } else {
            unreachable!()
        }
    }
}

// LabelIdentifier[Yield, Await] :
//      Identifier
//      [~Yield]yield
//      [~Await]await
#[derive(Debug)]
pub enum LabelIdentifier {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

impl fmt::Display for LabelIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelIdentifier::Identifier(node) => node.fmt(f),
            LabelIdentifier::Yield => write!(f, "yield"),
            LabelIdentifier::Await => write!(f, "await"),
        }
    }
}

impl PrettyPrint for LabelIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelIdentifier: {}", first, self)?;
        match self {
            LabelIdentifier::Identifier(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LabelIdentifier::Yield | LabelIdentifier::Await => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LabelIdentifier::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            LabelIdentifier::Yield => pprint_token(writer, "yield", pad, state),
            LabelIdentifier::Await => pprint_token(writer, "await", pad, state),
        }
    }
}

impl LabelIdentifier {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_id = Identifier::parse(parser, scanner)?;
        if let Some((id, after_id)) = pot_id {
            return Ok(Some((Box::new(LabelIdentifier::Identifier(id)), after_id)));
        }
        if !yield_flag || !await_flag {
            let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            if !yield_flag
                && matches!(&tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Yield))
            {
                return Ok(Some((Box::new(LabelIdentifier::Yield), after_tok)));
            }
            if !await_flag
                && matches!(&tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Await))
            {
                return Ok(Some((Box::new(LabelIdentifier::Await), after_tok)));
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;
    fn id_kwd_test(kwd: &str) {
        let result = Identifier::parse(
            &mut super::Parser::new(kwd, false, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    #[test]
    fn identifier_test_pprint() {
        let pot_id = Identifier::parse(&mut Parser::new("phil", false, ParseGoal::Script), Scanner::new());
        let (id, _) = pot_id.unwrap().unwrap();
        pretty_check(&*id, "Identifier: phil", vec![]);
    }
    #[test]
    fn identifier_test_await() {
        id_kwd_test("await")
    }
    #[test]
    fn identifier_test_break() {
        id_kwd_test("break")
    }
    #[test]
    fn identifier_test_case() {
        id_kwd_test("case")
    }
    #[test]
    fn identifier_test_catch() {
        id_kwd_test("catch")
    }
    #[test]
    fn identifier_test_class() {
        id_kwd_test("class")
    }
    #[test]
    fn identifier_test_const() {
        id_kwd_test("const")
    }
    #[test]
    fn identifier_test_continue() {
        id_kwd_test("continue")
    }
    #[test]
    fn identifier_test_debugger() {
        id_kwd_test("debugger")
    }
    #[test]
    fn identifier_test_default() {
        id_kwd_test("default")
    }
    #[test]
    fn identifier_test_delete() {
        id_kwd_test("delete")
    }
    #[test]
    fn identifier_test_do() {
        id_kwd_test("do")
    }
    #[test]
    fn identifier_test_else() {
        id_kwd_test("else")
    }
    #[test]
    fn identifier_test_enum() {
        id_kwd_test("enum")
    }
    #[test]
    fn identifier_test_export() {
        id_kwd_test("export")
    }
    #[test]
    fn identifier_test_extends() {
        id_kwd_test("extends")
    }
    #[test]
    fn identifier_test_false() {
        id_kwd_test("false")
    }
    #[test]
    fn identifier_test_finally() {
        id_kwd_test("finally")
    }
    #[test]
    fn identifier_test_for() {
        id_kwd_test("for")
    }
    #[test]
    fn identifier_test_function() {
        id_kwd_test("function")
    }
    #[test]
    fn identifier_test_if() {
        id_kwd_test("if")
    }
    #[test]
    fn identifier_test_import() {
        id_kwd_test("import")
    }
    #[test]
    fn identifier_test_in() {
        id_kwd_test("in")
    }
    #[test]
    fn identifier_test_instanceof() {
        id_kwd_test("instanceof")
    }
    #[test]
    fn identifier_test_new() {
        id_kwd_test("new")
    }
    #[test]
    fn identifier_test_null() {
        id_kwd_test("null")
    }
    #[test]
    fn identifier_test_return() {
        id_kwd_test("return")
    }
    #[test]
    fn identifier_test_super() {
        id_kwd_test("super")
    }
    #[test]
    fn identifier_test_switch() {
        id_kwd_test("switch")
    }
    #[test]
    fn identifier_test_this() {
        id_kwd_test("this")
    }
    #[test]
    fn identifier_test_throw() {
        id_kwd_test("throw")
    }
    #[test]
    fn identifier_test_true() {
        id_kwd_test("true")
    }
    #[test]
    fn identifier_test_try() {
        id_kwd_test("try")
    }
    #[test]
    fn identifier_test_typeof() {
        id_kwd_test("typeof")
    }
    #[test]
    fn identifier_test_var() {
        id_kwd_test("var")
    }
    #[test]
    fn identifier_test_void() {
        id_kwd_test("void")
    }
    #[test]
    fn identifier_test_while() {
        id_kwd_test("while")
    }
    #[test]
    fn identifier_test_with() {
        id_kwd_test("with")
    }
    #[test]
    fn identifier_test_yield() {
        id_kwd_test("yield")
    }
    #[test]
    fn identifier_test_err() {
        let result = Identifier::parse(
            &mut super::Parser::new("iden\\u{20}tifier", false, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    fn identifier_test_strict(kwd: &str) {
        let result = Identifier::parse(
            &mut super::Parser::new(kwd, true, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            format!("1:1: ‘{}’ not allowed as an identifier in strict mode", kwd)
        );
    }
    #[test]
    fn identifier_test_strict_implements() {
        identifier_test_strict("implements")
    }
    #[test]
    fn identifier_test_strict_interface() {
        identifier_test_strict("interface")
    }
    #[test]
    fn identifier_test_strict_let() {
        identifier_test_strict("let")
    }
    #[test]
    fn identifier_test_strict_package() {
        identifier_test_strict("package")
    }
    #[test]
    fn identifier_test_strict_private() {
        identifier_test_strict("private")
    }
    #[test]
    fn identifier_test_strict_protected() {
        identifier_test_strict("protected")
    }
    #[test]
    fn identifier_test_strict_public() {
        identifier_test_strict("public")
    }
    #[test]
    fn identifier_test_strict_static() {
        identifier_test_strict("static")
    }
    #[test]
    fn identifier_test_await_module() {
        let result = Identifier::parse(
            &mut Parser::new("aw\\u0061it", false, ParseGoal::Module),
            Scanner::new(),
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "1:1: ‘await’ not allowed as an identifier in modules"
        );
    }
    #[test]
    fn identifier_test_nothing() {
        let result = Identifier::parse(&mut Parser::new(".", false, ParseGoal::Script), Scanner::new());
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    fn identifier_test_keyword(kwd: &str) {
        let firstch = kwd.chars().next().unwrap();
        let id_src = format!("\\u{{{:x}}}{}", firstch as u32, &kwd[firstch.len_utf8()..]);
        let result = Identifier::parse(
            &mut super::Parser::new(&id_src, false, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            format!("1:1: ‘{}’ is a reserved word and may not be used as an identifier", kwd)
        );
    }
    #[test]
    fn identifier_test_keyword_break() {
        identifier_test_keyword("break")
    }
    #[test]
    fn identifier_test_keyword_case() {
        identifier_test_keyword("case")
    }
    #[test]
    fn identifier_test_keyword_catch() {
        identifier_test_keyword("catch")
    }
    #[test]
    fn identifier_test_keyword_class() {
        identifier_test_keyword("class")
    }
    #[test]
    fn identifier_test_keyword_const() {
        identifier_test_keyword("const")
    }
    #[test]
    fn identifier_test_keyword_continue() {
        identifier_test_keyword("continue")
    }
    #[test]
    fn identifier_test_keyword_debugger() {
        identifier_test_keyword("debugger")
    }
    #[test]
    fn identifier_test_keyword_default() {
        identifier_test_keyword("default")
    }
    #[test]
    fn identifier_test_keyword_delete() {
        identifier_test_keyword("delete")
    }
    #[test]
    fn identifier_test_keyword_do() {
        identifier_test_keyword("do")
    }
    #[test]
    fn identifier_test_keyword_else() {
        identifier_test_keyword("else")
    }
    #[test]
    fn identifier_test_keyword_enum() {
        identifier_test_keyword("enum")
    }
    #[test]
    fn identifier_test_keyword_export() {
        identifier_test_keyword("export")
    }
    #[test]
    fn identifier_test_keyword_extends() {
        identifier_test_keyword("extends")
    }
    #[test]
    fn identifier_test_keyword_false() {
        identifier_test_keyword("false")
    }
    #[test]
    fn identifier_test_keyword_finally() {
        identifier_test_keyword("finally")
    }
    #[test]
    fn identifier_test_keyword_for() {
        identifier_test_keyword("for")
    }
    #[test]
    fn identifier_test_keyword_function() {
        identifier_test_keyword("function")
    }
    #[test]
    fn identifier_test_keyword_if() {
        identifier_test_keyword("if")
    }
    #[test]
    fn identifier_test_keyword_import() {
        identifier_test_keyword("import")
    }
    #[test]
    fn identifier_test_keyword_in() {
        identifier_test_keyword("in")
    }
    #[test]
    fn identifier_test_keyword_instanceof() {
        identifier_test_keyword("instanceof")
    }
    #[test]
    fn identifier_test_keyword_new() {
        identifier_test_keyword("new")
    }
    #[test]
    fn identifier_test_keyword_null() {
        identifier_test_keyword("null")
    }
    #[test]
    fn identifier_test_keyword_return() {
        identifier_test_keyword("return")
    }
    #[test]
    fn identifier_test_keyword_super() {
        identifier_test_keyword("super")
    }
    #[test]
    fn identifier_test_keyword_switch() {
        identifier_test_keyword("switch")
    }
    #[test]
    fn identifier_test_keyword_this() {
        identifier_test_keyword("this")
    }
    #[test]
    fn identifier_test_keyword_throw() {
        identifier_test_keyword("throw")
    }
    #[test]
    fn identifier_test_keyword_true() {
        identifier_test_keyword("true")
    }
    #[test]
    fn identifier_test_keyword_try() {
        identifier_test_keyword("try")
    }
    #[test]
    fn identifier_test_keyword_typeof() {
        identifier_test_keyword("typeof")
    }
    #[test]
    fn identifier_test_keyword_var() {
        identifier_test_keyword("var")
    }
    #[test]
    fn identifier_test_keyword_void() {
        identifier_test_keyword("void")
    }
    #[test]
    fn identifier_test_keyword_while() {
        identifier_test_keyword("while")
    }
    #[test]
    fn identifier_test_keyword_with() {
        identifier_test_keyword("with")
    }
    #[test]
    fn identifier_test_successful_bob() {
        let result = Identifier::parse(
            &mut super::Parser::new("bob", true, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(
            scanner,
            super::Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            }
        );
        let Identifier::IdentifierName(data) = *identifier;
        assert_eq!(data.string_value, "bob");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }
    #[test]
    fn identifier_test_successful_japanese() {
        let text = "手がける黒田征太郎さんです";
        let result = Identifier::parse(
            &mut super::Parser::new(text, true, super::ParseGoal::Script),
            Scanner::new(),
        );
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(
            scanner,
            super::Scanner {
                line: 1,
                column: 14,
                start_idx: 39
            }
        );
        let Identifier::IdentifierName(data) = *identifier;
        assert_eq!(data.string_value, "手がける黒田征太郎さんです");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }

    #[test]
    fn identifier_reference_test_debug() {
        assert_eq!(
            format!(
                "{:?}",
                IdentifierReference {
                    kind: IdentifierReferenceKind::Yield,
                    strict: false
                }
            ),
            "IdentifierReference { kind: Yield, strict: false }"
        );
    }
    fn idref_create(text: &str, strict: bool) -> Box<IdentifierReference> {
        let yield_syntax = false;
        let await_syntax = false;
        let result = IdentifierReference::parse(
            &mut Parser::new(text, strict, ParseGoal::Script),
            Scanner::new(),
            yield_syntax,
            await_syntax,
        );
        assert!(result.is_ok());
        let optional_idref = result.unwrap();
        assert!(optional_idref.is_some());
        let (idref, scanner) = optional_idref.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len()
            }
        );
        idref
    }

    #[test]
    fn identifier_reference_test_simple_success() {
        let idref = idref_create("identifier", false);
        assert_eq!(idref.strict, false);
        use IdentifierReferenceKind::*;
        match &idref.kind {
            Yield | Await => assert!(false, "Wrong IdentifierReference Kind (expected Identifier)"),
            Identifier(_) => (),
        }

        assert_eq!(idref.string_value(), "identifier");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
        pretty_check(
            &*idref,
            "IdentifierReference: identifier",
            vec!["Identifier: identifier"],
        );
    }
    #[test]
    fn identifier_reference_test_yield() {
        let idref = idref_create("yield", false);
        assert_eq!(idref.strict, false);
        assert!(matches!(idref.kind, IdentifierReferenceKind::Yield));
        assert_eq!(idref.string_value(), "yield");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
        pretty_check(&*idref, "IdentifierReference: yield", vec![]);
    }
    #[test]
    fn identifier_reference_test_yield_02() {
        let idref = IdentifierReference::parse(
            &mut Parser::new("yield", false, ParseGoal::Script),
            Scanner::new(),
            true,
            true,
        );
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_await() {
        let idref = idref_create("await", false);
        assert_eq!(idref.strict, false);
        assert!(matches!(idref.kind, IdentifierReferenceKind::Await));
        assert_eq!(idref.string_value(), "await");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
        pretty_check(&*idref, "IdentifierReference: await", vec![]);
    }
    #[test]
    fn identifier_reference_test_await_02() {
        let idref = IdentifierReference::parse(
            &mut Parser::new("await", false, ParseGoal::Script),
            Scanner::new(),
            true,
            true,
        );
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_kwd() {
        let idref = IdentifierReference::parse(
            &mut Parser::new("new", false, ParseGoal::Script),
            Scanner::new(),
            true,
            true,
        );
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_punct() {
        let idref = IdentifierReference::parse(
            &mut Parser::new("**", false, ParseGoal::Script),
            Scanner::new(),
            true,
            true,
        );
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_att_strict() {
        let idref = idref_create("abcd", true);
        assert_eq!(idref.string_value(), "abcd");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn identifier_reference_test_eval_strict() {
        let idref = idref_create("eval", true);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn identifier_reference_test_eval_loose() {
        let idref = idref_create("eval", false);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn identifier_reference_test_arguments_strict() {
        let idref = idref_create("arguments", true);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn identifier_reference_test_arguments_loose() {
        let idref = idref_create("arguments", false);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }

    fn bindingid_create(text: &str, y: bool, a: bool) -> Box<BindingIdentifier> {
        let yield_syntax = y;
        let await_syntax = a;
        let strict = false;
        let result = BindingIdentifier::parse(
            &mut Parser::new(text, strict, ParseGoal::Script),
            Scanner::new(),
            yield_syntax,
            await_syntax,
        );
        assert!(result.is_ok());
        let optional_bid = result.unwrap();
        assert!(optional_bid.is_some());
        let (bid, scanner) = optional_bid.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len()
            }
        );
        bid
    }

    fn bid_allflags(text: &str) {
        for yflag in [false, true].iter() {
            for aflag in [false, true].iter() {
                let bid = bindingid_create(text, *yflag, *aflag);
                assert_eq!(bid.string_value(), text);
                assert_eq!(bid.bound_names(), [text]);
                assert_eq!(bid.yield_flag, *yflag);
                assert_eq!(bid.await_flag, *aflag);
            }
        }
    }

    #[test]
    fn binding_identifier_test_normal() {
        bid_allflags("green");
    }
    #[test]
    fn binding_identifier_test_yield() {
        bid_allflags("yield");
    }
    #[test]
    fn binding_identifier_test_await() {
        bid_allflags("await");
    }
    #[test]
    fn binding_identifier_test_pprint() {
        let b1 = bindingid_create("joe", false, false);
        pretty_check(&*b1, "BindingIdentifier: joe", vec!["Identifier: joe"]);
        let b2 = bindingid_create("yield", false, false);
        pretty_check(&*b2, "BindingIdentifier: yield", vec![]);
        let b3 = bindingid_create("await", false, false);
        pretty_check(&*b3, "BindingIdentifier: await", vec![]);
    }
    #[test]
    fn binding_identifier_test_debug() {
        format!("{:?}", bindingid_create("abcd", true, true));
    }
    #[test]
    fn binding_identifier_test_non_matches() {
        let mut p1 = Parser::new("function", false, ParseGoal::Script);
        let r1 = BindingIdentifier::parse(&mut p1, Scanner::new(), false, false);
        assert!(r1.is_ok());
        assert!(r1.unwrap().is_none());
        let mut p2 = Parser::new("**", false, ParseGoal::Script);
        let r2 = BindingIdentifier::parse(&mut p2, Scanner::new(), false, false);
        assert!(r2.is_ok());
        assert!(r2.unwrap().is_none());
    }
}

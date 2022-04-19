use super::scanner::{scan_token, IdentifierData, Keyword, ScanGoal, Scanner, Token};
use super::*;
use crate::chunk::Chunk;
use crate::opcodes::Insn;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use crate::strings::JSString;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub struct Identifier {
    name: IdentifierData,
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
        pprint_token(writer, self, TokenType::IdentifierName, pad, state)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_value())
    }
}

impl Identifier {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Identifier(id) => match id.keyword_id {
                Some(Keyword::Await)
                | Some(Keyword::Break)
                | Some(Keyword::Case)
                | Some(Keyword::Catch)
                | Some(Keyword::Class)
                | Some(Keyword::Const)
                | Some(Keyword::Continue)
                | Some(Keyword::Debugger)
                | Some(Keyword::Default)
                | Some(Keyword::Delete)
                | Some(Keyword::Do)
                | Some(Keyword::Else)
                | Some(Keyword::Enum)
                | Some(Keyword::Export)
                | Some(Keyword::Extends)
                | Some(Keyword::False)
                | Some(Keyword::Finally)
                | Some(Keyword::For)
                | Some(Keyword::Function)
                | Some(Keyword::If)
                | Some(Keyword::Import)
                | Some(Keyword::In)
                | Some(Keyword::Instanceof)
                | Some(Keyword::New)
                | Some(Keyword::Null)
                | Some(Keyword::Return)
                | Some(Keyword::Super)
                | Some(Keyword::Switch)
                | Some(Keyword::This)
                | Some(Keyword::Throw)
                | Some(Keyword::True)
                | Some(Keyword::Try)
                | Some(Keyword::Typeof)
                | Some(Keyword::Var)
                | Some(Keyword::Void)
                | Some(Keyword::While)
                | Some(Keyword::With)
                | Some(Keyword::Yield) => Err(ParseError::new(PECode::KeywordUsedAsIdentifier(id.keyword_id.unwrap()), scanner)),
                _ => Ok((Rc::new(Identifier { name: id }), after_tok)),
            },
            _ => Err(ParseError::new(PECode::InvalidIdentifier, scanner)),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match parser.identifier_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.identifier_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, in_module: bool) {
        // Static Semantics: Early Errors
        //      Identifier : IdentifierName but not ReservedWord
        //  * It is a Syntax Error if this phrase is contained in strict mode code and the StringValue of IdentifierName
        //    is: "implements", "interface", "let", "package", "private", "protected", "public", "static", or "yield".
        //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module and the StringValue of
        //    IdentifierName is "await".
        //  * It is a Syntax Error if StringValue of IdentifierName is the same String value as the StringValue of any
        //    ReservedWord except for yield or await.
        let id = &self.name;

        if strict
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
            errs.push(create_syntax_error_object(agent, format!("‘{}’ not allowed as an identifier in strict mode", id.string_value).as_str()));
        }
        if in_module && id.string_value == "await" {
            errs.push(create_syntax_error_object(agent, "‘await’ not allowed as an identifier in modules"));
        }
        if id.string_value == "break"
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
            errs.push(create_syntax_error_object(agent, format!("‘{}’ is a reserved word and may not be used as an identifier", id.string_value).as_str()));
        }
    }

    pub fn string_value(&self) -> JSString {
        let identifier_name = &self.name;
        identifier_name.string_value.clone()
    }
}

// IdentifierReference[Yield, Await]:
//      Identifier
//      [~Yield]yield
//      [~Await]await

#[derive(Debug)]
enum IdentifierReferenceKind {
    Identifier(Rc<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct IdentifierReference {
    kind: IdentifierReferenceKind,
    yield_flag: bool,
    await_flag: bool,
    in_module: bool,
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
        let mut work = |tok: &str| pprint_token(writer, tok, TokenType::Keyword, pad, state);
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
    fn parse_core(parser: &mut Parser, initial_scanner: Scanner, arg_yield: bool, arg_await: bool) -> ParseResult<Self> {
        let production = Identifier::parse(parser, initial_scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match production {
            Ok((ident, scanner)) => {
                let node = IdentifierReference { kind: IdentifierReferenceKind::Identifier(ident), in_module, yield_flag: arg_yield, await_flag: arg_await };
                let boxed = Rc::new(node);
                Ok((boxed, scanner))
            }
            Err(pe) => {
                let (token, scan) = scan_token(&initial_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if !arg_await && id.matches(Keyword::Await) => {
                        Ok((Rc::new(IdentifierReference { kind: IdentifierReferenceKind::Await, in_module, yield_flag: arg_yield, await_flag: arg_await }), scan))
                    }
                    Token::Identifier(id) if !arg_yield && id.matches(Keyword::Yield) => {
                        Ok((Rc::new(IdentifierReference { kind: IdentifierReferenceKind::Yield, in_module, yield_flag: arg_yield, await_flag: arg_await }), scan))
                    }
                    _ => Err(pe),
                }
            }
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.identifier_reference_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.identifier_reference_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            IdentifierReferenceKind::Identifier(boxed) => boxed.contains(kind),
            IdentifierReferenceKind::Yield => false,
            IdentifierReferenceKind::Await => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match &self.kind {
            IdentifierReferenceKind::Identifier(id) => {
                // IdentifierReference : Identifier
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if self.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(agent, "identifier 'yield' not allowed when yield expressions are valid"));
                }
                if self.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(agent, "identifier 'await' not allowed when await expressions are valid"));
                }
                id.early_errors(agent, errs, strict, self.in_module);
            }
            IdentifierReferenceKind::Yield => {
                // IdentifierReference : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                if strict {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in strict mode: yield"));
                }
            }
            IdentifierReferenceKind::Await => {
                // IdentifierReference : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                if self.in_module {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in modules: await"));
                }
            }
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  IdentifierReference : Identifier
        //      1. If the StringValue of Identifier is "arguments", return true.
        //      2. Return false.
        //  IdentifierReference : yield
        //  IdentifierReference : await
        //      1. Return false.
        matches!(&self.kind, IdentifierReferenceKind::Identifier(id) if id.string_value() == "arguments")
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        use ATTKind::*;
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => {
                if strict {
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

    pub fn string_value(&self) -> JSString {
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => JSString::from("yield"),
            Await => JSString::from("await"),
        }
    }

    /// Generate the code for IdentifierReference
    ///
    /// See [IdentifierReference Evaluation](https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        // Runtime Semantics: Evaluation
        //  IdentifierReference : Identifier
        //      1. Return ? ResolveBinding(StringValue of Identifier).
        //  IdentifierReference : yield
        //      1. Return ? ResolveBinding("yield").
        //  IdentifierReference : await
        //      1. Return ? ResolveBinding("await").
        //
        // NOTE 1   | The result of evaluating an IdentifierReference is always a value of type Reference.
        // NOTE 2   | In non-strict code, the keyword yield may be used as an identifier. Evaluating the
        //          | IdentifierReference resolves the binding of yield as if it was an Identifier. Early Error
        //          | restriction ensures that such an evaluation only can occur for non-strict code.

        // Add the identifier string to this chunk's string pool.
        let string_id = chunk.add_to_string_pool(match &self.kind {
            IdentifierReferenceKind::Identifier(id) => id.string_value(),
            IdentifierReferenceKind::Yield => "yield".into(),
            IdentifierReferenceKind::Await => "await".into(),
        })?;
        chunk.op_plus_arg(Insn::PushStr, string_id);
        chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
        Ok(())
    }
}

// BindingIdentifier[Yield, Await] :
//    Identifier
//    yield
//    await
#[derive(Debug)]
enum BindingIdentifierKind {
    Identifier(Rc<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct BindingIdentifier {
    kind: BindingIdentifierKind,
    yield_flag: bool,
    await_flag: bool,
    in_module: bool,
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
        match &self.kind {
            BindingIdentifierKind::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            BindingIdentifierKind::Await => pprint_token(writer, "await", TokenType::Keyword, pad, state),
            BindingIdentifierKind::Yield => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
        }
    }
}

impl BindingIdentifier {
    fn parse_core(parser: &mut Parser, starting_scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let production = Identifier::parse(parser, starting_scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match production {
            Ok((ident, scanner)) => {
                let node = BindingIdentifier { kind: BindingIdentifierKind::Identifier(ident), yield_flag, await_flag, in_module };
                let boxed = Rc::new(node);
                Ok((boxed, scanner))
            }
            Err(pe) => {
                let (token, scan) = scan_token(&starting_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if id.matches(Keyword::Await) => Ok((Rc::new(BindingIdentifier { kind: BindingIdentifierKind::Await, yield_flag, await_flag, in_module }), scan)),
                    Token::Identifier(id) if id.matches(Keyword::Yield) => Ok((Rc::new(BindingIdentifier { kind: BindingIdentifierKind::Yield, yield_flag, await_flag, in_module }), scan)),
                    _ => Err(pe),
                }
            }
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.binding_identifier_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.binding_identifier_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            BindingIdentifierKind::Yield => false,
            BindingIdentifierKind::Await => false,
            BindingIdentifierKind::Identifier(id) => id.contains(kind),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => vec![id.string_value()],
            Yield => vec![JSString::from("yield")],
            Await => vec![JSString::from("await")],
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match &self.kind {
            BindingIdentifierKind::Identifier(id) => {
                // BindingIdentifier : Identifier
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code and
                //    the StringValue of Identifier is "arguments" or "eval".
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if strict && [JSString::from("arguments"), JSString::from("eval")].contains(&sv) {
                    errs.push(create_syntax_error_object(agent, format!("identifier not allowed in strict mode: {}", sv).as_str()));
                }
                if self.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(agent, "identifier 'yield' not allowed when yield expressions are valid"));
                }
                if self.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(agent, "identifier 'await' not allowed when await expressions are valid"));
                }
                id.early_errors(agent, errs, strict, self.in_module);
            }
            BindingIdentifierKind::Yield => {
                // BindingIdentifier : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                //  * It is a Syntax Error if this production has a [Yield] parameter.
                if strict {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in strict mode: yield"));
                }
                if self.yield_flag {
                    errs.push(create_syntax_error_object(agent, "identifier 'yield' not allowed when yield expressions are valid"));
                }
            }
            BindingIdentifierKind::Await => {
                // BindingIdentifier : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                //  * It is a Syntax Error if this production has an [Await] parameter.
                if self.in_module {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in modules: await"));
                }
                if self.await_flag {
                    errs.push(create_syntax_error_object(agent, "identifier 'await' not allowed when await expressions are valid"));
                }
            }
        }
    }

    pub fn string_value(&self) -> JSString {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => JSString::from("yield"),
            Await => JSString::from("await"),
        }
    }
}

// LabelIdentifier[Yield, Await] :
//      Identifier
//      [~Yield]yield
//      [~Await]await
#[derive(Debug)]
pub struct LabelIdentifier {
    kind: LabelIdentifierKind,
    in_module: bool,
    yield_flag: bool,
    await_flag: bool,
}
#[derive(Debug)]
pub enum LabelIdentifierKind {
    Identifier(Rc<Identifier>),
    Yield,
    Await,
}

impl fmt::Display for LabelIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            LabelIdentifierKind::Identifier(id) => id.fmt(f),
            LabelIdentifierKind::Yield => write!(f, "yield"),
            LabelIdentifierKind::Await => write!(f, "await"),
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
        match &self.kind {
            LabelIdentifierKind::Identifier(id) => id.pprint_with_leftpad(writer, &successive, Spot::Final),
            LabelIdentifierKind::Yield | LabelIdentifierKind::Await => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.kind {
            LabelIdentifierKind::Identifier(id) => id.concise_with_leftpad(writer, pad, state),
            LabelIdentifierKind::Yield => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            LabelIdentifierKind::Await => pprint_token(writer, "await", TokenType::Keyword, pad, state),
        }
    }
}

impl LabelIdentifier {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let pot_id = Identifier::parse(parser, scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match pot_id {
            Ok((id, after_id)) => Ok((Rc::new(LabelIdentifier { kind: LabelIdentifierKind::Identifier(id), in_module, yield_flag, await_flag }), after_id)),
            Err(pe) => {
                if !yield_flag || !await_flag {
                    let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
                    if !yield_flag && tok.matches_keyword(Keyword::Yield) {
                        Ok((Rc::new(LabelIdentifier { kind: LabelIdentifierKind::Yield, in_module, yield_flag, await_flag }), after_tok))
                    } else if !await_flag && tok.matches_keyword(Keyword::Await) {
                        Ok((Rc::new(LabelIdentifier { kind: LabelIdentifierKind::Await, in_module, yield_flag, await_flag }), after_tok))
                    } else {
                        Err(pe)
                    }
                } else {
                    Err(pe)
                }
            }
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.label_identifier_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.label_identifier_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn string_value(&self) -> JSString {
        match &self.kind {
            LabelIdentifierKind::Await => JSString::from("await"),
            LabelIdentifierKind::Yield => JSString::from("yield"),
            LabelIdentifierKind::Identifier(id) => id.string_value(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            LabelIdentifierKind::Identifier(node) => node.contains(kind),
            LabelIdentifierKind::Yield => false,
            LabelIdentifierKind::Await => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match &self.kind {
            LabelIdentifierKind::Identifier(id) => {
                // LabelIdentifier : Identifier
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if self.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(agent, "identifier 'yield' not allowed when yield expressions are valid"));
                }
                if self.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(agent, "identifier 'await' not allowed when await expressions are valid"));
                }
                id.early_errors(agent, errs, strict, self.in_module);
            }
            LabelIdentifierKind::Yield => {
                // LabelIdentifier : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                if strict {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in strict mode: yield"));
                }
            }
            LabelIdentifierKind::Await => {
                // LabelIdentifier : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                if self.in_module {
                    errs.push(create_syntax_error_object(agent, "identifier not allowed in modules: await"));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;

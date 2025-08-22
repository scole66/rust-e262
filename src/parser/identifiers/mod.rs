use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub struct Identifier {
    name: IdentifierData,
    location: Location,
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
        let (tok, tok_loc, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Identifier(id) => match id.keyword_id {
                Some(
                    Keyword::Await
                    | Keyword::Break
                    | Keyword::Case
                    | Keyword::Catch
                    | Keyword::Class
                    | Keyword::Const
                    | Keyword::Continue
                    | Keyword::Debugger
                    | Keyword::Default
                    | Keyword::Delete
                    | Keyword::Do
                    | Keyword::Else
                    | Keyword::Enum
                    | Keyword::Export
                    | Keyword::Extends
                    | Keyword::False
                    | Keyword::Finally
                    | Keyword::For
                    | Keyword::Function
                    | Keyword::If
                    | Keyword::Import
                    | Keyword::In
                    | Keyword::Instanceof
                    | Keyword::New
                    | Keyword::Null
                    | Keyword::Return
                    | Keyword::Super
                    | Keyword::Switch
                    | Keyword::This
                    | Keyword::Throw
                    | Keyword::True
                    | Keyword::Try
                    | Keyword::Typeof
                    | Keyword::Var
                    | Keyword::Void
                    | Keyword::While
                    | Keyword::With
                    | Keyword::Yield,
                ) => Err(ParseError::new(PECode::KeywordUsedAsIdentifier(id.keyword_id.unwrap()), tok_loc)),
                _ => Ok((Rc::new(Identifier { name: id, location: tok_loc }), after_tok)),
            },
            _ => Err(ParseError::new(PECode::InvalidIdentifier, tok_loc)),
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

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::IdentifierName
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, in_module: bool) {
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
            errs.push(create_syntax_error_object(
                format!("‘{}’ not allowed as an identifier in strict mode", id.string_value).as_str(),
                Some(self.location),
            ));
        }
        if in_module && id.string_value == "await" {
            errs.push(create_syntax_error_object(
                "‘await’ not allowed as an identifier in modules",
                Some(self.location),
            ));
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
            errs.push(create_syntax_error_object(
                format!("‘{}’ is a reserved word and may not be used as an identifier", id.string_value).as_str(),
                Some(self.location),
            ));
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
pub struct IdRefData {
    yield_flag: bool,
    await_flag: bool,
    in_module: bool,
    location: Location,
}

#[derive(Debug)]
pub enum IdentifierReference {
    Identifier { identifier: Rc<Identifier>, data: IdRefData },
    Yield { data: IdRefData },
    Await { data: IdRefData },
}

impl PrettyPrint for IdentifierReference {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}IdentifierReference: {self}")?;
        if let IdentifierReference::Identifier { identifier, .. } = self {
            identifier.pprint_with_leftpad(writer, &successive, Spot::Final)?;
        }
        Ok(())
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |tok: &str| pprint_token(writer, tok, TokenType::Keyword, pad, state);
        match self {
            IdentifierReference::Identifier { identifier, .. } => identifier.concise_with_leftpad(writer, pad, state),
            IdentifierReference::Await { .. } => work("await"),
            IdentifierReference::Yield { .. } => work("yield"),
        }
    }
}

impl fmt::Display for IdentifierReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IdentifierReference::Identifier { identifier, .. } => write!(f, "{identifier}"),
            IdentifierReference::Yield { .. } => write!(f, "yield"),
            IdentifierReference::Await { .. } => write!(f, "await"),
        }
    }
}

impl IdentifierReference {
    fn parse_core(
        parser: &mut Parser,
        initial_scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let production = Identifier::parse(parser, initial_scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match production {
            Ok((ident, scanner)) => Ok((
                {
                    let location = ident.location();
                    Rc::new(IdentifierReference::Identifier {
                        identifier: ident,
                        data: IdRefData { yield_flag, await_flag, in_module, location },
                    })
                },
                scanner,
            )),
            Err(pe) => {
                let (token, tok_loc, scan) = scan_token(&initial_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if !await_flag && id.matches(Keyword::Await) => Ok((
                        Rc::new(IdentifierReference::Await {
                            data: IdRefData { yield_flag, await_flag, in_module, location: tok_loc },
                        }),
                        scan,
                    )),
                    Token::Identifier(id) if !yield_flag && id.matches(Keyword::Yield) => Ok((
                        Rc::new(IdentifierReference::Yield {
                            data: IdRefData { yield_flag, await_flag, in_module, location: tok_loc },
                        }),
                        scan,
                    )),
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

    pub fn location(&self) -> Location {
        match self {
            IdentifierReference::Identifier { identifier: _, data }
            | IdentifierReference::Yield { data }
            | IdentifierReference::Await { data } => data.location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            IdentifierReference::Identifier { identifier, .. } => identifier.contains(kind),
            IdentifierReference::Yield { .. } | IdentifierReference::Await { .. } => false,
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            IdentifierReference::Identifier { identifier: id, data } => {
                // IdentifierReference : Identifier
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if data.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(
                        "identifier 'yield' not allowed when yield expressions are valid",
                        Some(data.location),
                    ));
                }
                if data.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(
                        "identifier 'await' not allowed when await expressions are valid",
                        Some(data.location),
                    ));
                }
                id.early_errors(errs, strict, data.in_module);
            }
            IdentifierReference::Yield { data } => {
                // IdentifierReference : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                if strict {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in strict mode: yield",
                        Some(data.location),
                    ));
                }
            }
            IdentifierReference::Await { data } => {
                // IdentifierReference : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                if data.in_module {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in modules: await",
                        Some(data.location),
                    ));
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
        matches!(self, IdentifierReference::Identifier{identifier:id, ..} if id.string_value() == "arguments")
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        use ATTKind::{Invalid, Simple};
        use IdentifierReference::{Await, Identifier, Yield};
        match self {
            Identifier { identifier: id, .. } => {
                if strict {
                    let sv = id.string_value();
                    if sv == "eval" || sv == "arguments" { Invalid } else { Simple }
                } else {
                    Simple
                }
            }
            Await { .. } | Yield { .. } => Simple,
        }
    }

    pub fn string_value(&self) -> JSString {
        use IdentifierReference::{Await, Identifier, Yield};
        match self {
            Identifier { identifier: id, .. } => id.string_value(),
            Yield { .. } => JSString::from("yield"),
            Await { .. } => JSString::from("await"),
        }
    }
}

// BindingIdentifier[Yield, Await] :
//    Identifier
//    yield
//    await

#[derive(Debug)]
pub struct BIData {
    yield_flag: bool,
    await_flag: bool,
    in_module: bool,
    location: Location,
}

#[derive(Debug)]
pub enum BindingIdentifier {
    Identifier { identifier: Rc<Identifier>, data: BIData },
    Yield { data: BIData },
    Await { data: BIData },
}

impl fmt::Display for BindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingIdentifier::Await { .. } => write!(f, "await"),
            BindingIdentifier::Identifier { identifier, .. } => write!(f, "{identifier}"),
            BindingIdentifier::Yield { .. } => write!(f, "yield"),
        }
    }
}

impl PrettyPrint for BindingIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingIdentifier: {self}")?;
        if let BindingIdentifier::Identifier { identifier, .. } = self {
            identifier.pprint_with_leftpad(writer, &successive, Spot::Final)?;
        }
        Ok(())
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BindingIdentifier::Identifier { identifier, .. } => identifier.concise_with_leftpad(writer, pad, state),
            BindingIdentifier::Await { .. } => pprint_token(writer, "await", TokenType::Keyword, pad, state),
            BindingIdentifier::Yield { .. } => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
        }
    }
}

impl BindingIdentifier {
    fn parse_core(
        parser: &mut Parser,
        starting_scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let production = Identifier::parse(parser, starting_scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match production {
            Ok((ident, scanner)) => Ok((
                {
                    let location = ident.location();
                    Rc::new(BindingIdentifier::Identifier {
                        identifier: ident,
                        data: BIData { yield_flag, await_flag, in_module, location },
                    })
                },
                scanner,
            )),
            Err(pe) => {
                let (token, tok_loc, scan) = scan_token(&starting_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if id.matches(Keyword::Await) => Ok((
                        Rc::new(BindingIdentifier::Await {
                            data: BIData { yield_flag, await_flag, in_module, location: tok_loc },
                        }),
                        scan,
                    )),
                    Token::Identifier(id) if id.matches(Keyword::Yield) => Ok((
                        Rc::new(BindingIdentifier::Yield {
                            data: BIData { yield_flag, await_flag, in_module, location: tok_loc },
                        }),
                        scan,
                    )),
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

    pub fn location(&self) -> Location {
        match self {
            BindingIdentifier::Identifier { identifier: _, data }
            | BindingIdentifier::Yield { data }
            | BindingIdentifier::Await { data } => data.location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingIdentifier::Yield { .. } | BindingIdentifier::Await { .. } => false,
            BindingIdentifier::Identifier { identifier, .. } => identifier.contains(kind),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        vec![self.bound_name()]
    }

    pub fn bound_name(&self) -> JSString {
        use BindingIdentifier::{Await, Identifier, Yield};
        match self {
            Identifier { identifier, .. } => identifier.string_value(),
            Yield { .. } => JSString::from("yield"),
            Await { .. } => JSString::from("await"),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            BindingIdentifier::Identifier { identifier: id, data } => {
                // BindingIdentifier : Identifier
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code and
                //    the StringValue of Identifier is "arguments" or "eval".
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if strict && [JSString::from("arguments"), JSString::from("eval")].contains(&sv) {
                    errs.push(create_syntax_error_object(
                        format!("identifier not allowed in strict mode: {sv}").as_str(),
                        Some(data.location),
                    ));
                }
                if data.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(
                        "identifier 'yield' not allowed when yield expressions are valid",
                        Some(data.location),
                    ));
                }
                if data.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(
                        "identifier 'await' not allowed when await expressions are valid",
                        Some(data.location),
                    ));
                }
                id.early_errors(errs, strict, data.in_module);
            }
            BindingIdentifier::Yield { data } => {
                // BindingIdentifier : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                //  * It is a Syntax Error if this production has a [Yield] parameter.
                if strict {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in strict mode: yield",
                        Some(data.location),
                    ));
                }
                if data.yield_flag {
                    errs.push(create_syntax_error_object(
                        "identifier 'yield' not allowed when yield expressions are valid",
                        Some(data.location),
                    ));
                }
            }
            BindingIdentifier::Await { data } => {
                // BindingIdentifier : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                //  * It is a Syntax Error if this production has an [Await] parameter.
                if data.in_module {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in modules: await",
                        Some(data.location),
                    ));
                }
                if data.await_flag {
                    errs.push(create_syntax_error_object(
                        "identifier 'await' not allowed when await expressions are valid",
                        Some(data.location),
                    ));
                }
            }
        }
    }

    pub fn string_value(&self) -> JSString {
        use BindingIdentifier::{Await, Identifier, Yield};
        match self {
            Identifier { identifier, .. } => identifier.string_value(),
            Yield { .. } => JSString::from("yield"),
            Await { .. } => JSString::from("await"),
        }
    }

    pub fn body_containing_location(&self, _: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        None
    }
}

// LabelIdentifier[Yield, Await] :
//      Identifier
//      [~Yield]yield
//      [~Await]await
#[derive(Debug)]
pub struct LIData {
    in_module: bool,
    yield_flag: bool,
    await_flag: bool,
    location: Location,
}
#[derive(Debug)]
pub enum LabelIdentifier {
    Identifier { identifier: Rc<Identifier>, data: LIData },
    Yield { data: LIData },
    Await { data: LIData },
}

impl fmt::Display for LabelIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelIdentifier::Identifier { identifier: id, .. } => id.fmt(f),
            LabelIdentifier::Yield { .. } => write!(f, "yield"),
            LabelIdentifier::Await { .. } => write!(f, "await"),
        }
    }
}

impl PrettyPrint for LabelIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LabelIdentifier: {self}")?;
        match self {
            LabelIdentifier::Identifier { identifier: id, .. } => {
                id.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            LabelIdentifier::Yield { .. } | LabelIdentifier::Await { .. } => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LabelIdentifier::Identifier { identifier: id, .. } => id.concise_with_leftpad(writer, pad, state),
            LabelIdentifier::Yield { .. } => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            LabelIdentifier::Await { .. } => pprint_token(writer, "await", TokenType::Keyword, pad, state),
        }
    }
}

impl LabelIdentifier {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let pot_id = Identifier::parse(parser, scanner);
        let in_module = parser.goal == ParseGoal::Module;
        match pot_id {
            Ok((id, after_id)) => Ok((
                {
                    let location = id.location();
                    Rc::new(LabelIdentifier::Identifier {
                        identifier: id,
                        data: LIData { in_module, yield_flag, await_flag, location },
                    })
                },
                after_id,
            )),
            Err(pe) => {
                if !yield_flag || !await_flag {
                    let (tok, tok_loc, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
                    if !yield_flag && tok.matches_keyword(Keyword::Yield) {
                        Ok((
                            Rc::new(LabelIdentifier::Yield {
                                data: LIData { in_module, yield_flag, await_flag, location: tok_loc },
                            }),
                            after_tok,
                        ))
                    } else if !await_flag && tok.matches_keyword(Keyword::Await) {
                        Ok((
                            Rc::new(LabelIdentifier::Await {
                                data: LIData { in_module, yield_flag, await_flag, location: tok_loc },
                            }),
                            after_tok,
                        ))
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

    pub fn location(&self) -> Location {
        match self {
            LabelIdentifier::Identifier { data, .. }
            | LabelIdentifier::Yield { data }
            | LabelIdentifier::Await { data } => data.location,
        }
    }

    pub fn string_value(&self) -> JSString {
        match self {
            LabelIdentifier::Await { .. } => JSString::from("await"),
            LabelIdentifier::Yield { .. } => JSString::from("yield"),
            LabelIdentifier::Identifier { identifier: id, .. } => id.string_value(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LabelIdentifier::Identifier { identifier: node, .. } => node.contains(kind),
            LabelIdentifier::Yield { .. } | LabelIdentifier::Await { .. } => false,
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            LabelIdentifier::Identifier { identifier: id, data } => {
                // LabelIdentifier : Identifier
                //  * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
                //  * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
                let sv = id.string_value();
                if data.yield_flag && sv == "yield" {
                    errs.push(create_syntax_error_object(
                        "identifier 'yield' not allowed when yield expressions are valid",
                        Some(data.location),
                    ));
                }
                if data.await_flag && sv == "await" {
                    errs.push(create_syntax_error_object(
                        "identifier 'await' not allowed when await expressions are valid",
                        Some(data.location),
                    ));
                }
                id.early_errors(errs, strict, data.in_module);
            }
            LabelIdentifier::Yield { data } => {
                // LabelIdentifier : yield
                //  * It is a Syntax Error if the code matched by this production is contained in strict mode code.
                if strict {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in strict mode: yield",
                        Some(data.location),
                    ));
                }
            }
            LabelIdentifier::Await { data } => {
                // LabelIdentifier : await
                //  * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
                if data.in_module {
                    errs.push(create_syntax_error_object(
                        "identifier not allowed in modules: await",
                        Some(data.location),
                    ));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;

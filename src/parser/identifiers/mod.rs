use super::scanner::{scan_token, IdentifierData, Keyword, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use crate::strings::JSString;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub enum Identifier {
    IdentifierName(IdentifierData),
}

impl StringValue for Identifier {
    fn string_value(&self) -> JSString {
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
                | Some(Keyword::Yield) => Err(ParseError::new(format!("‘{}’ is a reserved word and may not be used as an identifier", id.string_value), scanner.line, scanner.column)),
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
                        Err(ParseError::new(format!("‘{}’ not allowed as an identifier in strict mode", id.string_value), id.line, id.column))
                    } else if parser.goal == ParseGoal::Module && id.string_value == "await" {
                        Err(ParseError::new("‘await’ not allowed as an identifier in modules", id.line, id.column))
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
                        Err(ParseError::new(format!("‘{}’ is a reserved word and may not be used as an identifier", id.string_value), id.line, id.column))
                    } else {
                        Ok((Rc::new(Identifier::IdentifierName(id)), after_tok))
                    }
                }
            },
            _ => Err(ParseError::new("Not an identifier", scanner.line, scanner.column)),
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
    strict: bool,
}

impl StringValue for IdentifierReference {
    fn string_value(&self) -> JSString {
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => JSString::from("yield"),
            Await => JSString::from("await"),
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
        match production {
            Ok((ident, scanner)) => {
                let node = IdentifierReference { kind: IdentifierReferenceKind::Identifier(ident), strict: parser.strict };
                let boxed = Rc::new(node);
                Ok((boxed, scanner))
            }
            Err(pe) => {
                let (token, scan) = scan_token(&initial_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if !arg_await && id.matches(Keyword::Await) => {
                        Ok((Rc::new(IdentifierReference { kind: IdentifierReferenceKind::Await, strict: parser.strict }), scan))
                    }
                    Token::Identifier(id) if !arg_yield && id.matches(Keyword::Yield) => {
                        Ok((Rc::new(IdentifierReference { kind: IdentifierReferenceKind::Yield, strict: parser.strict }), scan))
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
}

impl StringValue for BindingIdentifier {
    fn string_value(&self) -> JSString {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => JSString::from("yield"),
            Await => JSString::from("await"),
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
        match production {
            Ok((ident, scanner)) => {
                let node = BindingIdentifier { kind: BindingIdentifierKind::Identifier(ident), yield_flag, await_flag };
                let boxed = Rc::new(node);
                Ok((boxed, scanner))
            }
            Err(pe) => {
                let (token, scan) = scan_token(&starting_scanner, parser.source, ScanGoal::InputElementRegExp);
                match token {
                    Token::Identifier(id) if id.matches(Keyword::Await) => Ok((Rc::new(BindingIdentifier { kind: BindingIdentifierKind::Await, yield_flag, await_flag }), scan)),
                    Token::Identifier(id) if id.matches(Keyword::Yield) => Ok((Rc::new(BindingIdentifier { kind: BindingIdentifierKind::Yield, yield_flag, await_flag }), scan)),
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

    pub fn early_errors(&self, _agent: &mut Agent) -> Vec<Object> {
        // todo!()
        println!("{}:{}: Not yet implemented", file!(), line!());
        Vec::new()
    }
}

// LabelIdentifier[Yield, Await] :
//      Identifier
//      [~Yield]yield
//      [~Await]await
#[derive(Debug)]
pub enum LabelIdentifier {
    Identifier(Rc<Identifier>),
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
            LabelIdentifier::Yield => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            LabelIdentifier::Await => pprint_token(writer, "await", TokenType::Keyword, pad, state),
        }
    }
}

impl LabelIdentifier {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let pot_id = Identifier::parse(parser, scanner);
        match pot_id {
            Ok((id, after_id)) => Ok((Rc::new(LabelIdentifier::Identifier(id)), after_id)),
            Err(pe) => {
                if !yield_flag || !await_flag {
                    let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
                    if !yield_flag && tok.matches_keyword(Keyword::Yield) {
                        Ok((Rc::new(LabelIdentifier::Yield), after_tok))
                    } else if !await_flag && tok.matches_keyword(Keyword::Await) {
                        Ok((Rc::new(LabelIdentifier::Await), after_tok))
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
        match self {
            LabelIdentifier::Await => JSString::from("await"),
            LabelIdentifier::Yield => JSString::from("yield"),
            LabelIdentifier::Identifier(id) => id.string_value(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LabelIdentifier::Identifier(node) => node.contains(kind),
            LabelIdentifier::Yield => false,
            LabelIdentifier::Await => false,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent) -> Vec<Object> {
        // todo!()
        println!("{}:{}: Not yet implemented", file!(), line!());
        Vec::new()
    }
}

#[cfg(test)]
mod tests;

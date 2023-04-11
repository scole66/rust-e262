use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// UnaryExpression[Yield, Await] :
//      UpdateExpression[?Yield, ?Await]
//      delete UnaryExpression[?Yield, ?Await]
//      void UnaryExpression[?Yield, ?Await]
//      typeof UnaryExpression[?Yield, ?Await]
//      + UnaryExpression[?Yield, ?Await]
//      - UnaryExpression[?Yield, ?Await]
//      ~ UnaryExpression[?Yield, ?Await]
//      ! UnaryExpression[?Yield, ?Await]
//      [+Await]AwaitExpression[?Yield]
#[derive(Debug)]
pub enum UnaryExpression {
    UpdateExpression(Rc<UpdateExpression>),
    Delete { ue: Rc<UnaryExpression>, location: Location },
    Void { ue: Rc<UnaryExpression>, location: Location },
    Typeof { ue: Rc<UnaryExpression>, location: Location },
    NoOp { ue: Rc<UnaryExpression>, location: Location },
    Negate { ue: Rc<UnaryExpression>, location: Location },
    Complement { ue: Rc<UnaryExpression>, location: Location },
    Not { ue: Rc<UnaryExpression>, location: Location },
    Await(Rc<AwaitExpression>),
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryExpression::UpdateExpression(boxed) => write!(f, "{}", boxed),
            UnaryExpression::Delete { ue: boxed, .. } => write!(f, "delete {}", boxed),
            UnaryExpression::Void { ue: boxed, .. } => write!(f, "void {}", boxed),
            UnaryExpression::Typeof { ue: boxed, .. } => write!(f, "typeof {}", boxed),
            UnaryExpression::NoOp { ue: boxed, .. } => write!(f, "+ {}", boxed),
            UnaryExpression::Negate { ue: boxed, .. } => write!(f, "- {}", boxed),
            UnaryExpression::Complement { ue: boxed, .. } => write!(f, "~ {}", boxed),
            UnaryExpression::Not { ue: boxed, .. } => write!(f, "! {}", boxed),
            UnaryExpression::Await(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for UnaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UnaryExpression: {}", first, self)?;
        match &self {
            UnaryExpression::UpdateExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Delete { ue: boxed, .. }
            | UnaryExpression::Void { ue: boxed, .. }
            | UnaryExpression::Typeof { ue: boxed, .. }
            | UnaryExpression::NoOp { ue: boxed, .. }
            | UnaryExpression::Negate { ue: boxed, .. }
            | UnaryExpression::Complement { ue: boxed, .. }
            | UnaryExpression::Not { ue: boxed, .. } => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Await(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |node: &UnaryExpression, op, kind| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}UnaryExpression: {}", first, self)
                .and_then(|_| pprint_token(writer, op, kind, &successive, Spot::NotFinal))
                .and_then(|_| node.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            UnaryExpression::UpdateExpression(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Await(node) => node.concise_with_leftpad(writer, pad, state),
            UnaryExpression::Delete { ue: node, .. } => work(node, "delete", TokenType::Keyword),
            UnaryExpression::Void { ue: node, .. } => work(node, "void", TokenType::Keyword),
            UnaryExpression::Typeof { ue: node, .. } => work(node, "typeof", TokenType::Keyword),
            UnaryExpression::NoOp { ue: node, .. } => work(node, "+", TokenType::Punctuator),
            UnaryExpression::Negate { ue: node, .. } => work(node, "-", TokenType::Punctuator),
            UnaryExpression::Complement { ue: node, .. } => work(node, "~", TokenType::Punctuator),
            UnaryExpression::Not { ue: node, .. } => work(node, "!", TokenType::Punctuator),
        }
    }
}

impl IsFunctionDefinition for UnaryExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.is_function_definition(),
            UnaryExpression::Delete { .. }
            | UnaryExpression::Void { .. }
            | UnaryExpression::Typeof { .. }
            | UnaryExpression::NoOp { .. }
            | UnaryExpression::Negate { .. }
            | UnaryExpression::Complement { .. }
            | UnaryExpression::Not { .. }
            | UnaryExpression::Await(_) => false,
        }
    }
}

enum UETokenType {
    Delete,
    Void,
    Typeof,
    Noop,
    Negate,
    Complement,
    Not,
}
impl TryFrom<Token> for UETokenType {
    type Error = ();
    fn try_from(src: Token) -> Result<Self, Self::Error> {
        match src {
            Token::Identifier(id) if id.matches(Keyword::Delete) => Ok(Self::Delete),
            Token::Identifier(id) if id.matches(Keyword::Void) => Ok(Self::Void),
            Token::Identifier(id) if id.matches(Keyword::Typeof) => Ok(Self::Typeof),
            Token::Punctuator(Punctuator::Plus) => Ok(Self::Noop),
            Token::Punctuator(Punctuator::Minus) => Ok(Self::Negate),
            Token::Punctuator(Punctuator::Tilde) => Ok(Self::Complement),
            Token::Punctuator(Punctuator::Bang) => Ok(Self::Not),
            _ => Err(()),
        }
    }
}

impl UnaryExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (token, tok_loc, after_token) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);

        if let Ok(prod_marker) = UETokenType::try_from(token) {
            let (ue, after_ue) = UnaryExpression::parse(parser, after_token, yield_flag, await_flag)?;
            let location = tok_loc.merge(&ue.location());
            Ok((
                Rc::new(match prod_marker {
                    UETokenType::Delete => UnaryExpression::Delete { ue, location },
                    UETokenType::Void => UnaryExpression::Void { ue, location },
                    UETokenType::Typeof => UnaryExpression::Typeof { ue, location },
                    UETokenType::Noop => UnaryExpression::NoOp { ue, location },
                    UETokenType::Negate => UnaryExpression::Negate { ue, location },
                    UETokenType::Complement => UnaryExpression::Complement { ue, location },
                    UETokenType::Not => UnaryExpression::Not { ue, location },
                }),
                after_ue,
            ))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::UnaryExpression), scanner))
                .otherwise(|| {
                    if await_flag {
                        AwaitExpression::parse(parser, scanner, yield_flag)
                            .map(|(ae, after)| (Rc::new(UnaryExpression::Await(ae)), after))
                    } else {
                        Err(ParseError::default())
                    }
                })
                .otherwise(|| {
                    UpdateExpression::parse(parser, scanner, yield_flag, await_flag)
                        .map(|(ue, after)| (Rc::new(UnaryExpression::UpdateExpression(ue)), after))
                })
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.unary_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.unary_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.location(),
            UnaryExpression::Delete { location, .. }
            | UnaryExpression::Void { location, .. }
            | UnaryExpression::Typeof { location, .. }
            | UnaryExpression::NoOp { location, .. }
            | UnaryExpression::Negate { location, .. }
            | UnaryExpression::Complement { location, .. }
            | UnaryExpression::Not { location, .. } => *location,
            UnaryExpression::Await(ae) => ae.location(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            UnaryExpression::UpdateExpression(n) => n.contains(kind),
            UnaryExpression::Delete { ue, .. }
            | UnaryExpression::Void { ue, .. }
            | UnaryExpression::Typeof { ue, .. }
            | UnaryExpression::NoOp { ue, .. }
            | UnaryExpression::Negate { ue, .. }
            | UnaryExpression::Complement { ue, .. }
            | UnaryExpression::Not { ue, .. } => ue.contains(kind),
            UnaryExpression::Await(n) => kind == ParseNodeKind::AwaitExpression || n.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            UnaryExpression::UpdateExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            UnaryExpression::UpdateExpression(n) => n.all_private_identifiers_valid(names),
            UnaryExpression::Delete { ue, .. }
            | UnaryExpression::Void { ue, .. }
            | UnaryExpression::Typeof { ue, .. }
            | UnaryExpression::NoOp { ue, .. }
            | UnaryExpression::Negate { ue, .. }
            | UnaryExpression::Complement { ue, .. }
            | UnaryExpression::Not { ue, .. } => ue.all_private_identifiers_valid(names),
            UnaryExpression::Await(n) => n.all_private_identifiers_valid(names),
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.contains_arguments(),
            UnaryExpression::Delete { ue, .. }
            | UnaryExpression::Void { ue, .. }
            | UnaryExpression::Typeof { ue, .. }
            | UnaryExpression::NoOp { ue, .. }
            | UnaryExpression::Negate { ue, .. }
            | UnaryExpression::Complement { ue, .. }
            | UnaryExpression::Not { ue, .. } => ue.contains_arguments(),
            UnaryExpression::Await(ae) => ae.contains_arguments(),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            UnaryExpression::UpdateExpression(exp) => exp.is_strictly_deletable(),
            _ => true,
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            UnaryExpression::UpdateExpression(n) => n.early_errors(errs, strict),
            UnaryExpression::Delete { ue: n, .. } => {
                // Static Semantics: Early Errors
                //      UnaryExpression : delete UnaryExpression
                //
                //  * It is a Syntax Error if the UnaryExpression is contained in strict mode code and the derived
                //    UnaryExpression is one of:
                //      PrimaryExpression : IdentifierReference
                //      MemberExpression : MemberExpression . PrivateIdentifier
                //      CallExpression : CallExpression . PrivateIdentifier
                //      OptionalChain : ?. PrivateIdentifier
                //      OptionalChain : OptionalChain . PrivateIdentifier
                //  * It is a Syntax Error if the derived UnaryExpression is
                //      PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
                //    and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in
                //    place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is
                //    recursively applied.
                //
                // NOTE |   The last rule means that expressions such as delete (((foo))) produce early errors because
                //      |   of recursive application of the first rule.
                if strict && !n.is_strictly_deletable() {
                    // node.js errors for these cases:
                    //  * "Private fields can not be deleted"
                    //  * "Delete of an unqualified identifier in strict mode."
                    errs.push(create_syntax_error_object("Item is not deletable", Some(n.location())));
                }
                n.early_errors(errs, strict);
            }
            UnaryExpression::Void { ue, .. }
            | UnaryExpression::Typeof { ue, .. }
            | UnaryExpression::NoOp { ue, .. }
            | UnaryExpression::Negate { ue, .. }
            | UnaryExpression::Complement { ue, .. }
            | UnaryExpression::Not { ue, .. } => ue.early_errors(errs, strict),
            UnaryExpression::Await(n) => n.early_errors(errs, strict),
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            UnaryExpression::UpdateExpression(boxed) => boxed.assignment_target_type(strict),
            UnaryExpression::Delete { .. }
            | UnaryExpression::Void { .. }
            | UnaryExpression::Typeof { .. }
            | UnaryExpression::NoOp { .. }
            | UnaryExpression::Negate { .. }
            | UnaryExpression::Complement { .. }
            | UnaryExpression::Not { .. }
            | UnaryExpression::Await(_) => ATTKind::Invalid,
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            UnaryExpression::UpdateExpression(node) => node.is_named_function(),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests;

use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ExpressionStatement[Yield, Await] :
//      [lookahead ∉ { {, function, async [no LineTerminator here] function, class, let [ }] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub struct ExpressionStatement {
    pub exp: Rc<Expression>,
    location: Location,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ;", self.exp)
    }
}

impl PrettyPrint for ExpressionStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ExpressionStatement: {self}")?;
        self.exp.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ExpressionStatement: {self}")?;
        self.exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ExpressionStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (first_token, _, after_token) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        let invalid = match first_token {
            Token::Punctuator(Punctuator::LeftBrace) => true,
            Token::Identifier(id) if id.matches(Keyword::Function) => true,
            Token::Identifier(id) if id.matches(Keyword::Class) => true,
            Token::Identifier(id) if id.matches(Keyword::Let) => {
                let (second_token, _, _) = scan_token(&after_token, parser.source, ScanGoal::InputElementRegExp);
                second_token.matches_punct(Punctuator::LeftBracket)
            }
            Token::Identifier(id) if id.matches(Keyword::Async) => {
                if no_line_terminator(after_token, parser.source).is_ok() {
                    let (second_token, _, _) = scan_token(&after_token, parser.source, ScanGoal::InputElementRegExp);
                    matches!(second_token, Token::Identifier(xx) if xx.matches(Keyword::Function))
                } else {
                    false
                }
            }
            _ => false,
        };

        if invalid {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ExpressionStatement), scanner))
        } else {
            let (exp, after_exp) = Expression::parse(parser, scanner, true, yield_flag, await_flag)?;
            let (semi_loc, after_semi) = scan_for_auto_semi(after_exp, parser.source, ScanGoal::InputElementRegExp)?;
            let location = exp.location().merge(&semi_loc);
            Ok((Rc::new(ExpressionStatement { exp, location }), after_semi))
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::Expression || self.exp.contains(kind)
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        self.exp.as_string_literal()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.exp.all_private_identifiers_valid(names)
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
        self.exp.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.exp.early_errors(errs, strict);
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.exp.body_containing_location(location) } else { None }
    }
}

#[cfg(test)]
mod tests;

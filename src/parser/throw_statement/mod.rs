use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ThrowStatement[Yield, Await] :
//      throw [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub(crate) struct ThrowStatement {
    pub(crate) exp: Rc<Expression>,
    location: Location,
}

impl fmt::Display for ThrowStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "throw {} ;", self.exp)
    }
}

impl PrettyPrint for ThrowStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ThrowStatement: {self}")?;
        self.exp.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ThrowStatement: {self}")?;
        pprint_token(writer, "throw", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ThrowStatement {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (throw_loc, after_throw) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Throw)?;
        no_line_terminator(after_throw, parser.source)?;
        let (exp, after_exp) = Expression::parse(parser, after_throw, true, yield_flag, await_flag)?;
        let (semi_loc, after_semi) = scan_for_auto_semi(after_exp, parser.source, InputElementGoal::RegExp)?;
        Ok((Rc::new(ThrowStatement { exp, location: throw_loc.merge(&semi_loc) }), after_semi))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.exp.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.exp.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.exp.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

#[cfg(test)]
mod tests;

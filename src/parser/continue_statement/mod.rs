use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ContinueStatement[Yield, Await] :
//      continue ;
//      continue [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum ContinueStatement {
    Bare { location: Location },
    Labelled { label: Rc<LabelIdentifier>, location: Location },
}

impl fmt::Display for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ContinueStatement::Bare { .. } => write!(f, "continue ;"),
            ContinueStatement::Labelled { label, .. } => write!(f, "continue {label} ;"),
        }
    }
}

impl PrettyPrint for ContinueStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ContinueStatement: {self}")?;
        match self {
            ContinueStatement::Bare { .. } => Ok(()),
            ContinueStatement::Labelled { label, .. } => label.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ContinueStatement: {self}")?;
        pprint_token(writer, "continue", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let ContinueStatement::Labelled { label, .. } = self {
            label.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ContinueStatement {
    // no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (cont_loc, after_cont) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Continue)?;
        scan_for_auto_semi(after_cont, parser.source, ScanGoal::InputElementDiv)
            .map(|(semi_loc, after_semi)| {
                (Rc::new(ContinueStatement::Bare { location: cont_loc.merge(&semi_loc) }), after_semi)
            })
            .otherwise(|| {
                let (li, after_li) = LabelIdentifier::parse(parser, after_cont, yield_flag, await_flag)?;
                let (semi_loc, after_semi) = scan_for_auto_semi(after_li, parser.source, ScanGoal::InputElementDiv)?;
                Ok((
                    Rc::new(ContinueStatement::Labelled { label: li, location: cont_loc.merge(&semi_loc) }),
                    after_semi,
                ))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            ContinueStatement::Bare { location } | ContinueStatement::Labelled { location, .. } => *location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ContinueStatement::Bare { .. } => false,
            ContinueStatement::Labelled { label, .. } => label.contains(kind),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            ContinueStatement::Bare { .. } => false,
            ContinueStatement::Labelled { label, .. } => !iteration_set.contains(&label.string_value()),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        // Static Semantics: Early Errors
        // ContinueStatement :
        //      continue ;
        //      continue LabelIdentifier ;
        //  * It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing
        //    function or static initialization block boundaries), within an IterationStatement.
        if !within_iteration {
            errs.push(create_syntax_error_object(
                "Continue statements must lie within iteration statements.",
                Some(self.location()),
            ));
        }
        if let ContinueStatement::Labelled { label, .. } = self {
            label.early_errors(errs, strict);
        }
    }
}

#[cfg(test)]
mod tests;

use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// BreakStatement[Yield, Await] :
//      break ;
//      break [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum BreakStatement {
    Bare { location: Location },
    Labelled { label: Rc<LabelIdentifier>, location: Location },
}

impl fmt::Display for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BreakStatement::Bare { .. } => write!(f, "break ;"),
            BreakStatement::Labelled { label, .. } => write!(f, "break {} ;", label),
        }
    }
}

impl PrettyPrint for BreakStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        match self {
            BreakStatement::Bare { .. } => Ok(()),
            BreakStatement::Labelled { label, .. } => label.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        pprint_token(writer, "break", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let BreakStatement::Labelled { label: node, .. } = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl BreakStatement {
    // no cache needed
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (break_loc, after_break) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Break)?;
        scan_for_auto_semi(after_break, parser.source, ScanGoal::InputElementDiv)
            .map(|(semi_loc, after_semi)| {
                (Rc::new(BreakStatement::Bare { location: break_loc.merge(&semi_loc) }), after_semi)
            })
            .otherwise(|| {
                let (li, after_li) = LabelIdentifier::parse(parser, after_break, yield_flag, await_flag)?;
                let (semi_loc, after_semi) = scan_for_auto_semi(after_li, parser.source, ScanGoal::InputElementDiv)?;
                Ok((Rc::new(BreakStatement::Labelled { label: li, location: break_loc.merge(&semi_loc) }), after_semi))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            BreakStatement::Bare { location } | BreakStatement::Labelled { location, .. } => *location,
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            BreakStatement::Bare { .. } => false,
            BreakStatement::Labelled { label, .. } => !label_set.contains(&label.string_value()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BreakStatement::Bare { .. } => false,
            BreakStatement::Labelled { label, .. } => label.contains(kind),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_breakable: bool) {
        // Static Semantics: Early Errors
        //  BreakStatement : break ;
        //      * It is a Syntax Error if this BreakStatement is not nested, directly or indirectly (but not crossing
        //        function or static initialization block boundaries), within an IterationStatement or a
        //        SwitchStatement.
        match self {
            BreakStatement::Bare { .. } => {
                if !within_breakable {
                    errs.push(create_syntax_error_object(
                        "break statement must lie within iteration or switch statement",
                        Some(self.location()),
                    ));
                }
            }
            BreakStatement::Labelled { label, .. } => label.early_errors(errs, strict),
        }
    }
}

#[cfg(test)]
mod tests;

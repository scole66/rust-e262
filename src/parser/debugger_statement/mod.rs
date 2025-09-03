use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// DebuggerStatement :
//      debugger ;
#[derive(Debug)]
pub(crate) struct DebuggerStatement {
    location: Location,
}

impl fmt::Display for DebuggerStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "debugger ;")
    }
}

impl PrettyPrint for DebuggerStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}DebuggerStatement: {self}")
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}DebuggerStatement: {self}")?;
        pprint_token(writer, "debugger", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl DebuggerStatement {
    // no need to cache
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (deb_loc, after_deb) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Debugger)?;
        let (semi_loc, after_semi) = scan_for_auto_semi(after_deb, parser.source, InputElementGoal::Div)?;
        Ok((Rc::new(DebuggerStatement { location: deb_loc.merge(&semi_loc) }), after_semi))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }
}

#[cfg(test)]
mod tests;

use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// EmptyStatement :
//      ;
#[derive(Debug)]
pub(crate) struct EmptyStatement {
    location: Location,
}

impl fmt::Display for EmptyStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ";")
    }
}

impl PrettyPrint for EmptyStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}EmptyStatement: {self}")
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, ";", TokenType::Punctuator, pad, state)
    }
}

impl EmptyStatement {
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (semi_loc, after_semi) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Semicolon)?;
        Ok((Rc::new(EmptyStatement { location: semi_loc }), after_semi))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }
}

#[cfg(test)]
mod tests;

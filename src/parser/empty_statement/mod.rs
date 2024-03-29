use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// EmptyStatement :
//      ;
#[derive(Debug)]
pub struct EmptyStatement {
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
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (semi_loc, after_semi) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Rc::new(EmptyStatement { location: semi_loc }), after_semi))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

#[cfg(test)]
mod tests;

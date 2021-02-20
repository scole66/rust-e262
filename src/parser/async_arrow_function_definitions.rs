use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum AsyncArrowFunction {}

impl fmt::Display for AsyncArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowFunction: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl AsyncArrowFunction {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _in_flag: bool, _yield_flag: bool, _await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncArrowFunction unimplemeted", scanner.line, scanner.column))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

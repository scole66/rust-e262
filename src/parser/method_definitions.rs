use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum MethodDefinition {}

impl fmt::Display for MethodDefinition {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}

impl PrettyPrint for MethodDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}MethodDefinition: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl MethodDefinition {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _yield_flag: bool, _await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("MethodDefinition unimplmenented", scanner.line, scanner.column))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}

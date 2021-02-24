use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum AsyncGeneratorMethod {}

impl fmt::Display for AsyncGeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncGeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorMethod: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl AsyncGeneratorMethod {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _yield_flag: bool, _await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncGeneratorMethod unimplemenented", scanner.line, scanner.column))
    }
}

#[derive(Debug)]
pub enum AsyncGeneratorDeclaration {}

impl fmt::Display for AsyncGeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncGeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorDeclaration: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl IsFunctionDefinition for AsyncGeneratorDeclaration {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorDeclaration {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _yield_flag: bool, _await_flag: bool, _default_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncGeneratorDeclaration unimplemented", scanner.line, scanner.column))
    }
}

#[derive(Debug)]
pub enum AsyncGeneratorExpression {}

impl fmt::Display for AsyncGeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncGeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorExpression: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl IsFunctionDefinition for AsyncGeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorExpression {
    pub fn parse(_parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncGeneratorExpression Unimplemented", scanner.line, scanner.column))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

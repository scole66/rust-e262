use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum ClassExpression {}

impl fmt::Display for ClassExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for ClassExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassExpression: {}", first, self)
    }
}

impl IsFunctionDefinition for ClassExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl ClassExpression {
    pub fn parse(
        _parser: &mut Parser,
        _scanner: Scanner,
        _yield_flag: bool,
        _await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

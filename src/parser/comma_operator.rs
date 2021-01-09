use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum Expression {
    // todo!
    Temp(Box<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self::Temp(boxed) = &self;
        write!(f, "{}", boxed)
    }
}

impl PrettyPrint for Expression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Expression: {}", first, self)?;
        let Expression::Temp(boxed) = &self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Expression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        // todo!
        let potential = AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match potential {
            None => Ok(None),
            Some((boxed, scanner)) => Ok(Some((Box::new(Expression::Temp(boxed)), scanner))),
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}

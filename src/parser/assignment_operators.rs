use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

use super::relational_operators::RelationalExpression;

#[derive(Debug)]
pub enum AssignmentExpressionKind {
    Temp(Box<RelationalExpression>),
}
#[derive(Debug)]
pub struct AssignmentExpression {
    pub kind: AssignmentExpressionKind,
}

impl fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        write!(f, "{}", boxed)
    }
}

impl PrettyPrint for AssignmentExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for AssignmentExpression {
    fn is_function_definition(&self) -> bool {
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        boxed.is_function_definition()
    }
}

impl AssignmentTargetType for AssignmentExpression {
    fn assignment_target_type(&self) -> ATTKind {
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        boxed.assignment_target_type()
    }
}

impl AssignmentExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<AssignmentExpression>, Scanner)>, String> {
        let potential = RelationalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match potential {
            None => Ok(None),
            Some((boxed, scanner)) => Ok(Some((
                Box::new(AssignmentExpression {
                    kind: AssignmentExpressionKind::Temp(boxed),
                }),
                scanner,
            ))),
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}

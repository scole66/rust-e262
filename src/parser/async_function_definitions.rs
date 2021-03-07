use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionBody;
use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

#[derive(Debug)]
pub enum AsyncMethod {}

impl fmt::Display for AsyncMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncMethod: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl AsyncMethod {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _yield_flag: bool, _await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncMethod unimplemenented", scanner.line, scanner.column))
    }
}

#[derive(Debug)]
pub enum AsyncFunctionDeclaration {}

impl fmt::Display for AsyncFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncFunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionDeclaration: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl AsyncFunctionDeclaration {
    pub fn parse(_parser: &mut Parser, scanner: Scanner, _yield_flag: bool, _await_flag: bool, _default_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncFunctionDeclaration unimplemented", scanner.line, scanner.column))
    }
}

#[derive(Debug)]
pub enum AsyncFunctionExpression {}

impl fmt::Display for AsyncFunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unimplemented")
    }
}

impl PrettyPrint for AsyncFunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionExpression: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, _writer: &mut T, _pad: &str, _state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        todo!()
    }
}

impl IsFunctionDefinition for AsyncFunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncFunctionExpression {
    pub fn parse(_parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("AsyncFunctionExpression unimplemented", scanner.line, scanner.column))
    }
}

// AsyncFunctionBody :
//      FunctionBody[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncFunctionBody(Box<FunctionBody>);

impl fmt::Display for AsyncFunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncFunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncFunctionBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Box<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, false, true);
        (Box::new(AsyncFunctionBody(fb)), after_fb)
    }
}

// AwaitExpression[Yield] :
//      await UnaryExpression[?Yield, +Await]
#[derive(Debug)]
pub enum AwaitExpression {
    Await(Box<UnaryExpression>),
}

impl fmt::Display for AwaitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AwaitExpression::Await(boxed) = &self;
        write!(f, "await {}", boxed)
    }
}

impl PrettyPrint for AwaitExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        let AwaitExpression::Await(boxed) = &self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        pprint_token(writer, "await", TokenType::Keyword, &successive, Spot::NotFinal)?;
        let AwaitExpression::Await(ue) = self;
        ue.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AwaitExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_await = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Await)?;
        let (ue, after_ue) = UnaryExpression::parse(parser, after_await, yield_flag, true)?;
        Ok((Box::new(AwaitExpression::Await(ue)), after_ue))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}

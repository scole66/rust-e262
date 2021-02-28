use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::arrow_function_definitions::{ArrowFormalParameters, ExpressionBody};
use super::async_function_definitions::AsyncFunctionBody;
use super::identifiers::BindingIdentifier;
use super::left_hand_side_expressions::{Arguments, MemberExpression};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncArrowFunction[In, Yield, Await] :
//      async [no LineTerminator here] AsyncArrowBindingIdentifier[?Yield] [no LineTerminator here] => AsyncConciseBody[?In]
//      CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await] [no LineTerminator here] => AsyncConciseBody[?In]
#[derive(Debug)]
pub enum AsyncArrowFunction {
    IdentOnly(Box<AsyncArrowBindingIdentifier>, Box<AsyncConciseBody>),
    Formals(Box<AsyncArrowHead>, Box<AsyncConciseBody>),
}

impl fmt::Display for AsyncArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncArrowFunction::IdentOnly(id, body) => write!(f, "async {} => {}", id, body),
            AsyncArrowFunction::Formals(params, body) => write!(f, "{} => {}", params, body),
        }
    }
}

impl PrettyPrint for AsyncArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowFunction: {}", first, self)?;
        match self {
            AsyncArrowFunction::IdentOnly(id, body) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(params, body) => {
                params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowFunction: {}", first, self)?;
        match self {
            AsyncArrowFunction::IdentOnly(head, tail) => {
                pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(head, tail) => {
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncArrowFunction {
    fn parse_normal_form(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (id, after_id) = AsyncArrowBindingIdentifier::parse(parser, after_async, yield_flag)?;
        no_line_terminator(after_id, parser.source)?;
        let after_arrow = scan_for_punct(after_id, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((Box::new(AsyncArrowFunction::IdentOnly(id, body)), after_body))
    }
    fn parse_covered_form(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Box<CoverCallExpressionAndAsyncArrowHead>, Scanner, Box<AsyncConciseBody>, Scanner), ParseError> {
        let (cceaaah, after_params) = CoverCallExpressionAndAsyncArrowHead::parse(parser, scanner, yield_flag, await_flag)?;
        no_line_terminator(after_params, parser.source)?;
        let after_arrow = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((cceaaah, after_params, body, after_body))
    }
    fn reparse(parser: &mut Parser, start: Scanner, after: Scanner) -> Result<Box<AsyncArrowHead>, ParseError> {
        let (real_params, after_reals) = AsyncArrowHead::parse(parser, start)?;
        if after_reals != after {
            Err(ParseError::new("‘)’ expected", after_reals.line, after_reals.column))
        } else {
            Ok(real_params)
        }
    }
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let pot_norm = Self::parse_normal_form(parser, scanner, in_flag, yield_flag, await_flag);
        let pot_covered = Self::parse_covered_form(parser, scanner, in_flag, yield_flag, await_flag);
        match (pot_norm, pot_covered) {
            (Err(err1), Err(err2)) => Err(cmp::max_by(err2, err1, ParseError::compare)),
            (Err(_), Ok((params, after_params, body, after_covered))) => {
                let real_params = Self::reparse(parser, scanner, after_params)?;
                Ok((Box::new(AsyncArrowFunction::Formals(real_params, body)), after_covered))
            }
            (Ok(norm), Err(_)) => Ok(norm),
            (Ok((normal, after_normal)), Ok((params, after_params, body, after_covered))) => {
                if after_normal >= after_covered {
                    Ok((normal, after_normal))
                } else {
                    let real_params = Self::reparse(parser, scanner, after_params)?;
                    Ok((Box::new(AsyncArrowFunction::Formals(real_params, body)), after_covered))
                }
            }
        }
    }
}

// AsyncArrowHead :
//      async [no LineTerminator here] ArrowFormalParameters[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowHead(Box<ArrowFormalParameters>);

impl fmt::Display for AsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async {}", self.0)
    }
}

impl PrettyPrint for AsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowHead: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowHead: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AsyncArrowHead {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (params, after_params) = ArrowFormalParameters::parse(parser, after_async, false, true)?;
        Ok((Box::new(AsyncArrowHead(params)), after_params))
    }
}

// AsyncConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, +Await]
//      { AsyncFunctionBody }
#[derive(Debug)]
pub enum AsyncConciseBody {
    Expression(Box<ExpressionBody>),
    Function(Box<AsyncFunctionBody>),
}

impl fmt::Display for AsyncConciseBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncConciseBody::Expression(node) => node.fmt(f),
            AsyncConciseBody::Function(node) => write!(f, "{{ {} }}", node),
        }
    }
}

impl PrettyPrint for AsyncConciseBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncConciseBody: {}", first, self)?;
        match self {
            AsyncConciseBody::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AsyncConciseBody::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AsyncConciseBody::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            AsyncConciseBody::Function(node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AsyncConciseBody: {}", first, self)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncConciseBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        match scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace) {
            Ok(after_curly) => {
                let (fb, after_fb) = AsyncFunctionBody::parse(parser, after_curly);
                let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Box::new(AsyncConciseBody::Function(fb)), after_rb))
            }
            Err(_) => {
                let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, true)?;
                Ok((Box::new(AsyncConciseBody::Expression(exp)), after_exp))
            }
        }
    }
}

// AsyncArrowBindingIdentifier[Yield] :
//      BindingIdentifier[?Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowBindingIdentifier(Box<BindingIdentifier>);

impl fmt::Display for AsyncArrowBindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncArrowBindingIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowBindingIdentifier: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncArrowBindingIdentifier {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (ident, after_ident) = BindingIdentifier::parse(parser, scanner, yield_flag, true)?;
        Ok((Box::new(AsyncArrowBindingIdentifier(ident)), after_ident))
    }
}

// CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CoverCallExpressionAndAsyncArrowHead {
    expression: Box<MemberExpression>,
    args: Box<Arguments>,
}

impl fmt::Display for CoverCallExpressionAndAsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.expression, self.args)
    }
}

impl PrettyPrint for CoverCallExpressionAndAsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverCallExpressionAndAsyncArrowHead: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverCallExpressionAndAsyncArrowHead: {}", first, self)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverCallExpressionAndAsyncArrowHead {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (expression, after_exp) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_exp, yield_flag, await_flag)?;
        Ok((Box::new(CoverCallExpressionAndAsyncArrowHead { expression, args }), after_args))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

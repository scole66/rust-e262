use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::declarations_and_variables::{BindingElement, BindingRestElement};
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// UniqueFormalParameters[Yield, Await] :
//      FormalParameters[?Yield, ?Await]
#[derive(Debug)]
pub struct UniqueFormalParameters {
    formals: Box<FormalParameters>,
}

impl fmt::Display for UniqueFormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.formals.fmt(f)
    }
}

impl PrettyPrint for UniqueFormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UniqueFormalParameters: {}", first, self)?;
        self.formals.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.formals.concise_with_leftpad(writer, pad, state)
    }
}

impl UniqueFormalParameters {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_fp = FormalParameters::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((fp, after_fp)) = pot_fp {
            return Ok(Some((Box::new(UniqueFormalParameters { formals: fp }), after_fp)));
        }
        Ok(None)
    }
}

// FormalParameters[Yield, Await] :
//      [empty]
//      FunctionRestParameter[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await] ,
//      FormalParameterList[?Yield, ?Await] , FunctionRestParameter[?Yield, ?Await]
#[derive(Debug)]
pub enum FormalParameters {
    Empty,
    Rest(Box<FunctionRestParameter>),
    List(Box<FormalParameterList>),
    ListComma(Box<FormalParameterList>),
    ListRest(Box<FormalParameterList>, Box<FunctionRestParameter>),
}

impl fmt::Display for FormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.fmt(f),
            FormalParameters::List(node) => node.fmt(f),
            FormalParameters::ListComma(node) => write!(f, "{} ,", node),
            FormalParameters::ListRest(list, rest) => write!(f, "{} , {}", list, rest),
        }
    }
}

impl PrettyPrint for FormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameters: {}", first, self)?;
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameters::List(node) | FormalParameters::ListComma(node) => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            FormalParameters::ListRest(list, rest) => {
                list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rest.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let header = |w: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(w, "{}FormalParameters: {}", first, self).and(Ok(successive))
        };
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::List(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::ListComma(node) => {
                let successive = header(writer)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::Final)
            }
            FormalParameters::ListRest(list, node) => {
                let successive = header(writer)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl FormalParameters {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_fpl = FormalParameterList::parse(parser, scanner, yield_flag, await_flag)?;
        let (fpl, after_fpl) = match pot_fpl {
            None => (None, scanner),
            Some((f, s)) => (Some(f), s),
        };
        let (pot_comma, after_pot) = scan_token(&after_fpl, parser.source, ScanGoal::InputElementDiv);
        let (has_comma, after_comma) = match pot_comma {
            Token::Punctuator(Punctuator::Comma) => (true, after_pot),
            _ => (false, after_fpl),
        };
        let pot_frp = FunctionRestParameter::parse(parser, after_comma, yield_flag, await_flag)?;
        let (frp, after_frp) = match pot_frp {
            None => (None, after_comma),
            Some((f, s)) => (Some(f), s),
        };
        match (fpl, has_comma, frp) {
            (Some(pl), true, Some(rp)) => Ok(Some((Box::new(FormalParameters::ListRest(pl, rp)), after_frp))),
            (Some(pl), true, None) => Ok(Some((Box::new(FormalParameters::ListComma(pl)), after_comma))),
            (Some(pl), false, _) => Ok(Some((Box::new(FormalParameters::List(pl)), after_fpl))),
            (None, false, Some(rp)) => Ok(Some((Box::new(FormalParameters::Rest(rp)), after_frp))),
            (None, false, None) | (None, true, _) => Ok(Some((Box::new(FormalParameters::Empty), scanner))),
        }
    }
}

// FormalParameterList[Yield, Await] :
//      FormalParameter[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await] , FormalParameter[?Yield, ?Await]
#[derive(Debug)]
pub enum FormalParameterList {
    Item(Box<FormalParameter>),
    List(Box<FormalParameterList>, Box<FormalParameter>),
}

impl fmt::Display for FormalParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormalParameterList::Item(node) => node.fmt(f),
            FormalParameterList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for FormalParameterList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameterList: {}", first, self)?;
        match self {
            FormalParameterList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameterList::List(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            FormalParameterList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameterList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}FormalParameterList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl FormalParameterList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_fp = FormalParameter::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((fp, after_fp)) = pot_fp {
            let mut current = Box::new(FormalParameterList::Item(fp));
            let mut current_scanner = after_fp;
            loop {
                let (comma, after_comma) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
                match comma {
                    Token::Punctuator(Punctuator::Comma) => {
                        let pot_next = FormalParameter::parse(parser, after_comma, yield_flag, await_flag)?;
                        match pot_next {
                            Some((next, after_next)) => {
                                current = Box::new(FormalParameterList::List(current, next));
                                current_scanner = after_next;
                            }
                            None => {
                                break;
                            }
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }
            return Ok(Some((current, current_scanner)));
        }
        Ok(None)
    }
}

// FunctionRestParameter[Yield, Await] :
//      BindingRestElement[?Yield, ?Await]
#[derive(Debug)]
pub struct FunctionRestParameter {
    element: Box<BindingRestElement>,
}

impl fmt::Display for FunctionRestParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.element.fmt(f)
    }
}

impl PrettyPrint for FunctionRestParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionRestParameter: {}", first, self)?;
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.element.concise_with_leftpad(writer, pad, state)
    }
}

impl FunctionRestParameter {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bre = BindingRestElement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bre, after_bre)) = pot_bre {
            return Ok(Some((Box::new(FunctionRestParameter { element: bre }), after_bre)));
        }
        Ok(None)
    }
}

// FormalParameter[Yield, Await] :
//      BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub struct FormalParameter {
    element: Box<BindingElement>,
}

impl fmt::Display for FormalParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.element.fmt(f)
    }
}

impl PrettyPrint for FormalParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameter: {}", first, self)?;
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.element.concise_with_leftpad(writer, pad, state)
    }
}

impl FormalParameter {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_be = BindingElement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((be, after_be)) = pot_be {
            return Ok(Some((Box::new(FormalParameter { element: be }), after_be)));
        }
        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

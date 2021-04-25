use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

// Script :
//      ScriptBody opt
#[derive(Debug)]
pub struct Script(Option<Rc<ScriptBody>>);

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            None => Ok(()),
            Some(n) => n.fmt(f),
        }
    }
}

impl PrettyPrint for Script {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Script: {}", first, self)?;
        if let Some(body) = &self.0 {
            body.pprint_with_leftpad(writer, &successive, Spot::Final)
        } else {
            Ok(())
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.0 {
            None => {
                let (first, _) = prettypad(pad, state);
                writeln!(writer, "{}Script:", first)
            }
            Some(body) => body.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl Script {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match ScriptBody::parse(parser, scanner) {
            Ok((n, s)) => Ok((Rc::new(Script(Some(n))), s)),
            Err(_) => Ok((Rc::new(Script(None)), scanner)),
        }
    }
}

// ScriptBody :
//      StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
pub struct ScriptBody(Rc<StatementList>);

impl fmt::Display for ScriptBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for ScriptBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ScriptBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl ScriptBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (sl, after_sl) = StatementList::parse(parser, scanner, false, false, false)?;
        Ok((Rc::new(ScriptBody(sl)), after_sl))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // SCRIPT
    #[test]
    fn script_test_01() {
        let (node, scanner) = check(Script::parse(&mut newparser("let a=1;"), Scanner::new()));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "Script: let a = 1 ;", vec!["ScriptBody: let a = 1 ;"]);
        concise_check(&*node, "LexicalDeclaration: let a = 1 ;", vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn script_test_02() {
        let (node, scanner) = check(Script::parse(&mut newparser(""), Scanner::new()));
        chk_scan(&scanner, 0);
        pretty_check(&*node, "Script: ", vec![]);
        concise_check(&*node, "Script:", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn script_test_prettyerrors_1() {
        let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn script_test_prettyerrors_2() {
        let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn script_test_conciseerrors_1() {
        let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn script_test_conciseerrors_2() {
        let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }

    // SCRIPT BODY
    #[test]
    fn script_body_test_01() {
        let (node, scanner) = check(ScriptBody::parse(&mut newparser("let a=1;"), Scanner::new()));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "ScriptBody: let a = 1 ;", vec!["StatementList: let a = 1 ;"]);
        concise_check(&*node, "LexicalDeclaration: let a = 1 ;", vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn script_body_test_err_01() {
        check_err(ScriptBody::parse(&mut newparser(""), Scanner::new()), "Declaration or Statement expected", 1, 1);
    }
    #[test]
    fn script_body_test_prettyerrors_1() {
        let (item, _) = Script::parse(&mut newparser(";"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn script_body_test_conciseerrors_1() {
        let (item, _) = Script::parse(&mut newparser(";"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
}

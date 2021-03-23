use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// BreakStatement[Yield, Await] :
//      break ;
//      break [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum BreakStatement {
    Bare,
    Labelled(Rc<LabelIdentifier>),
}

impl fmt::Display for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BreakStatement::Bare => write!(f, "break ;"),
            BreakStatement::Labelled(label) => write!(f, "break {} ;", label),
        }
    }
}

impl PrettyPrint for BreakStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        match self {
            BreakStatement::Bare => Ok(()),
            BreakStatement::Labelled(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        pprint_token(writer, "break", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let BreakStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl BreakStatement {
    // no cache needed
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_break = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Break)?;
        scan_for_punct(after_break, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon).map(|after_semi| (Rc::new(BreakStatement::Bare), after_semi)).otherwise(|| {
            no_line_terminator(after_break, parser.source)?;
            let (li, after_li) = LabelIdentifier::parse(parser, after_break, yield_flag, await_flag)?;
            let after_semi = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
            Ok((Rc::new(BreakStatement::Labelled(li)), after_semi))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // BREAK STATEMENT
    #[test]
    fn break_statement_test_01() {
        let (node, scanner) = check(BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "BreakStatement: break ;", vec![]);
        concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn break_statement_test_02() {
        let (node, scanner) = check(BreakStatement::parse(&mut newparser("break a;"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "BreakStatement: break a ;", vec!["LabelIdentifier: a"]);
        concise_check(&*node, "BreakStatement: break a ;", vec!["Keyword: break", "IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn break_statement_test_err_01() {
        check_err(BreakStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘break’ expected", 1, 1);
    }
    #[test]
    fn break_statement_test_err_02() {
        check_err(BreakStatement::parse(&mut newparser("break\n"), Scanner::new(), false, false), "‘;’ expected", 1, 6);
    }
    #[test]
    fn break_statement_test_err_03() {
        check_err(BreakStatement::parse(&mut newparser("break a"), Scanner::new(), false, false), "‘;’ expected", 1, 8);
    }
    #[test]
    fn break_statement_test_err_04() {
        check_err(BreakStatement::parse(&mut newparser("break for"), Scanner::new(), false, false), "‘;’ expected", 1, 6);
    }
    #[test]
    fn break_statement_test_prettyerrors_1() {
        let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn break_statement_test_prettyerrors_2() {
        let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn break_statement_test_conciseerrors_1() {
        let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn break_statement_test_conciseerrors_2() {
        let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
}

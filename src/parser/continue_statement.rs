use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ContinueStatement[Yield, Await] :
//      continue ;
//      continue [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum ContinueStatement {
    Bare,
    Labelled(Rc<LabelIdentifier>),
}

impl fmt::Display for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ContinueStatement::Bare => write!(f, "continue ;"),
            ContinueStatement::Labelled(label) => write!(f, "continue {} ;", label),
        }
    }
}

impl PrettyPrint for ContinueStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ContinueStatement: {}", first, self)?;
        match self {
            ContinueStatement::Bare => Ok(()),
            ContinueStatement::Labelled(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ContinueStatement: {}", first, self)?;
        pprint_token(writer, "continue", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let ContinueStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ContinueStatement {
    // no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_cont = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Continue)?;
        scan_for_auto_semi(after_cont, parser.source, ScanGoal::InputElementDiv).map(|after_semi| (Rc::new(ContinueStatement::Bare), after_semi)).otherwise(|| {
            let (li, after_li) = LabelIdentifier::parse(parser, after_cont, yield_flag, await_flag)?;
            let after_semi = scan_for_auto_semi(after_li, parser.source, ScanGoal::InputElementDiv)?;
            Ok((Rc::new(ContinueStatement::Labelled(li)), after_semi))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // CONTINUE STATEMENT
    #[test]
    fn continue_statement_test_01() {
        let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
        concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn continue_statement_test_02() {
        let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue a;"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        pretty_check(&*node, "ContinueStatement: continue a ;", vec!["LabelIdentifier: a"]);
        concise_check(&*node, "ContinueStatement: continue a ;", vec!["Keyword: continue", "IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn continue_statement_test_03() {
        let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
        concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn continue_statement_test_04() {
        let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue label"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "ContinueStatement: continue label ;", vec!["LabelIdentifier: label"]);
        concise_check(&*node, "ContinueStatement: continue label ;", vec!["Keyword: continue", "IdentifierName: label", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn continue_statement_test_05() {
        let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue\nlabel"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
        concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn continue_statement_test_err_01() {
        check_err(ContinueStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘continue’ expected", 1, 1);
    }
    #[test]
    fn continue_statement_test_err_02() {
        check_err(ContinueStatement::parse(&mut newparser("continue for"), Scanner::new(), false, false), "‘;’ expected", 1, 9);
    }
    #[test]
    fn continue_statement_test_err_03() {
        check_err(ContinueStatement::parse(&mut newparser("continue a for"), Scanner::new(), false, false), "‘;’ expected", 1, 11);
    }
    #[test]
    fn continue_statement_test_prettyerrors_1() {
        let (item, _) = ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn continue_statement_test_prettyerrors_2() {
        let (item, _) = ContinueStatement::parse(&mut newparser("continue label;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn continue_statement_test_conciseerrors_1() {
        let (item, _) = ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn continue_statement_test_conciseerrors_2() {
        let (item, _) = ContinueStatement::parse(&mut newparser("continue label;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
}

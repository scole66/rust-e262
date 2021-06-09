use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ReturnStatement[Yield, Await] :
//      return ;
//      return [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum ReturnStatement {
    Bare,
    Expression(Rc<Expression>),
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReturnStatement::Bare => write!(f, "return ;"),
            ReturnStatement::Expression(node) => write!(f, "return {} ;", node),
        }
    }
}

impl PrettyPrint for ReturnStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ReturnStatement: {}", first, self)?;
        match self {
            ReturnStatement::Bare => Ok(()),
            ReturnStatement::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ReturnStatement: {}", first, self)?;
        pprint_token(writer, "return", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let ReturnStatement::Expression(exp) = self {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ReturnStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_ret = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Return)?;
        scan_for_auto_semi(after_ret, parser.source, ScanGoal::InputElementRegExp).map(|after_semi| (Rc::new(ReturnStatement::Bare), after_semi)).otherwise(|| {
            let (exp, after_exp) = Expression::parse(parser, after_ret, true, yield_flag, await_flag)?;
            let after_semi = scan_for_auto_semi(after_exp, parser.source, ScanGoal::InputElementDiv)?;
            Ok((Rc::new(ReturnStatement::Expression(exp)), after_semi))
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ReturnStatement::Bare => false,
            ReturnStatement::Expression(node) => node.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // RETURN STATEMENT
    #[test]
    fn return_statement_test_01() {
        let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        pretty_check(&*node, "ReturnStatement: return ;", vec![]);
        concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn return_statement_test_02() {
        let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null;"), Scanner::new(), false, false));
        chk_scan(&scanner, 12);
        pretty_check(&*node, "ReturnStatement: return null ;", vec!["Expression: null"]);
        concise_check(&*node, "ReturnStatement: return null ;", vec!["Keyword: return", "Keyword: null", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn return_statement_test_asi_01() {
        let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "ReturnStatement: return ;", vec![]);
        concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn return_statement_test_asi_02() {
        let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        pretty_check(&*node, "ReturnStatement: return null ;", vec!["Expression: null"]);
        concise_check(&*node, "ReturnStatement: return null ;", vec!["Keyword: return", "Keyword: null", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn return_statement_test_err_01() {
        check_err(ReturnStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘return’ expected", 1, 1);
    }
    #[test]
    fn return_statement_test_err_02() {
        check_err(ReturnStatement::parse(&mut newparser("return ="), Scanner::new(), false, false), "‘;’ expected", 1, 7);
    }
    #[test]
    fn return_statement_test_err_03() {
        check_err(ReturnStatement::parse(&mut newparser("return 0 for"), Scanner::new(), false, false), "‘;’ expected", 1, 9);
    }
    #[test]
    fn return_statement_test_prettyerrors_1() {
        let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn return_statement_test_prettyerrors_2() {
        let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn return_statement_test_conciseerrors_1() {
        let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn return_statement_test_conciseerrors_2() {
        let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
}

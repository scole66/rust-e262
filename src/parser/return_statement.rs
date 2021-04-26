use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
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
    // Given ‘return’
    // See if we can parse ‘[no LineTerminator here] Expression ;’
    fn parse_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (_, after_next) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        // The following check is broken for literals which span more than one line (strings can do this). Need to come up with a better way.
        if scanner.line == after_next.line {
            let (exp, after_exp) = Expression::parse(parser, scanner, true, yield_flag, await_flag)?;
            let after_semi = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
            Ok((Rc::new(ReturnStatement::Expression(exp)), after_semi))
        } else {
            Err(ParseError::new("Expression expected", scanner.line, scanner.column))
        }
    }

    // Given ‘return’
    // See if we can parse ‘;’
    fn parse_semi(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_semi = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Rc::new(ReturnStatement::Bare), after_semi))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_ret = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Return)?;
        Err(ParseError::new("‘;’ or an Expression expected", after_ret.line, after_ret.column))
            .otherwise(|| Self::parse_exp(parser, after_ret, yield_flag, await_flag))
            .otherwise(|| Self::parse_semi(parser, after_ret))
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
    fn return_statement_test_err_01() {
        check_err(ReturnStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘return’ expected", 1, 1);
    }
    #[test]
    fn return_statement_test_err_02() {
        check_err(ReturnStatement::parse(&mut newparser("return"), Scanner::new(), false, false), "‘;’ or an Expression expected", 1, 7);
    }
    #[test]
    fn return_statement_test_err_03() {
        check_err(ReturnStatement::parse(&mut newparser("return 0"), Scanner::new(), false, false), "‘;’ expected", 1, 9);
    }
    #[test]
    fn return_statement_test_err_04() {
        check_err(ReturnStatement::parse(&mut newparser("return\n0;"), Scanner::new(), false, false), "‘;’ or an Expression expected", 1, 7);
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

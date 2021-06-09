use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// IfStatement[Yield, Await, Return] :
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] else Statement[?Yield, ?Await, ?Return]
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] [lookahead ≠ else]
#[derive(Debug)]
pub enum IfStatement {
    WithElse(Rc<Expression>, Rc<Statement>, Rc<Statement>),
    WithoutElse(Rc<Expression>, Rc<Statement>),
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IfStatement::WithElse(e, s1, s2) => write!(f, "if ( {} ) {} else {}", e, s1, s2),
            IfStatement::WithoutElse(e, s1) => write!(f, "if ( {} ) {}", e, s1),
        }
    }
}

impl PrettyPrint for IfStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        match self {
            IfStatement::WithoutElse(e, s1) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s2.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        pprint_token(writer, "if", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let condition = |writer: &mut T, exp: &Expression| {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal).and_then(|_| pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal))
        };
        match self {
            IfStatement::WithoutElse(e, s1) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "else", TokenType::Keyword, &successive, Spot::NotFinal)?;
                s2.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IfStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_lead = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::If)?;
        let after_open = scan_for_punct(after_lead, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt1, after_stmt1) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        match scan_for_keyword(after_stmt1, parser.source, ScanGoal::InputElementRegExp, Keyword::Else) {
            Err(_) => Ok((Rc::new(IfStatement::WithoutElse(exp, stmt1)), after_stmt1)),
            Ok(after_else) => {
                let (stmt2, after_stmt2) = Statement::parse(parser, after_else, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IfStatement::WithElse(exp, stmt1, stmt2)), after_stmt2))
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            IfStatement::WithElse(_, s1, s2) => {
                let mut names = s1.var_declared_names();
                names.extend(s2.var_declared_names());
                names
            }
            IfStatement::WithoutElse(_, s1) => s1.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            IfStatement::WithElse(_, s1, s2) => s1.contains_undefined_break_target(label_set) || s2.contains_undefined_break_target(label_set),
            IfStatement::WithoutElse(_, s1) => s1.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            IfStatement::WithElse(e, s1, s2) => e.contains(kind) || s1.contains(kind) || s2.contains(kind),
            IfStatement::WithoutElse(e, s1) => e.contains(kind) || s1.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // IF STATEMENT
    #[test]
    fn if_statement_test_01() {
        let (node, scanner) = check(IfStatement::parse(&mut newparser("if (true) { a; }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 16);
        pretty_check(&*node, "IfStatement: if ( true ) { a ; }", vec!["Expression: true", "Statement: { a ; }"]);
        concise_check(&*node, "IfStatement: if ( true ) { a ; }", vec!["Keyword: if", "Punctuator: (", "Keyword: true", "Punctuator: )", "Block: { a ; }"]);
        format!("{:?}", node);
    }
    #[test]
    fn if_statement_test_02() {
        let (node, scanner) = check(IfStatement::parse(&mut newparser("if (0) { a; } else { b; }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 25);
        pretty_check(&*node, "IfStatement: if ( 0 ) { a ; } else { b ; }", vec!["Expression: 0", "Statement: { a ; }", "Statement: { b ; }"]);
        concise_check(
            &*node,
            "IfStatement: if ( 0 ) { a ; } else { b ; }",
            vec!["Keyword: if", "Punctuator: (", "Numeric: 0", "Punctuator: )", "Block: { a ; }", "Keyword: else", "Block: { b ; }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn if_statement_test_err_01() {
        check_err(IfStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘if’ expected", 1, 1);
    }
    #[test]
    fn if_statement_test_err_02() {
        check_err(IfStatement::parse(&mut newparser("if"), Scanner::new(), false, false, true), "‘(’ expected", 1, 3);
    }
    #[test]
    fn if_statement_test_err_03() {
        check_err(IfStatement::parse(&mut newparser("if ("), Scanner::new(), false, false, true), "Expression expected", 1, 5);
    }
    #[test]
    fn if_statement_test_err_04() {
        check_err(IfStatement::parse(&mut newparser("if (0"), Scanner::new(), false, false, true), "‘)’ expected", 1, 6);
    }
    #[test]
    fn if_statement_test_err_05() {
        check_err(IfStatement::parse(&mut newparser("if (0)"), Scanner::new(), false, false, true), "Statement expected", 1, 7);
    }
    #[test]
    fn if_statement_test_err_06() {
        check_err(IfStatement::parse(&mut newparser("if (0) a; else"), Scanner::new(), false, false, true), "Statement expected", 1, 15);
    }
    #[test]
    fn if_statement_test_prettyerrors_1() {
        let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn if_statement_test_prettyerrors_2() {
        let (item, _) = IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn if_statement_test_conciseerrors_1() {
        let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn if_statement_test_conciseerrors_2() {
        let (item, _) = IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
}

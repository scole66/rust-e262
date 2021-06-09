use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// WithStatement[Yield, Await, Return] :
//      with ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct WithStatement {
    expression: Rc<Expression>,
    statement: Rc<Statement>,
}

impl fmt::Display for WithStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "with ( {} ) {}", self.expression, self.statement)
    }
}

impl PrettyPrint for WithStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.statement.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        pprint_token(writer, "with", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.statement.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl WithStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_width = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::With)?;
        let after_open = scan_for_punct(after_width, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(WithStatement { expression: exp, statement: stmt }), after_stmt))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.statement.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.statement.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.statement.contains(kind)
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // WITH STATEMENT
    #[test]
    fn with_statement_test_01() {
        let (node, scanner) = check(WithStatement::parse(&mut newparser("with ( x in obj ) { x.used = true; }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 36);
        format!("{:?}", node);
        pretty_check(&*node, "WithStatement: with ( x in obj ) { x . used = true ; }", vec!["Expression: x in obj", "Statement: { x . used = true ; }"]);
        concise_check(
            &*node,
            "WithStatement: with ( x in obj ) { x . used = true ; }",
            vec!["Keyword: with", "Punctuator: (", "RelationalExpression: x in obj", "Punctuator: )", "Block: { x . used = true ; }"],
        );
    }
    #[test]
    fn with_statement_test_err_01() {
        check_err(WithStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘with’ expected", 1, 1);
    }
    #[test]
    fn with_statement_test_err_02() {
        check_err(WithStatement::parse(&mut newparser("with"), Scanner::new(), false, false, true), "‘(’ expected", 1, 5);
    }
    #[test]
    fn with_statement_test_err_03() {
        check_err(WithStatement::parse(&mut newparser("with("), Scanner::new(), false, false, true), "Expression expected", 1, 6);
    }
    #[test]
    fn with_statement_test_err_04() {
        check_err(WithStatement::parse(&mut newparser("with(a"), Scanner::new(), false, false, true), "‘)’ expected", 1, 7);
    }
    #[test]
    fn with_statement_test_err_05() {
        check_err(WithStatement::parse(&mut newparser("with(a)"), Scanner::new(), false, false, true), "Statement expected", 1, 8);
    }
    #[test]
    fn with_statement_prettycheck_1() {
        let (item, _) = WithStatement::parse(&mut newparser("with (returns_an_object()) { obj = 12; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn with_statement_concisecheck_1() {
        let (item, _) = WithStatement::parse(&mut newparser("with (returns_an_object()) { obj = 12; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
}

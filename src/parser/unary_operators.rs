use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::async_function_definitions::AwaitExpression;
use super::scanner::Scanner;
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum UnaryExpression {
    UpdateExpression(Box<UpdateExpression>),
    Delete(Box<UnaryExpression>),
    Void(Box<UnaryExpression>),
    Typeof(Box<UnaryExpression>),
    NoOp(Box<UnaryExpression>),
    Negate(Box<UnaryExpression>),
    Complement(Box<UnaryExpression>),
    Not(Box<UnaryExpression>),
    Await(Box<AwaitExpression>),
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryExpression::UpdateExpression(boxed) => write!(f, "{}", boxed),
            UnaryExpression::Delete(boxed) => write!(f, "delete {}", boxed),
            UnaryExpression::Void(boxed) => write!(f, "void {}", boxed),
            UnaryExpression::Typeof(boxed) => write!(f, "typeof {}", boxed),
            UnaryExpression::NoOp(boxed) => write!(f, "+ {}", boxed),
            UnaryExpression::Negate(boxed) => write!(f, "- {}", boxed),
            UnaryExpression::Complement(boxed) => write!(f, "~ {}", boxed),
            UnaryExpression::Not(boxed) => write!(f, "! {}", boxed),
            UnaryExpression::Await(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for UnaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UnaryExpression: {}", first, self)?;
        match &self {
            UnaryExpression::UpdateExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Delete(boxed)
            | UnaryExpression::Void(boxed)
            | UnaryExpression::Typeof(boxed)
            | UnaryExpression::NoOp(boxed)
            | UnaryExpression::Negate(boxed)
            | UnaryExpression::Complement(boxed)
            | UnaryExpression::Not(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UnaryExpression::Await(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl UnaryExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_token) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        let mut unary_helper =
            |f: fn(Box<Self>) -> Self| UnaryExpression::parse(parser, after_token, yield_flag, await_flag).and_then(|opt| opt.map_or(Ok(None), |(boxed, after)| Ok(Some((Box::new(f(boxed)), after)))));
        match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Delete) => unary_helper(|boxed| UnaryExpression::Delete(boxed)),
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Void) => unary_helper(|boxed| UnaryExpression::Void(boxed)),
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Typeof) => unary_helper(|boxed| UnaryExpression::Typeof(boxed)),
            scanner::Token::Plus => unary_helper(|boxed| UnaryExpression::NoOp(boxed)),
            scanner::Token::Minus => unary_helper(|boxed| UnaryExpression::Negate(boxed)),
            scanner::Token::Tilde => unary_helper(|boxed| UnaryExpression::Complement(boxed)),
            scanner::Token::Bang => unary_helper(|boxed| UnaryExpression::Not(boxed)),
            _ => {
                let mut production: Option<(Box<Self>, Scanner)> = None;
                if await_flag {
                    let pot_ae = AwaitExpression::parse(parser, scanner, yield_flag)?;
                    match pot_ae {
                        Some((boxed, scanner)) => {
                            production = Some((Box::new(UnaryExpression::Await(boxed)), scanner));
                        }
                        None => {}
                    }
                }
                if production.is_none() {
                    production = {
                        let pot_ue = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
                        match pot_ue {
                            Some((boxed, scanner)) => Some((Box::new(UnaryExpression::UpdateExpression(boxed)), scanner)),
                            None => None,
                        }
                    };
                }
                Ok(production)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;

    // UNARY EXPRESSION
    #[test]
    fn unary_expression_test_update_expression() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("900"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UnaryExpression::UpdateExpression(_)));
        pretty_check(&*ue, "UnaryExpression: Number(900.0)", vec!["UpdateExpression: Number(900.0)"]);
    }
    #[test]
    fn unary_expression_test_delete() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("delete bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Delete(_)));
        pretty_check(&*ue, "UnaryExpression: delete bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_void() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("void bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(*ue, UnaryExpression::Void(_)));
        pretty_check(&*ue, "UnaryExpression: void bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_typeof() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("typeof bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Typeof(_)));
        pretty_check(&*ue, "UnaryExpression: typeof bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_numberify() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("+bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::NoOp(_)));
        pretty_check(&*ue, "UnaryExpression: + bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_negate() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("-bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Negate(_)));
        pretty_check(&*ue, "UnaryExpression: - bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_complement() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("~bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Complement(_)));
        pretty_check(&*ue, "UnaryExpression: ~ bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_not() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("!bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Not(_)));
        pretty_check(&*ue, "UnaryExpression: ! bob", vec!["UnaryExpression: bob"]);
    }
    #[test]
    fn unary_expression_test_await() {
        //let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
        //chk_scan(&scanner, 9);
        //assert!(matches!(*ue, UnaryExpression::Await(_)));

        // Use the prior lines when AwaitExpression gets implemented.
        check_none(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
    }
    #[test]
    fn unary_expression_test_nomatch() {
        check_none(UnaryExpression::parse(&mut newparser(""), Scanner::new(), false, false));
    }
    #[test]
    fn unary_expression_test_incomplete() {
        check_none(UnaryExpression::parse(&mut newparser("delete"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("void"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("typeof"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("+"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("-"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("~"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("!"), Scanner::new(), false, false));
    }
}

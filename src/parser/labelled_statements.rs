use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionDeclaration;
use super::identifiers::LabelIdentifier;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LabelledStatement[Yield, Await, Return] :
//      LabelIdentifier[?Yield, ?Await] : LabelledItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct LabelledStatement {
    identifier: Rc<LabelIdentifier>,
    item: Rc<LabelledItem>,
}

impl fmt::Display for LabelledStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.identifier, self.item)
    }
}

impl PrettyPrint for LabelledStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.item.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.item.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl LabelledStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (identifier, after_li) = LabelIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let after_colon = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (item, after_item) = LabelledItem::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(LabelledStatement { identifier, item }), after_item))
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        self.item.top_level_var_declared_names()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.item.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let label = self.identifier.string_value();
        let mut new_label_set: Vec<JSString> = Vec::new();
        new_label_set.extend_from_slice(label_set);
        new_label_set.push(label);
        self.item.contains_undefined_break_target(&new_label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.identifier.contains(kind) || self.item.contains(kind)
    }
}

// LabelledItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      FunctionDeclaration[?Yield, ?Await, ~Default]
#[derive(Debug)]
pub enum LabelledItem {
    Statement(Rc<Statement>),
    Function(Rc<FunctionDeclaration>),
}

impl fmt::Display for LabelledItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelledItem::Statement(node) => node.fmt(f),
            LabelledItem::Function(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for LabelledItem {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledItem: {}", first, self)?;
        match self {
            LabelledItem::Statement(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LabelledItem::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LabelledItem::Statement(node) => node.concise_with_leftpad(writer, pad, state),
            LabelledItem::Function(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl LabelledItem {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("LabelledItem expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (stmt, after_stmt) = Statement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(LabelledItem::Statement(stmt)), after_stmt))
            })
            .otherwise(|| {
                let (fcn, after_fcn) = FunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
                Ok((Rc::new(LabelledItem::Function(fcn)), after_fcn))
            })
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            LabelledItem::Statement(node) => match &**node {
                Statement::Labelled(stmt) => stmt.top_level_var_declared_names(),
                _ => node.var_declared_names(),
            },
            LabelledItem::Function(node) => node.bound_names(),
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            LabelledItem::Statement(node) => node.var_declared_names(),
            LabelledItem::Function(..) => vec![],
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains_undefined_break_target(label_set),
            LabelledItem::Function(..) => false,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains(kind),
            LabelledItem::Function(node) => node.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // LABELLED STATEMENT
    #[test]
    fn labelled_statement_test_01() {
        let (node, scanner) = check(LabelledStatement::parse(&mut newparser("blue: orange;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 13);
        pretty_check(&*node, "LabelledStatement: blue : orange ;", vec!["LabelIdentifier: blue", "LabelledItem: orange ;"]);
        concise_check(&*node, "LabelledStatement: blue : orange ;", vec!["IdentifierName: blue", "Punctuator: :", "ExpressionStatement: orange ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn labelled_statement_test_err_01() {
        check_err(LabelledStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "Not an identifier", 1, 1);
    }
    #[test]
    fn labelled_statement_test_err_02() {
        check_err(LabelledStatement::parse(&mut newparser("a"), Scanner::new(), false, false, true), "‘:’ expected", 1, 2);
    }
    #[test]
    fn labelled_statement_test_err_03() {
        check_err(LabelledStatement::parse(&mut newparser("a:"), Scanner::new(), false, false, true), "LabelledItem expected", 1, 3);
    }
    #[test]
    fn labelled_statement_test_prettyerrors_1() {
        let (item, _) = LabelledStatement::parse(&mut newparser("i:b;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn labelled_statement_test_conciseerrors_1() {
        let (item, _) = LabelledStatement::parse(&mut newparser("i:b;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // LABELLED ITEM
    #[test]
    fn labelled_item_test_01() {
        let (node, scanner) = check(LabelledItem::parse(&mut newparser("orange;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 7);
        pretty_check(&*node, "LabelledItem: orange ;", vec!["Statement: orange ;"]);
        concise_check(&*node, "ExpressionStatement: orange ;", vec!["IdentifierName: orange", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn labelled_item_test_02() {
        let (node, scanner) = check(LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "LabelledItem: function a (  ) {  }", vec!["FunctionDeclaration: function a (  ) {  }"]);
        concise_check(
            &*node,
            "FunctionDeclaration: function a (  ) {  }",
            vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn labelled_item_test_err_01() {
        check_err(LabelledItem::parse(&mut newparser(""), Scanner::new(), false, false, true), "LabelledItem expected", 1, 1);
    }
    #[test]
    fn labelled_item_test_prettyerrors_1() {
        let (item, _) = LabelledItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn labelled_item_test_prettyerrors_2() {
        let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn labelled_item_test_conciseerrors_1() {
        let (item, _) = LabelledItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn labelled_item_test_conciseerrors_2() {
        let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
}

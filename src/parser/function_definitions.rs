use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::StatementList;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::FormalParameters;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// FunctionDeclaration[Yield, Await, Default] :
//      function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
//      [+Default] function ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<FunctionBody>,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for FunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl FunctionDeclaration {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (bi, after_bi) = match BindingIdentifier::parse(parser, after_func, yield_flag, await_flag) {
            Ok((node, scan)) => Ok((Some(node), scan)),
            Err(e) => {
                if default_flag {
                    Ok((None, after_func))
                } else {
                    Err(e)
                }
            }
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (fp, after_fp) = FormalParameters::parse(parser, after_lp, false, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (fb, after_fb) = FunctionBody::parse(parser, after_lb, false, false);
        let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(FunctionDeclaration { ident: bi, params: fp, body: fb }), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitDefaultKey { scanner, yield_flag, await_flag, default_flag };
        match parser.function_declaration_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, default_flag);
                parser.function_declaration_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// FunctionExpression :
//      function BindingIdentifier[~Yield, ~Await]opt ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionExpression {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<FunctionBody>,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for FunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for FunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl FunctionExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (bi, after_bi) = match BindingIdentifier::parse(parser, after_func, false, false) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_func),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (fp, after_fp) = FormalParameters::parse(parser, after_lp, false, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (fb, after_fb) = FunctionBody::parse(parser, after_lb, false, false);
        let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(FunctionExpression { ident: bi, params: fp, body: fb }), after_rb))
    }
}

// FunctionBody[Yield, Await] :
//      FunctionStatementList[?Yield, ?Await]
#[derive(Debug)]
pub struct FunctionBody {
    statements: Rc<FunctionStatementList>,
}

impl fmt::Display for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statements.fmt(f)
    }
}

impl PrettyPrint for FunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionBody: {}", first, self)?;
        self.statements.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.statements.concise_with_leftpad(writer, pad, state)
    }
}

impl FunctionBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        // Can never return an error
        let (fsl, after_fsl) = FunctionStatementList::parse(parser, scanner, yield_flag, await_flag);
        (Rc::new(FunctionBody { statements: fsl }), after_fsl)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.function_body_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.function_body_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// FunctionStatementList[Yield, Await] :
//      StatementList[?Yield, ?Await, +Return]opt
#[derive(Debug)]
pub struct FunctionStatementList {
    statements: Option<Rc<StatementList>>,
}

impl fmt::Display for FunctionStatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.statements {
            None => Ok(()),
            Some(s) => s.fmt(f),
        }
    }
}

impl PrettyPrint for FunctionStatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionStatementList: {}", first, self)?;
        match &self.statements {
            None => Ok(()),
            Some(s) => s.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.statements {
            None => Ok(()),
            Some(s) => s.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl FunctionStatementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        // Can never return an error.
        let (stmts, after_stmts) = match StatementList::parse(parser, scanner, yield_flag, await_flag, true) {
            Err(_) => (None, scanner),
            Ok((st, s)) => (Some(st), s),
        };
        (Rc::new(FunctionStatementList { statements: stmts }), after_stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // FUNCTION DECLARATION
    #[test]
    fn function_declaration_test_01() {
        let (node, scanner) = check(FunctionDeclaration::parse(&mut newparser("function bob(a,b) { return foo(a+b); }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 38);
        pretty_check(
            &*node,
            "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
            vec!["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
            vec![
                "Keyword: function",
                "IdentifierName: bob",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
    }
    #[test]
    fn function_declaration_test_02() {
        let (node, scanner) = check(FunctionDeclaration::parse(&mut newparser("function (z) {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "FunctionDeclaration: function ( z ) {  }", vec!["FormalParameters: z", "FunctionBody: "]);
        concise_check(&*node, "FunctionDeclaration: function ( z ) {  }", vec!["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn function_declaration_test_err_01() {
        check_err(FunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘function’ expected", 1, 1);
    }
    #[test]
    fn function_declaration_test_err_02() {
        check_err(FunctionDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true), "‘(’ expected", 1, 9);
    }
    #[test]
    fn function_declaration_test_err_03() {
        check_err(FunctionDeclaration::parse(&mut newparser("function (z)"), Scanner::new(), false, false, false), "Not an identifier", 1, 9);
    }
    #[test]
    fn function_declaration_test_err_04() {
        check_err(FunctionDeclaration::parse(&mut newparser("function foo"), Scanner::new(), false, false, true), "‘(’ expected", 1, 13);
    }
    #[test]
    fn function_declaration_test_err_05() {
        check_err(FunctionDeclaration::parse(&mut newparser("function foo("), Scanner::new(), false, false, true), "‘)’ expected", 1, 14);
    }
    #[test]
    fn function_declaration_test_err_06() {
        check_err(FunctionDeclaration::parse(&mut newparser("function foo()"), Scanner::new(), false, false, true), "‘{’ expected", 1, 15);
    }
    #[test]
    fn function_declaration_test_err_07() {
        check_err(FunctionDeclaration::parse(&mut newparser("function foo(){"), Scanner::new(), false, false, true), "‘}’ expected", 1, 16);
    }
    #[test]
    fn function_declaration_test_prettyerrors_1() {
        let (item, _) = FunctionDeclaration::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_declaration_test_prettyerrors_2() {
        let (item, _) = FunctionDeclaration::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_declaration_test_conciseerrors_1() {
        let (item, _) = FunctionDeclaration::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn function_declaration_test_conciseerrors_2() {
        let (item, _) = FunctionDeclaration::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn function_declaration_test_cache_01() {
        let mut parser = newparser("function f() {}");
        let (node, scanner) = check(FunctionDeclaration::parse(&mut parser, Scanner::new(), false, false, false));
        let (node2, scanner2) = check(FunctionDeclaration::parse(&mut parser, Scanner::new(), false, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }

    // FUNCTION EXPRESSION
    #[test]
    fn function_expression_test_01() {
        let (node, scanner) = check(FunctionExpression::parse(&mut newparser("function bob(a,b) { return foo(a+b); }"), Scanner::new()));
        chk_scan(&scanner, 38);
        pretty_check(
            &*node,
            "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
            vec!["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
            vec![
                "Keyword: function",
                "IdentifierName: bob",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn function_expression_test_02() {
        let (node, scanner) = check(FunctionExpression::parse(&mut newparser("function (z) {}"), Scanner::new()));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "FunctionExpression: function ( z ) {  }", vec!["FormalParameters: z", "FunctionBody: "]);
        concise_check(&*node, "FunctionExpression: function ( z ) {  }", vec!["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn function_expression_test_err_01() {
        check_err(FunctionExpression::parse(&mut newparser(""), Scanner::new()), "‘function’ expected", 1, 1);
    }
    #[test]
    fn function_expression_test_err_02() {
        check_err(FunctionExpression::parse(&mut newparser("function"), Scanner::new()), "‘(’ expected", 1, 9);
    }
    #[test]
    fn function_expression_test_err_04() {
        check_err(FunctionExpression::parse(&mut newparser("function foo"), Scanner::new()), "‘(’ expected", 1, 13);
    }
    #[test]
    fn function_expression_test_err_05() {
        check_err(FunctionExpression::parse(&mut newparser("function foo("), Scanner::new()), "‘)’ expected", 1, 14);
    }
    #[test]
    fn function_expression_test_err_06() {
        check_err(FunctionExpression::parse(&mut newparser("function foo()"), Scanner::new()), "‘{’ expected", 1, 15);
    }
    #[test]
    fn function_expression_test_err_07() {
        check_err(FunctionExpression::parse(&mut newparser("function foo(){"), Scanner::new()), "‘}’ expected", 1, 16);
    }
    #[test]
    fn function_expression_test_prettyerrors_1() {
        let (item, _) = FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_expression_test_prettyerrors_2() {
        let (item, _) = FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_expression_test_conciseerrors_1() {
        let (item, _) = FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn function_expression_test_conciseerrors_2() {
        let (item, _) = FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }

    // FUNCTION BODY
    #[test]
    fn function_body_test_01() {
        let (node, scanner) = FunctionBody::parse(&mut newparser(""), Scanner::new(), false, false);
        chk_scan(&scanner, 0);
        pretty_check(&*node, "FunctionBody: ", vec!["FunctionStatementList: "]);
        concise_check(&*node, "", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn function_body_test_prettyerrors_1() {
        let (item, _) = FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_body_test_conciseeerrors_1() {
        let (item, _) = FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
        concise_error_validate(&*item);
    }
    #[test]
    fn function_body_test_cache_01() {
        let mut parser = newparser("a; b; c;");
        let (node, scanner) = FunctionBody::parse(&mut parser, Scanner::new(), false, false);
        let (node2, scanner2) = FunctionBody::parse(&mut parser, Scanner::new(), false, false);
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }

    // FUNCTION STATEMENT LIST
    #[test]
    fn function_statement_list_test_01() {
        let (node, scanner) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
        chk_scan(&scanner, 0);
        pretty_check(&*node, "FunctionStatementList: ", vec![]);
        concise_check(&*node, "", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn function_statement_list_test_prettyerrors_1() {
        let (item, _) = FunctionStatementList::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_statement_list_test_prettyerrors_2() {
        let (item, _) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
        pretty_error_validate(&*item);
    }
    #[test]
    fn function_statement_list_test_conciseeerrors_1() {
        let (item, _) = FunctionStatementList::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
        concise_error_validate(&*item);
    }
    #[test]
    fn function_statement_list_test_conciseerrors_2() {
        let (item, _) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
        concise_error_validate(&*item);
    }
}

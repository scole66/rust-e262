use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::primary_expressions::PropertyName;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncGeneratorMethod[Yield, Await] :
//      async [no LineTerminator here] * PropertyName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorMethod {
    name: Box<PropertyName>,
    params: Box<UniqueFormalParameters>,
    body: Box<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async * {} ( {} ) {{ {} }}", self.name, self.params, self.body)
    }
}

impl PrettyPrint for AsyncGeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorMethod: {}", first, self)?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorMethod: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl AsyncGeneratorMethod {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        let after_star = scan_for_punct(after_async, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (name, after_name) = PropertyName::parse(parser, after_star, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(AsyncGeneratorMethod { name, params, body }), after_rb))
    }
}

// AsyncGeneratorDeclaration[Yield, Await, Default] :
//      async [no LineTerminator here] function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
//      [+Default] async [no LineTerminator here] function * ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorDeclaration {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncGeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}AsyncGeneratorDeclaration: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncGeneratorDeclaration {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_func = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, yield_flag, await_flag) {
            Err(err) => {
                if default_flag {
                    Ok((None, after_star))
                } else {
                    Err(err)
                }
            }
            Ok((node, scan)) => Ok((Some(node), scan)),
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(AsyncGeneratorDeclaration { ident, params, body }), after_rb))
    }
}

// AsyncGeneratorExpression :
//      async [no LineTerminator here] function * BindingIdentifier[+Yield, +Await]opt ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorExpression {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
            None => write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncGeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorExpression: {}", first, self)?;
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
        writeln!(writer, "{}AsyncGeneratorExpression: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncGeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_func = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_ident) = match BindingIdentifier::parse(parser, after_star, true, true) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_ident, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, true, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(AsyncGeneratorExpression { ident, params, body }), after_rb))
    }
}

// AsyncGeneratorBody :
//      FunctionBody[+Yield, +Await]
#[derive(Debug)]
pub struct AsyncGeneratorBody(Box<FunctionBody>);

impl fmt::Display for AsyncGeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncGeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncGeneratorBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Box<Self>, Scanner) {
        let (body, after_body) = FunctionBody::parse(parser, scanner, true, true);
        (Box::new(AsyncGeneratorBody(body)), after_body)
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // ASYNC GENERATOR METHOD
    #[test]
    fn async_generator_method_test_01() {
        let (node, scanner) = check(AsyncGeneratorMethod::parse(&mut newparser("async *a(){}"), Scanner::new(), false, false));
        chk_scan(&scanner, 6 + 6);
        pretty_check(&*node, "AsyncGeneratorMethod: async * a (  ) {  }", vec!["PropertyName: a", "UniqueFormalParameters: ", "AsyncGeneratorBody: "]);
        concise_check(
            &*node,
            "AsyncGeneratorMethod: async * a (  ) {  }",
            vec!["Keyword: async", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn async_generator_method_test_025() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_generator_method_test_02() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async"), Scanner::new(), false, false), "‘*’ expected", 1, 6);
    }
    #[test]
    fn async_generator_method_test_03() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async *"), Scanner::new(), false, false), "PropertyName expected", 1, 2 + 6);
    }
    #[test]
    fn async_generator_method_test_04() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async *a"), Scanner::new(), false, false), "‘(’ expected", 1, 3 + 6);
    }
    #[test]
    fn async_generator_method_test_05() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async *a("), Scanner::new(), false, false), "‘)’ expected", 1, 4 + 6);
    }
    #[test]
    fn async_generator_method_test_06() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async *a()"), Scanner::new(), false, false), "‘{’ expected", 1, 5 + 6);
    }
    #[test]
    fn async_generator_method_test_07() {
        check_err(AsyncGeneratorMethod::parse(&mut newparser("async *a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 6 + 6);
    }
    #[test]
    fn async_generator_method_test_prettyerrors_1() {
        let (item, _) = AsyncGeneratorMethod::parse(&mut newparser("async * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_method_test_conciseerrors_1() {
        let (item, _) = AsyncGeneratorMethod::parse(&mut newparser("async * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // ASYNC GENERATOR DECLARATION
    #[test]
    fn async_generator_declaration_test_01() {
        let (node, scanner) = check(AsyncGeneratorDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 15 + 6);
        pretty_check(&*node, "AsyncGeneratorDeclaration: async function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "AsyncGeneratorBody: "]);
        concise_check(
            &*node,
            "AsyncGeneratorDeclaration: async function * a (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_generator_declaration_test_02() {
        let (node, scanner) = check(AsyncGeneratorDeclaration::parse(&mut newparser("async function *(){}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 14 + 6);
        pretty_check(&*node, "AsyncGeneratorDeclaration: async function * (  ) {  }", vec!["FormalParameters: ", "AsyncGeneratorBody: "]);
        concise_check(
            &*node,
            "AsyncGeneratorDeclaration: async function * (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_generator_declaration_test_03() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_generator_declaration_test_04() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, true), "‘*’ expected", 1, 9 + 6);
    }
    #[test]
    fn async_generator_declaration_test_041() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async \nfunction"), Scanner::new(), false, false, true), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_generator_declaration_test_05() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function *"), Scanner::new(), false, false, true), "‘(’ expected", 1, 11 + 6);
    }
    #[test]
    fn async_generator_declaration_test_06() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h"), Scanner::new(), false, false, true), "‘(’ expected", 1, 13 + 6);
    }
    #[test]
    fn async_generator_declaration_test_07() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 15 + 6);
    }
    #[test]
    fn async_generator_declaration_test_075() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 13 + 6);
    }
    #[test]
    fn async_generator_declaration_test_076() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * ("), Scanner::new(), false, false, false), "Not an identifier", 1, 11 + 6);
    }
    #[test]
    fn async_generator_declaration_test_08() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ( u"), Scanner::new(), false, false, true), "‘)’ expected", 1, 17 + 6);
    }
    #[test]
    fn async_generator_declaration_test_09() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ( u )"), Scanner::new(), false, false, true), "‘{’ expected", 1, 19 + 6);
    }
    #[test]
    fn async_generator_declaration_test_10() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ( u ) {"), Scanner::new(), false, false, true), "‘}’ expected", 1, 21 + 6);
    }
    #[test]
    fn async_generator_declaration_test_11() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ( u ) { z;"), Scanner::new(), false, false, true), "‘}’ expected", 1, 24 + 6);
    }
    #[test]
    fn async_generator_declaration_test_12() {
        check_err(AsyncGeneratorDeclaration::parse(&mut newparser("async"), Scanner::new(), false, false, true), "‘function’ expected", 1, 6);
    }
    #[test]
    fn async_generator_declaration_test_prettyerrors_1() {
        let (item, _) = AsyncGeneratorDeclaration::parse(&mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_declaration_test_prettyerrors_2() {
        let (item, _) = AsyncGeneratorDeclaration::parse(&mut newparser("async function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_declaration_test_conciseerrors_1() {
        let (item, _) = AsyncGeneratorDeclaration::parse(&mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn async_generator_declaration_test_conciseerrors_2() {
        let (item, _) = AsyncGeneratorDeclaration::parse(&mut newparser("async function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(*item);
    }

    // ASYNC GENERATOR EXPRESSION
    #[test]
    fn async_generator_expression_test_01() {
        let (node, scanner) = check(AsyncGeneratorExpression::parse(&mut newparser("async function *a(){}"), Scanner::new()));
        chk_scan(&scanner, 15 + 6);
        pretty_check(&*node, "AsyncGeneratorExpression: async function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "AsyncGeneratorBody: "]);
        concise_check(
            &*node,
            "AsyncGeneratorExpression: async function * a (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_generator_expression_test_02() {
        let (node, scanner) = check(AsyncGeneratorExpression::parse(&mut newparser("async function *(){}"), Scanner::new()));
        chk_scan(&scanner, 14 + 6);
        pretty_check(&*node, "AsyncGeneratorExpression: async function * (  ) {  }", vec!["FormalParameters: ", "AsyncGeneratorBody: "]);
        concise_check(
            &*node,
            "AsyncGeneratorExpression: async function * (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_generator_expression_test_03() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_generator_expression_test_031() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async"), Scanner::new()), "‘function’ expected", 1, 6);
    }
    #[test]
    fn async_generator_expression_test_04() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function"), Scanner::new()), "‘*’ expected", 1, 9 + 6);
    }
    #[test]
    fn async_generator_expression_test_041() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async \nfunction"), Scanner::new()), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_generator_expression_test_05() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function *"), Scanner::new()), "‘(’ expected", 1, 11 + 6);
    }
    #[test]
    fn async_generator_expression_test_06() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h"), Scanner::new()), "‘(’ expected", 1, 13 + 6);
    }
    #[test]
    fn async_generator_expression_test_07() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h ("), Scanner::new()), "‘)’ expected", 1, 15 + 6);
    }
    #[test]
    fn async_generator_expression_test_08() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u"), Scanner::new()), "‘)’ expected", 1, 17 + 6);
    }
    #[test]
    fn async_generator_expression_test_09() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u )"), Scanner::new()), "‘{’ expected", 1, 19 + 6);
    }
    #[test]
    fn async_generator_expression_test_10() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u ) {"), Scanner::new()), "‘}’ expected", 1, 21 + 6);
    }
    #[test]
    fn async_generator_expression_test_11() {
        check_err(AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u ) { z;"), Scanner::new()), "‘}’ expected", 1, 24 + 6);
    }
    #[test]
    fn async_generator_expression_test_prettyerrors_1() {
        let (item, _) = AsyncGeneratorExpression::parse(&mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_expression_test_prettyerrors_2() {
        let (item, _) = AsyncGeneratorExpression::parse(&mut newparser("async function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_expression_test_conciseerrors_1() {
        let (item, _) = AsyncGeneratorExpression::parse(&mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn async_generator_expression_test_conciseerrors_2() {
        let (item, _) = AsyncGeneratorExpression::parse(&mut newparser("async function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }

    // ASYNC GENERATOR BODY
    #[test]
    fn async_generator_body_test_01() {
        let (node, scanner) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        chk_scan(&scanner, 8);
        pretty_check(&*node, "AsyncGeneratorBody: yield 1 ;", vec!["FunctionBody: yield 1 ;"]);
        concise_check(&*node, "ExpressionStatement: yield 1 ;", vec!["YieldExpression: yield 1", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_generator_body_test_prettyerrors_1() {
        let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        pretty_error_validate(*item);
    }
    #[test]
    fn async_generator_body_test_conciseerrors_1() {
        let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        concise_error_validate(*item);
    }
}

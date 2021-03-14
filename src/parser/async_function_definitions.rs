use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::primary_expressions::PropertyName;
use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncFunctionDeclaration[Yield, Await, Default] :
//      async [no LineTerminator here] function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
//      [+Default] async [no LineTerminator here] function ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncFunctionDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncFunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}AsyncFunctionDeclaration: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
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

impl AsyncFunctionDeclaration {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_function = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_function, yield_flag, await_flag) {
            Err(e) => {
                if !default_flag {
                    Err(e)
                } else {
                    Ok((None, after_function))
                }
            }
            Ok((node, scan)) => Ok((Some(node), scan)),
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncFunctionDeclaration { ident, params, body }), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitDefaultKey { scanner, yield_flag, await_flag, default_flag };
        match parser.async_function_declaration_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, default_flag);
                parser.async_function_declaration_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// AsyncFunctionExpression :
//      async [no LineTerminator here] function BindingIdentifier[~Yield, +Await]opt ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncFunctionExpression {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncFunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncFunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionExpression: {}", first, self)?;
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
        writeln!(writer, "{}AsyncFunctionExpression: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncFunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncFunctionExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_function = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_function, false, true) {
            Err(e) => (None, after_function),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncFunctionExpression { ident, params, body }), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match parser.async_function_expression_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_function_expression_cache.insert(scanner, result.clone());
                result
            }
        }
    }
}

// AsyncMethod[Yield, Await] :
//      async [no LineTerminator here] PropertyName[?Yield, ?Await] ( UniqueFormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncMethod {
    ident: Rc<PropertyName>,
    params: Rc<UniqueFormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async {} ( {} ) {{ {} }}", self.ident, self.params, self.body)
    }
}

impl PrettyPrint for AsyncMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncMethod: {}", first, self)?;
        self.ident.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncMethod: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.ident.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl AsyncMethod {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (ident, after_ident) = PropertyName::parse(parser, after_async, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_ident, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncMethod { ident, params, body }), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.async_method_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.async_method_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// AsyncFunctionBody :
//      FunctionBody[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncFunctionBody(Rc<FunctionBody>);

impl fmt::Display for AsyncFunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncFunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncFunctionBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, false, true);
        (Rc::new(AsyncFunctionBody(fb)), after_fb)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.async_function_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_function_body_cache.insert(scanner, result.clone());
                result
            }
        }
    }
}

// AwaitExpression[Yield] :
//      await UnaryExpression[?Yield, +Await]
#[derive(Debug)]
pub enum AwaitExpression {
    Await(Rc<UnaryExpression>),
}

impl fmt::Display for AwaitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AwaitExpression::Await(boxed) = &self;
        write!(f, "await {}", boxed)
    }
}

impl PrettyPrint for AwaitExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        let AwaitExpression::Await(boxed) = &self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        pprint_token(writer, "await", TokenType::Keyword, &successive, Spot::NotFinal)?;
        let AwaitExpression::Await(ue) = self;
        ue.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AwaitExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let after_await = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Await)?;
        let (ue, after_ue) = UnaryExpression::parse(parser, after_await, yield_flag, true)?;
        Ok((Rc::new(AwaitExpression::Await(ue)), after_ue))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let key = YieldKey { scanner, yield_flag };
        match parser.await_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag);
                parser.await_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // ASYNC FUNCTION DECLARATION
    #[test]
    fn async_function_declaration_test_01() {
        let (node, scanner) = check(AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a,b) { return await foo(a+b); }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 50);
        pretty_check(
            &*node,
            "AsyncFunctionDeclaration: async function bob ( a , b ) { return await foo ( a + b ) ; }",
            vec!["BindingIdentifier: bob", "FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "AsyncFunctionDeclaration: async function bob ( a , b ) { return await foo ( a + b ) ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "IdentifierName: bob",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return await foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
    }
    #[test]
    fn async_function_declaration_test_02() {
        let (node, scanner) = check(AsyncFunctionDeclaration::parse(&mut newparser("async function (a,b) { return await foo(a+b); }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 47);
        pretty_check(
            &*node,
            "AsyncFunctionDeclaration: async function ( a , b ) { return await foo ( a + b ) ; }",
            vec!["FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "AsyncFunctionDeclaration: async function ( a , b ) { return await foo ( a + b ) ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return await foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
    }
    #[test]
    fn async_function_declaration_test_err_01() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_function_declaration_test_err_02() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async\n"), Scanner::new(), false, false, true), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_function_declaration_test_err_03() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async bob"), Scanner::new(), false, false, true), "‘function’ expected", 1, 6);
    }
    #[test]
    fn async_function_declaration_test_err_04() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, false), "Not an identifier", 1, 15);
    }
    #[test]
    fn async_function_declaration_test_err_05() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, true), "‘(’ expected", 1, 15);
    }
    #[test]
    fn async_function_declaration_test_err_06() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob("), Scanner::new(), false, false, true), "‘)’ expected", 1, 20);
    }
    #[test]
    fn async_function_declaration_test_err_07() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob()"), Scanner::new(), false, false, true), "‘{’ expected", 1, 21);
    }
    #[test]
    fn async_function_declaration_test_err_08() {
        check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob() {"), Scanner::new(), false, false, true), "‘}’ expected", 1, 23);
    }
    #[test]
    fn async_function_declaration_test_prettyerrors_1() {
        let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_function_declaration_test_prettyerrors_2() {
        let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_function_declaration_test_conciseerrors_1() {
        let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn async_function_declaration_test_conciseerrors_2() {
        let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC FUNCTION EXPRESSION
    #[test]
    fn async_function_expression_test_01() {
        let (node, scanner) = check(AsyncFunctionExpression::parse(&mut newparser("async function bob(a,b) { return await foo(a+b); }"), Scanner::new()));
        chk_scan(&scanner, 50);
        pretty_check(
            &*node,
            "AsyncFunctionExpression: async function bob ( a , b ) { return await foo ( a + b ) ; }",
            vec!["BindingIdentifier: bob", "FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "AsyncFunctionExpression: async function bob ( a , b ) { return await foo ( a + b ) ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "IdentifierName: bob",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return await foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_function_expression_test_02() {
        let (node, scanner) = check(AsyncFunctionExpression::parse(&mut newparser("async function (a,b) { return await foo(a+b); }"), Scanner::new()));
        chk_scan(&scanner, 47);
        pretty_check(
            &*node,
            "AsyncFunctionExpression: async function ( a , b ) { return await foo ( a + b ) ; }",
            vec!["FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "AsyncFunctionExpression: async function ( a , b ) { return await foo ( a + b ) ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return await foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn async_function_expression_test_err_01() {
        check_err(AsyncFunctionExpression::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_function_expression_test_err_02() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async\n"), Scanner::new()), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_function_expression_test_err_03() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async bob"), Scanner::new()), "‘function’ expected", 1, 6);
    }
    #[test]
    fn async_function_expression_test_err_05() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async function"), Scanner::new()), "‘(’ expected", 1, 15);
    }
    #[test]
    fn async_function_expression_test_err_06() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob("), Scanner::new()), "‘)’ expected", 1, 20);
    }
    #[test]
    fn async_function_expression_test_err_07() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob()"), Scanner::new()), "‘{’ expected", 1, 21);
    }
    #[test]
    fn async_function_expression_test_err_08() {
        check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob() {"), Scanner::new()), "‘}’ expected", 1, 23);
    }
    #[test]
    fn async_function_expression_test_prettyerrors_1() {
        let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_function_expression_test_prettyerrors_2() {
        let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_function_expression_test_conciseerrors_1() {
        let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn async_function_expression_test_conciseerrors_2() {
        let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC METHOD
    #[test]
    fn async_method_test_01() {
        let (node, scanner) = check(AsyncMethod::parse(&mut newparser("async bob(a,b) { return await foo(a+b); }"), Scanner::new(), false, false));
        chk_scan(&scanner, 41);
        pretty_check(
            &*node,
            "AsyncMethod: async bob ( a , b ) { return await foo ( a + b ) ; }",
            vec!["PropertyName: bob", "UniqueFormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
        );
        concise_check(
            &*node,
            "AsyncMethod: async bob ( a , b ) { return await foo ( a + b ) ; }",
            vec![
                "Keyword: async",
                "IdentifierName: bob",
                "Punctuator: (",
                "FormalParameterList: a , b",
                "Punctuator: )",
                "Punctuator: {",
                "ReturnStatement: return await foo ( a + b ) ;",
                "Punctuator: }",
            ],
        );
        format!("{:?}", node);
    }
    #[test]
    fn async_method_test_err_01() {
        check_err(AsyncMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_method_test_err_02() {
        check_err(AsyncMethod::parse(&mut newparser("async\n"), Scanner::new(), false, false), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_method_test_err_03() {
        check_err(AsyncMethod::parse(&mut newparser("async"), Scanner::new(), false, false), "PropertyName expected", 1, 6);
    }
    #[test]
    fn async_method_test_err_04() {
        check_err(AsyncMethod::parse(&mut newparser("async bob"), Scanner::new(), false, false), "‘(’ expected", 1, 10);
    }
    #[test]
    fn async_method_test_err_06() {
        check_err(AsyncMethod::parse(&mut newparser("async bob("), Scanner::new(), false, false), "‘)’ expected", 1, 11);
    }
    #[test]
    fn async_method_test_err_07() {
        check_err(AsyncMethod::parse(&mut newparser("async bob()"), Scanner::new(), false, false), "‘{’ expected", 1, 12);
    }
    #[test]
    fn async_method_test_err_08() {
        check_err(AsyncMethod::parse(&mut newparser("async bob() {"), Scanner::new(), false, false), "‘}’ expected", 1, 14);
    }
    #[test]
    fn async_method_test_prettyerrors_1() {
        let (item, _) = AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_method_test_conciseerrors_1() {
        let (item, _) = AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC FUNCTION BODY
    #[test]
    fn async_function_body_test_01() {
        let (node, scanner) = AsyncFunctionBody::parse(&mut newparser("yield = 3;"), Scanner::new());
        chk_scan(&scanner, 10);
        pretty_check(&*node, "AsyncFunctionBody: yield = 3 ;", vec!["FunctionBody: yield = 3 ;"]);
        concise_check(&*node, "ExpressionStatement: yield = 3 ;", vec!["AssignmentExpression: yield = 3", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_function_body_test_02() {
        let (node, scanner) = AsyncFunctionBody::parse(&mut newparser("await = 3;"), Scanner::new());
        chk_scan(&scanner, 0);
        pretty_check(&*node, "AsyncFunctionBody: ", vec!["FunctionBody: "]);
        concise_check(&*node, "", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn async_function_body_test_prettyerrors_1() {
        let (item, _) = AsyncFunctionBody::parse(&mut newparser("bananas;"), Scanner::new());
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_function_body_test_conciseerrors_1() {
        let (item, _) = AsyncFunctionBody::parse(&mut newparser("bananas;"), Scanner::new());
        concise_error_validate(&*item);
    }

    // AWAIT EXPRESSION
    #[test]
    fn await_expression_test_01() {
        let (node, scanner) = check(AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "AwaitExpression: await a ( )", vec!["UnaryExpression: a ( )"]);
        concise_check(&*node, "AwaitExpression: await a ( )", vec!["Keyword: await", "CallMemberExpression: a ( )"]);
        format!("{:?}", node);
    }
    #[test]
    fn await_expression_test_err_01() {
        check_err(AwaitExpression::parse(&mut newparser(""), Scanner::new(), false), "‘await’ expected", 1, 1);
    }
    #[test]
    fn await_expression_test_err_02() {
        check_err(AwaitExpression::parse(&mut newparser("await"), Scanner::new(), false), "UnaryExpression expected", 1, 6);
    }
    #[test]
    fn await_expression_test_prettyerrors_1() {
        let (item, _) = AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn await_expression_test_conciseerrors_1() {
        let (item, _) = AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false).unwrap();
        concise_error_validate(&*item);
    }
}

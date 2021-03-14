use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::arrow_function_definitions::{ArrowFormalParameters, ExpressionBody};
use super::async_function_definitions::AsyncFunctionBody;
use super::identifiers::BindingIdentifier;
use super::left_hand_side_expressions::{Arguments, MemberExpression};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncArrowFunction[In, Yield, Await] :
//      async [no LineTerminator here] AsyncArrowBindingIdentifier[?Yield] [no LineTerminator here] => AsyncConciseBody[?In]
//      CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await] [no LineTerminator here] => AsyncConciseBody[?In]
#[derive(Debug)]
pub enum AsyncArrowFunction {
    IdentOnly(Rc<AsyncArrowBindingIdentifier>, Rc<AsyncConciseBody>),
    Formals(Rc<AsyncArrowHead>, Rc<AsyncConciseBody>),
}

impl fmt::Display for AsyncArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncArrowFunction::IdentOnly(id, body) => write!(f, "async {} => {}", id, body),
            AsyncArrowFunction::Formals(params, body) => write!(f, "{} => {}", params, body),
        }
    }
}

impl PrettyPrint for AsyncArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowFunction: {}", first, self)?;
        match self {
            AsyncArrowFunction::IdentOnly(id, body) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(params, body) => {
                params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowFunction: {}", first, self)?;
        match self {
            AsyncArrowFunction::IdentOnly(head, tail) => {
                pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(head, tail) => {
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncArrowFunction {
    fn parse_normal_form(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (id, after_id) = AsyncArrowBindingIdentifier::parse(parser, after_async, yield_flag)?;
        no_line_terminator(after_id, parser.source)?;
        let after_arrow = scan_for_punct(after_id, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((Rc::new(AsyncArrowFunction::IdentOnly(id, body)), after_body))
    }
    fn parse_covered_form(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Rc<AsyncArrowHead>, Scanner, Rc<AsyncConciseBody>, Scanner), ParseError> {
        let (cceaaah, after_params) = CoverCallExpressionAndAsyncArrowHead::parse(parser, scanner, yield_flag, await_flag)?;
        let (real_params, after_reals) = AsyncArrowHead::parse(parser, scanner)?;
        assert!(after_params == after_reals);
        no_line_terminator(after_params, parser.source)?;
        let after_arrow = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((real_params, after_params, body, after_body))
    }
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let pot_norm = Self::parse_normal_form(parser, scanner, in_flag, yield_flag, await_flag);
        let pot_covered = Self::parse_covered_form(parser, scanner, in_flag, yield_flag, await_flag);
        match (pot_norm, pot_covered) {
            (Err(err1), Err(err2)) => Err(cmp::max_by(err2, err1, ParseError::compare)),
            (Err(_), Ok((real_params, after_params, body, after_covered))) => Ok((Rc::new(AsyncArrowFunction::Formals(real_params, body)), after_covered)),
            // (Ok(norm), Ok(covered)) can never happen, given the particulars of the productions
            (norm, covered) => {
                assert!(covered.is_err() && norm.is_ok());
                norm
            }
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.async_arrow_function_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.async_arrow_function_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// AsyncArrowHead :
//      async [no LineTerminator here] ArrowFormalParameters[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowHead(Rc<ArrowFormalParameters>);

impl fmt::Display for AsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async {}", self.0)
    }
}

impl PrettyPrint for AsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowHead: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowHead: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AsyncArrowHead {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (params, after_params) = ArrowFormalParameters::parse(parser, after_async, false, true)?;
        Ok((Rc::new(AsyncArrowHead(params)), after_params))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match parser.async_arrow_head_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_arrow_head_cache.insert(scanner, result.clone());
                result
            }
        }
    }
}

// AsyncConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, +Await]
//      { AsyncFunctionBody }
#[derive(Debug)]
pub enum AsyncConciseBody {
    Expression(Rc<ExpressionBody>),
    Function(Rc<AsyncFunctionBody>),
}

impl fmt::Display for AsyncConciseBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncConciseBody::Expression(node) => node.fmt(f),
            AsyncConciseBody::Function(node) => write!(f, "{{ {} }}", node),
        }
    }
}

impl PrettyPrint for AsyncConciseBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncConciseBody: {}", first, self)?;
        match self {
            AsyncConciseBody::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AsyncConciseBody::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AsyncConciseBody::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            AsyncConciseBody::Function(node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AsyncConciseBody: {}", first, self)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncConciseBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("AsyncConciseBody expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_curly = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
                let (fb, after_fb) = AsyncFunctionBody::parse(parser, after_curly);
                let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(AsyncConciseBody::Function(fb)), after_rb))
            })
            .otherwise(|| {
                let r = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace);
                match r {
                    Err(_) => {
                        let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, true)?;
                        Ok((Rc::new(AsyncConciseBody::Expression(exp)), after_exp))
                    }
                    Ok(_) => Err(ParseError::new(String::new(), scanner.line, scanner.column)),
                }
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        let key = InKey { scanner, in_flag };
        match parser.async_concise_body_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag);
                parser.async_concise_body_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// AsyncArrowBindingIdentifier[Yield] :
//      BindingIdentifier[?Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowBindingIdentifier(Rc<BindingIdentifier>);

impl fmt::Display for AsyncArrowBindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncArrowBindingIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncArrowBindingIdentifier: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncArrowBindingIdentifier {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let (ident, after_ident) = BindingIdentifier::parse(parser, scanner, yield_flag, true)?;
        Ok((Rc::new(AsyncArrowBindingIdentifier(ident)), after_ident))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let key = YieldKey { scanner, yield_flag };
        match parser.async_arrow_binding_identifer_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag);
                parser.async_arrow_binding_identifer_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CoverCallExpressionAndAsyncArrowHead {
    expression: Rc<MemberExpression>,
    args: Rc<Arguments>,
}

impl fmt::Display for CoverCallExpressionAndAsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.expression, self.args)
    }
}

impl PrettyPrint for CoverCallExpressionAndAsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverCallExpressionAndAsyncArrowHead: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverCallExpressionAndAsyncArrowHead: {}", first, self)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverCallExpressionAndAsyncArrowHead {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (expression, after_exp) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_exp, yield_flag, await_flag)?;
        Ok((Rc::new(CoverCallExpressionAndAsyncArrowHead { expression, args }), after_args))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.cover_call_expression_and_async_arrow_head_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.cover_call_expression_and_async_arrow_head_cache.insert(key, result.clone());
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

    // ASYNC ARROW FUNCTION
    #[test]
    fn async_arrow_function_test_01() {
        let (node, scanner) = check(AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(&*node, AsyncArrowFunction::IdentOnly(..)));
        pretty_check(&*node, "AsyncArrowFunction: async a => a", vec!["AsyncArrowBindingIdentifier: a", "AsyncConciseBody: a"]);
        concise_check(&*node, "AsyncArrowFunction: async a => a", vec!["Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_arrow_function_test_02() {
        let (node, scanner) = check(AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 16);
        assert!(matches!(&*node, AsyncArrowFunction::Formals(..)));
        pretty_check(&*node, "AsyncArrowFunction: async ( a , b ) => a + b", vec!["AsyncArrowHead: async ( a , b )", "AsyncConciseBody: a + b"]);
        concise_check(&*node, "AsyncArrowFunction: async ( a , b ) => a + b", vec!["AsyncArrowHead: async ( a , b )", "Punctuator: =>", "AdditiveExpression: a + b"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_arrow_function_test_err_01() {
        check_err(AsyncArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_arrow_function_test_err_02() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_arrow_function_test_err_03() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async"), Scanner::new(), true, false, false), "Not an identifier", 1, 6);
    }
    #[test]
    fn async_arrow_function_test_err_04() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async a\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 8);
    }
    #[test]
    fn async_arrow_function_test_err_05() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async a"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 8);
    }
    #[test]
    fn async_arrow_function_test_err_06() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async a=>"), Scanner::new(), true, false, false), "AsyncConciseBody expected", 1, 10);
    }
    #[test]
    fn async_arrow_function_test_err_07() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async ("), Scanner::new(), true, false, false), "‘)’ expected", 1, 8);
    }
    #[test]
    fn async_arrow_function_test_err_08() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async (5)"), Scanner::new(), true, false, false), "‘)’ expected", 1, 8);
    }
    #[test]
    fn async_arrow_function_test_err_09() {
        check_err(AsyncArrowFunction::parse(&mut newparser("blue (5)"), Scanner::new(), true, false, false), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_arrow_function_test_err_10() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async (a)\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 10);
    }
    #[test]
    fn async_arrow_function_test_err_11() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async (a)"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 10);
    }
    #[test]
    fn async_arrow_function_test_err_12() {
        check_err(AsyncArrowFunction::parse(&mut newparser("async (a)=>"), Scanner::new(), true, false, false), "AsyncConciseBody expected", 1, 12);
    }
    #[test]
    fn async_arrow_function_test_prettyerrors_1() {
        let (item, _) = AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_arrow_function_test_prettyerrors_2() {
        let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_arrow_function_test_conciseerrors_1() {
        let (item, _) = AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn async_arrow_function_test_conciseerrors_2() {
        let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC CONCISE BODY
    #[test]
    fn async_concise_body_test_01() {
        let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
        chk_scan(&scanner, 1);
        assert!(matches!(&*node, AsyncConciseBody::Expression(..)));
        pretty_check(&*node, "AsyncConciseBody: a", vec!["ExpressionBody: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn async_concise_body_test_02() {
        let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("{a;}"), Scanner::new(), true));
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, AsyncConciseBody::Function(..)));
        pretty_check(&*node, "AsyncConciseBody: { a ; }", vec!["AsyncFunctionBody: a ;"]);
        concise_check(&*node, "AsyncConciseBody: { a ; }", vec!["Punctuator: {", "ExpressionStatement: a ;", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_concise_body_test_err_01() {
        check_err(AsyncConciseBody::parse(&mut newparser(""), Scanner::new(), true), "AsyncConciseBody expected", 1, 1);
    }
    #[test]
    fn async_concise_body_test_err_02() {
        check_err(AsyncConciseBody::parse(&mut newparser("{"), Scanner::new(), true), "‘}’ expected", 1, 2);
    }
    #[test]
    fn async_concise_body_test_prettyerrors_1() {
        let (item, _) = AsyncConciseBody::parse(&mut newparser("expression"), Scanner::new(), true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_concise_body_test_prettyerrors_2() {
        let (item, _) = AsyncConciseBody::parse(&mut newparser("{ statement_list; }"), Scanner::new(), true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_concise_body_test_conciseerrors_1() {
        let (item, _) = AsyncConciseBody::parse(&mut newparser("expression"), Scanner::new(), true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn async_concise_body_test_conciseerrors_2() {
        let (item, _) = AsyncConciseBody::parse(&mut newparser("{ statement_list; }"), Scanner::new(), true).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC ARROW BINDING IDENTIFIER
    #[test]
    fn async_arrow_binding_identifier_test_01() {
        let (node, scanner) = check(AsyncArrowBindingIdentifier::parse(&mut newparser("a"), Scanner::new(), false));
        chk_scan(&scanner, 1);
        pretty_check(&*node, "AsyncArrowBindingIdentifier: a", vec!["BindingIdentifier: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn async_arrow_binding_identifier_test_err_01() {
        check_err(AsyncArrowBindingIdentifier::parse(&mut newparser(""), Scanner::new(), false), "Not an identifier", 1, 1);
    }
    #[test]
    fn async_arrow_binding_identifier_test_prettyerrors_1() {
        let (item, _) = AsyncArrowBindingIdentifier::parse(&mut newparser("identifier"), Scanner::new(), false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_arrow_binding_identifier_test_conciseerrors_1() {
        let (item, _) = AsyncArrowBindingIdentifier::parse(&mut newparser("identifier"), Scanner::new(), false).unwrap();
        concise_error_validate(&*item);
    }
    // COVER CALL EXPRESSION AND ASYNC ARROW HEAD
    #[test]
    fn cceaaah_test_01() {
        let (node, scanner) = check(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("a(10)"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        pretty_check(&*node, "CoverCallExpressionAndAsyncArrowHead: a ( 10 )", vec!["MemberExpression: a", "Arguments: ( 10 )"]);
        concise_check(&*node, "CoverCallExpressionAndAsyncArrowHead: a ( 10 )", vec!["IdentifierName: a", "Arguments: ( 10 )"]);
        format!("{:?}", node);
    }
    #[test]
    fn cceaaah_test_err_01() {
        check_err(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser(""), Scanner::new(), false, false), "MemberExpression expected", 1, 1);
    }
    #[test]
    fn cceaaah_test_err_02() {
        check_err(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("name"), Scanner::new(), false, false), "‘(’ expected", 1, 5);
    }
    #[test]
    fn cceaaah_test_prettyerrors_1() {
        let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn cceaaah_test_conciseerrors_1() {
        let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    // ASYNC ARROW HEAD
    #[test]
    fn async_arrow_head_test_01() {
        let (node, scanner) = check(AsyncArrowHead::parse(&mut newparser("async (a)"), Scanner::new()));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "AsyncArrowHead: async ( a )", vec!["ArrowFormalParameters: ( a )"]);
        concise_check(&*node, "AsyncArrowHead: async ( a )", vec!["Keyword: async", "ArrowFormalParameters: ( a )"]);
        format!("{:?}", node);
    }
    #[test]
    fn async_arrow_head_test_err_01() {
        check_err(AsyncArrowHead::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
    }
    #[test]
    fn async_arrow_head_test_err_02() {
        check_err(AsyncArrowHead::parse(&mut newparser("async\n"), Scanner::new()), "Newline not allowed here.", 1, 6);
    }
    #[test]
    fn async_arrow_head_test_err_03() {
        check_err(AsyncArrowHead::parse(&mut newparser("async"), Scanner::new()), "‘(’ expected", 1, 6);
    }
    #[test]
    fn async_arrow_head_test_prettyerrors_1() {
        let (item, _) = AsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn async_arrow_head_test_conciseerrors_1() {
        let (item, _) = AsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
}

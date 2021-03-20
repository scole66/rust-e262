use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::UniqueFormalParameters;
use super::primary_expressions::CoverParenthesizedExpressionAndArrowParameterList;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ArrowFunction[In, Yield, Await] :
//      ArrowParameters[?Yield, ?Await] [no LineTerminator here] => ConciseBody[?In]
#[derive(Debug)]
pub struct ArrowFunction {
    parameters: Rc<ArrowParameters>,
    body: Rc<ConciseBody>,
}

impl fmt::Display for ArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.parameters, self.body)
    }
}

impl PrettyPrint for ArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrowFunction: {}", first, self)?;
        self.parameters.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrowFunction: {}", first, self)?;
        self.parameters.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl ArrowFunction {
    // ArrowFunction's only parent is AssignmentExpression. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (parameters, after_params) = ArrowParameters::parse(parser, scanner, yield_flag, await_flag)?;
        no_line_terminator(after_params, parser.source)?;
        let after_arrow = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = ConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((Rc::new(ArrowFunction { parameters, body }), after_body))
    }
}

// ArrowParameters[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
#[derive(Debug)]
pub enum ArrowParameters {
    Identifier(Rc<BindingIdentifier>),
    Formals(Rc<ArrowFormalParameters>),
}

impl fmt::Display for ArrowParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrowParameters::Identifier(node) => node.fmt(f),
            ArrowParameters::Formals(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for ArrowParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrowParameters: {}", first, self)?;
        match self {
            ArrowParameters::Identifier(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrowParameters::Formals(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ArrowParameters::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            ArrowParameters::Formals(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl ArrowParameters {
    // ArrowParameters's only direct parent is ArrowFunction. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Identifier or Formal Parameters expected", scanner.line, scanner.column))
            .otherwise(|| BindingIdentifier::parse(parser, scanner, yield_flag, await_flag).map(|(bi, after_bi)| (Rc::new(ArrowParameters::Identifier(bi)), after_bi)))
            .otherwise(|| {
                let (covered_formals, after_formals) = CoverParenthesizedExpressionAndArrowParameterList::parse(parser, scanner, yield_flag, await_flag)?;
                let (formals, after_reparse) = ArrowFormalParameters::parse(parser, scanner, yield_flag, await_flag)?;

                // This is only a successful cover if the parsed production and its cover end at the same place. But
                // particular cover is all about balanced parenthses. Since both productions require starting and ending
                // with parentheses and they also both require correct nesting of parentheses, it's actually impossible
                // for "after_formals" and "after_reparse" to be different. (Which means I can never get coverage with
                // the "make an error if they're different" case.) So rather than do that, I'll just debug_assert.

                // if after_formals != after_reparse {
                //     Err(ParseError::new("‘)’ expected", after_reparse.line, after_reparse.column))
                // } else {
                //     Ok((Box::new(ArrowParameters::Formals(formals)), after_formals))
                // }
                debug_assert!(after_formals == after_reparse);
                Ok((Rc::new(ArrowParameters::Formals(formals)), after_formals))
            })
    }
}

// ArrowFormalParameters[Yield, Await] :
//      ( UniqueFormalParameters[?Yield, ?Await] )
#[derive(Debug)]
pub struct ArrowFormalParameters(Rc<UniqueFormalParameters>);

impl fmt::Display for ArrowFormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "( {} )", self.0)
    }
}

impl PrettyPrint for ArrowFormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrowFormalParameters: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrowFormalParameters: {}", first, self)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ArrowFormalParameters {
    // I _think_ this needs to be cached.
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_lp = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, yield_flag, await_flag);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        Ok((Rc::new(ArrowFormalParameters(params)), after_rp))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.arrow_formal_parameters_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.arrow_formal_parameters_cache.insert(key, result.clone());
                result
            }
        }
    }
}

// ConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, ~Await]
//      { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub enum ConciseBody {
    Expression(Rc<ExpressionBody>),
    Function(Rc<FunctionBody>),
}

impl fmt::Display for ConciseBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConciseBody::Expression(node) => node.fmt(f),
            ConciseBody::Function(node) => write!(f, "{{ {} }}", node),
        }
    }
}

impl PrettyPrint for ConciseBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ConciseBody: {}", first, self)?;
        match self {
            ConciseBody::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ConciseBody::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ConciseBody::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            ConciseBody::Function(node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ConciseBody: {}", first, self)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl ConciseBody {
    // ConciseBody's only direct parent is ArrowFunction. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("ConciseBody expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_curly = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
                let (fb, after_fb) = FunctionBody::parse(parser, after_curly, in_flag, false);
                let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ConciseBody::Function(fb)), after_rb))
            })
            .otherwise(|| {
                let r = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace);
                match r {
                    Err(_) => {
                        let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, false)?;
                        Ok((Rc::new(ConciseBody::Expression(exp)), after_exp))
                    }
                    Ok(_) => Err(ParseError::new("ExpressionBody expected", scanner.line, scanner.column)),
                }
            })
    }
}

// ExpressionBody[In, Await] :
//      AssignmentExpression[?In, ~Yield, ?Await]
#[derive(Debug)]
pub struct ExpressionBody {
    expression: Rc<AssignmentExpression>,
}

impl fmt::Display for ExpressionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.expression.fmt(f)
    }
}

impl PrettyPrint for ExpressionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ExpressionBody: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.expression.concise_with_leftpad(writer, pad, state)
    }
}

impl ExpressionBody {
    // ExpressionBody has multiple potential parents. It should be cached.
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (ae, after_ae) = AssignmentExpression::parse(parser, scanner, in_flag, false, await_flag)?;
        Ok((Rc::new(ExpressionBody { expression: ae }), after_ae))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InAwaitKey { scanner, in_flag, await_flag };
        match parser.expression_body_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, await_flag);
                parser.expression_body_cache.insert(key, result.clone());
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

    // ARROW FUNCTION
    #[test]
    fn arrow_function_test_01() {
        let (node, scanner) = check(ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        pretty_check(&*node, "ArrowFunction: a => a", vec!["ArrowParameters: a", "ConciseBody: a"]);
        concise_check(&*node, "ArrowFunction: a => a", vec!["IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
        format!("{:?}", node);
    }
    #[test]
    fn arrow_function_test_02() {
        check_err(ArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false), "Identifier or Formal Parameters expected", 1, 1);
    }
    #[test]
    fn arrow_function_test_03() {
        check_err(ArrowFunction::parse(&mut newparser("a"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 2);
    }
    #[test]
    fn arrow_function_test_04() {
        check_err(ArrowFunction::parse(&mut newparser("a=>"), Scanner::new(), true, false, false), "ConciseBody expected", 1, 4);
    }
    #[test]
    fn arrow_function_test_05() {
        check_err(ArrowFunction::parse(&mut newparser("a\n=>a"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 2);
    }
    #[test]
    fn arrow_function_test_prettyerrors_1() {
        let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn arrow_function_test_conciseerrors_1() {
        let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // ARROW PARAMETERS
    #[test]
    fn arrow_parameters_test_01() {
        let (node, scanner) = check(ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*node, ArrowParameters::Identifier(..)));
        pretty_check(&*node, "ArrowParameters: a", vec!["BindingIdentifier: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn arrow_parameters_test_02() {
        let r = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false);
        let (node, scanner) = check(r);
        chk_scan(&scanner, 3);
        assert!(matches!(&*node, ArrowParameters::Formals(..)));
        pretty_check(&*node, "ArrowParameters: ( a )", vec!["ArrowFormalParameters: ( a )"]);
        concise_check(&*node, "ArrowFormalParameters: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn arrow_parameters_test_err_01() {
        check_err(ArrowParameters::parse(&mut newparser(""), Scanner::new(), false, false), "Identifier or Formal Parameters expected", 1, 1);
    }
    #[test]
    fn arrow_parameters_test_err_02() {
        check_err(ArrowParameters::parse(&mut newparser("("), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
    }
    #[test]
    fn arrow_parameters_test_err_03() {
        check_err(ArrowParameters::parse(&mut newparser("(a"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
    }
    #[test]
    fn arrow_parameters_test_err_04() {
        check_err(ArrowParameters::parse(&mut newparser("(5 * 3)"), Scanner::new(), false, false), "‘)’ expected", 1, 2);
    }
    #[test]
    fn arrow_parameters_test_prettyerrors_1() {
        let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn arrow_parameters_test_prettyerrors_2() {
        let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn arrow_parameters_test_conciseerrors_1() {
        let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn arrow_parameters_test_conciseerrors_2() {
        let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    // CONCISE BODY
    #[test]
    fn concise_body_test_01() {
        let (node, scanner) = check(ConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
        chk_scan(&scanner, 1);
        assert!(matches!(&*node, ConciseBody::Expression(..)));
        pretty_check(&*node, "ConciseBody: a", vec!["ExpressionBody: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn concise_body_test_02() {
        let (node, scanner) = check(ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true));
        println!("node = {:?}", node);
        chk_scan(&scanner, 4);
        assert!(matches!(&*node, ConciseBody::Function(..)));
        pretty_check(&*node, "ConciseBody: { q ; }", vec!["FunctionBody: q ;"]);
        concise_check(&*node, "ConciseBody: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn concise_body_test_err_01() {
        check_err(ConciseBody::parse(&mut newparser(""), Scanner::new(), true), "ConciseBody expected", 1, 1);
    }
    #[test]
    fn concise_body_test_err_02() {
        check_err(ConciseBody::parse(&mut newparser("{"), Scanner::new(), true), "‘}’ expected", 1, 2);
    }
    #[test]
    fn concise_body_test_prettyerrors_1() {
        let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn concise_body_test_prettyerrors_2() {
        let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn concise_body_test_conciseerrors_1() {
        let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concise_body_test_conciseerrors_2() {
        let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
        concise_error_validate(&*item);
    }

    // EXPRESSION BODY
    #[test]
    fn expression_body_test_01() {
        let (node, scanner) = check(ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false));
        chk_scan(&scanner, 1);
        pretty_check(&*node, "ExpressionBody: a", vec!["AssignmentExpression: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn expression_body_test_cache_01() {
        let mut parser = newparser("a+b+c+d+e");
        let (node, scanner) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
        let (node2, scanner2) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn expression_body_test_err_01() {
        check_err(ExpressionBody::parse(&mut newparser(""), Scanner::new(), true, false), "AssignmentExpression expected", 1, 1);
    }
    #[test]
    fn expression_body_test_prettyerrors_1() {
        let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn expression_body_test_conciseerrors_1() {
        let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
        concise_error_validate(&*item);
    }

    // ARROW FORMAL PARAMETERS
    #[test]
    fn arrow_formal_parameters_test_01() {
        let (node, scanner) = check(ArrowFormalParameters::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        pretty_check(&*node, "ArrowFormalParameters: ( a , b )", vec!["UniqueFormalParameters: a , b"]);
        concise_check(&*node, "ArrowFormalParameters: ( a , b )", vec!["Punctuator: (", "FormalParameterList: a , b", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn arrow_formal_parameters_test_cache_01() {
        let mut parser = newparser("(a,b)");
        let (node, scanner) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
        let (node2, scanner2) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn arrow_formal_parameters_test_err_01() {
        check_err(ArrowFormalParameters::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
    }
    #[test]
    fn arrow_formal_parameters_test_err_02() {
        check_err(ArrowFormalParameters::parse(&mut newparser("("), Scanner::new(), false, false), "‘)’ expected", 1, 2);
    }
    #[test]
    fn arrow_formal_parameters_test_prettyerrors_1() {
        let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn arrow_formal_parameters_test_conciseerrors_1() {
        let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
}

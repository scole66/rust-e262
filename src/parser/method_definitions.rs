use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::async_function_definitions::AsyncMethod;
use super::async_generator_function_definitions::AsyncGeneratorMethod;
use super::function_definitions::FunctionBody;
use super::generator_function_definitions::GeneratorMethod;
use super::parameter_lists::{FormalParameter, UniqueFormalParameters};
use super::primary_expressions::PropertyName;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// MethodDefinition[Yield, Await] :
//      PropertyName[?Yield, ?Await] ( UniqueFormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
//      GeneratorMethod[?Yield, ?Await]
//      AsyncMethod[?Yield, ?Await]
//      AsyncGeneratorMethod[?Yield, ?Await]
//      get PropertyName[?Yield, ?Await] ( ) { FunctionBody[~Yield, ~Await] }
//      set PropertyName[?Yield, ?Await] ( PropertySetParameterList ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub enum MethodDefinition {
    NamedFunction(Box<PropertyName>, Box<UniqueFormalParameters>, Box<FunctionBody>),
    Generator(Box<GeneratorMethod>),
    Async(Box<AsyncMethod>),
    AsyncGenerator(Box<AsyncGeneratorMethod>),
    Getter(Box<PropertyName>, Box<FunctionBody>),
    Setter(Box<PropertyName>, Box<PropertySetParameterList>, Box<FunctionBody>),
}

impl fmt::Display for MethodDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MethodDefinition::NamedFunction(name, params, body) => write!(f, "{} ( {} ) {{ {} }}", name, params, body),
            MethodDefinition::Generator(node) => node.fmt(f),
            MethodDefinition::Async(node) => node.fmt(f),
            MethodDefinition::AsyncGenerator(node) => node.fmt(f),
            MethodDefinition::Getter(name, body) => write!(f, "get {} ( ) {{ {} }}", name, body),
            MethodDefinition::Setter(name, args, body) => write!(f, "set {} ( {} ) {{ {} }}", name, args, body),
        }
    }
}

impl PrettyPrint for MethodDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MethodDefinition: {}", first, self)?;
        match self {
            MethodDefinition::NamedFunction(name, args, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MethodDefinition::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::Async(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::Getter(name, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MethodDefinition::Setter(name, args, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut head = |pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}MethodDefinition: {}", first, self).and(Ok(successive))
        };
        match self {
            MethodDefinition::NamedFunction(name, args, body) => {
                let successive = head(pad, state)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
            MethodDefinition::Generator(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::AsyncGenerator(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::Async(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::Getter(name, body) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "get", TokenType::Keyword, &successive, Spot::NotFinal)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
            MethodDefinition::Setter(name, args, body) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "set", TokenType::Keyword, &successive, Spot::NotFinal)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl MethodDefinition {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("MethodDefinition expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_get = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Get)?;
                let (pn, after_pn) = PropertyName::parse(parser, after_get, yield_flag, await_flag)?;
                let after_open = scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let after_close = scan_for_punct(after_open, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb = scan_for_punct(after_close, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Box::new(MethodDefinition::Getter(pn, body)), after_rb))
            })
            .otherwise(|| {
                let after_set = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Set)?;
                let (pn, after_pn) = PropertyName::parse(parser, after_set, yield_flag, await_flag)?;
                let after_open = scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (args, after_args) = PropertySetParameterList::parse(parser, after_open)?;
                let after_close = scan_for_punct(after_args, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb = scan_for_punct(after_close, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Box::new(MethodDefinition::Setter(pn, args, body)), after_rb))
            })
            .otherwise(|| AsyncMethod::parse(parser, scanner, yield_flag, await_flag).map(|(node, scan)| (Box::new(MethodDefinition::Async(node)), scan)))
            .otherwise(|| AsyncGeneratorMethod::parse(parser, scanner, yield_flag, await_flag).map(|(node, scan)| (Box::new(MethodDefinition::AsyncGenerator(node)), scan)))
            .otherwise(|| GeneratorMethod::parse(parser, scanner, yield_flag, await_flag).map(|(node, scan)| (Box::new(MethodDefinition::Generator(node)), scan)))
            .otherwise(|| {
                let (name, after_name) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                let after_lp = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (ufp, after_ufp) = UniqueFormalParameters::parse(parser, after_lp, false, false);
                let after_rp = scan_for_punct(after_ufp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Box::new(MethodDefinition::NamedFunction(name, ufp, body)), after_rb))
            })
    }
}

// PropertySetParameterList :
//      FormalParameter[~Yield, ~Await]
#[derive(Debug)]
pub struct PropertySetParameterList {
    node: Box<FormalParameter>,
}

impl fmt::Display for PropertySetParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.node.fmt(f)
    }
}

impl PrettyPrint for PropertySetParameterList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertySetParameterList: {}", first, self)?;
        self.node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.node.concise_with_leftpad(writer, pad, state)
    }
}

impl PropertySetParameterList {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        FormalParameter::parse(parser, scanner, false, false).map(|(node, scanner)| (Box::new(PropertySetParameterList { node }), scanner))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // METHOD DEFINITION
    #[test]
    fn method_definition_test_01() {
        let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(&*pn, MethodDefinition::NamedFunction(..)));
        pretty_check(&*pn, "MethodDefinition: a ( b ) { c ; }", vec!["PropertyName: a", "UniqueFormalParameters: b", "FunctionBody: c ;"]);
        concise_check(
            &*pn,
            "MethodDefinition: a ( b ) { c ; }",
            vec!["IdentifierName: a", "Punctuator: (", "IdentifierName: b", "Punctuator: )", "Punctuator: {", "ExpressionStatement: c ;", "Punctuator: }"],
        );
        format!("{:?}", pn);
    }
    #[test]
    fn method_definition_test_02() {
        let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false));
        chk_scan(&scanner, 21);
        assert!(matches!(&*pn, MethodDefinition::Getter(..)));
        pretty_check(&*pn, "MethodDefinition: get a ( ) { return 1 ; }", vec!["PropertyName: a", "FunctionBody: return 1 ;"]);
        concise_check(
            &*pn,
            "MethodDefinition: get a ( ) { return 1 ; }",
            vec!["Keyword: get", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "ReturnStatement: return 1 ;", "Punctuator: }"],
        );
        format!("{:?}", pn);
    }
    #[test]
    fn method_definition_test_03() {
        let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false));
        chk_scan(&scanner, 28);
        assert!(matches!(&*pn, MethodDefinition::Setter(..)));
        pretty_check(&*pn, "MethodDefinition: set a ( blue ) { this . a = blue ; }", vec!["PropertyName: a", "PropertySetParameterList: blue", "FunctionBody: this . a = blue ;"]);
        concise_check(
            &*pn,
            "MethodDefinition: set a ( blue ) { this . a = blue ; }",
            vec!["Keyword: set", "IdentifierName: a", "Punctuator: (", "IdentifierName: blue", "Punctuator: )", "Punctuator: {", "ExpressionStatement: this . a = blue ;", "Punctuator: }"],
        );
        format!("{:?}", pn);
    }
    #[test]
    fn method_definition_test_errs_01() {
        check_err(MethodDefinition::parse(&mut newparser(""), Scanner::new(), false, false), "MethodDefinition expected", 1, 1);
    }
    #[test]
    fn method_definition_test_errs_02() {
        check_err(MethodDefinition::parse(&mut newparser("a"), Scanner::new(), false, false), "‘(’ expected", 1, 2);
    }
    #[test]
    fn method_definition_test_errs_03() {
        check_err(MethodDefinition::parse(&mut newparser("a("), Scanner::new(), false, false), "‘)’ expected", 1, 3);
    }
    #[test]
    fn method_definition_test_errs_04() {
        check_err(MethodDefinition::parse(&mut newparser("a(x"), Scanner::new(), false, false), "‘)’ expected", 1, 4);
    }
    #[test]
    fn method_definition_test_errs_05() {
        check_err(MethodDefinition::parse(&mut newparser("a(x,y)"), Scanner::new(), false, false), "‘{’ expected", 1, 7);
    }
    #[test]
    fn method_definition_test_errs_06() {
        check_err(MethodDefinition::parse(&mut newparser("a(x,y){"), Scanner::new(), false, false), "‘}’ expected", 1, 8);
    }
    #[test]
    fn method_definition_test_errs_07() {
        check_err(MethodDefinition::parse(&mut newparser("get"), Scanner::new(), false, false), "PropertyName expected", 1, 4);
    }
    #[test]
    fn method_definition_test_errs_08() {
        check_err(MethodDefinition::parse(&mut newparser("get a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
    }
    #[test]
    fn method_definition_test_errs_09() {
        check_err(MethodDefinition::parse(&mut newparser("get a("), Scanner::new(), false, false), "‘)’ expected", 1, 7);
    }
    #[test]
    fn method_definition_test_errs_10() {
        check_err(MethodDefinition::parse(&mut newparser("get a()"), Scanner::new(), false, false), "‘{’ expected", 1, 8);
    }
    #[test]
    fn method_definition_test_errs_11() {
        check_err(MethodDefinition::parse(&mut newparser("get a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 9);
    }
    #[test]
    fn method_definition_test_errs_12() {
        check_err(MethodDefinition::parse(&mut newparser("set"), Scanner::new(), false, false), "PropertyName expected", 1, 4);
    }
    #[test]
    fn method_definition_test_errs_13() {
        check_err(MethodDefinition::parse(&mut newparser("set a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
    }
    #[test]
    fn method_definition_test_errs_14() {
        check_err(MethodDefinition::parse(&mut newparser("set a("), Scanner::new(), false, false), "BindingElement expected", 1, 7);
    }
    #[test]
    fn method_definition_test_errs_15() {
        check_err(MethodDefinition::parse(&mut newparser("set a()"), Scanner::new(), false, false), "BindingElement expected", 1, 7);
    }
    #[test]
    fn method_definition_test_errs_16() {
        check_err(MethodDefinition::parse(&mut newparser("set a(h)"), Scanner::new(), false, false), "‘{’ expected", 1, 9);
    }
    #[test]
    fn method_definition_test_errs_17() {
        check_err(MethodDefinition::parse(&mut newparser("set a(h){"), Scanner::new(), false, false), "‘}’ expected", 1, 10);
    }
    #[test]
    fn method_definition_test_errs_18() {
        check_err(MethodDefinition::parse(&mut newparser("set a(h"), Scanner::new(), false, false), "‘)’ expected", 1, 8);
    }
    #[test]
    fn method_definition_test_prettyerrors_1() {
        let (item, _) = MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn method_definition_test_prettyerrors_2() {
        let (item, _) = MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn method_definition_test_prettyerrors_3() {
        let (item, _) = MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn method_definition_test_conciseerrors_1() {
        let (item, _) = MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn method_definition_test_conciseerrors_2() {
        let (item, _) = MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn method_definition_test_conciseerrors_3() {
        let (item, _) = MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
}

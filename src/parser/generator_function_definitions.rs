use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::primary_expressions::PropertyName;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// GeneratorMethod[Yield, Await] :
//      * PropertyName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorMethod {
    name: Box<PropertyName>,
    params: Box<UniqueFormalParameters>,
    body: Box<GeneratorBody>,
}

impl fmt::Display for GeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "* {} ( {} ) {{ {} }}", self.name, self.params, self.body)
    }
}

impl PrettyPrint for GeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorMethod: {}", first, self)?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorMethod: {}", first, self)?;
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

impl GeneratorMethod {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_star = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Star)?;
        let (name, after_name) = PropertyName::parse(parser, after_star, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(GeneratorMethod { name, params, body }), after_rb))
    }
}

// GeneratorDeclaration[Yield, Await, Default] :
//      function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
//      [+Default] function * ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorDeclaration {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<GeneratorBody>,
}

impl fmt::Display for GeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for GeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}GeneratorDeclaration: {}", first, self)?;
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

impl GeneratorDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
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
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(GeneratorDeclaration { ident, params, body }), after_rb))
    }
}

// GeneratorExpression :
//      function * BindingIdentifier[+Yield, ~Await]opt ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorExpression {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<GeneratorBody>,
}

impl fmt::Display for GeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for GeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorExpression: {}", first, self)?;
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
        writeln!(writer, "{}GeneratorExpression: {}", first, self)?;
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

impl IsFunctionDefinition for GeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl GeneratorExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, true, false) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(GeneratorExpression { ident, params, body }), after_rb))
    }
}

// GeneratorBody :
//      FunctionBody[+Yield, ~Await]
#[derive(Debug)]
pub struct GeneratorBody(Box<FunctionBody>);

impl fmt::Display for GeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for GeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl GeneratorBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Box<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, true, false);
        (Box::new(GeneratorBody(fb)), after_fb)
    }
}

// YieldExpression[In, Await] :
//      yield
//      yield [no LineTerminator here] AssignmentExpression[?In, +Yield, ?Await]
//      yield [no LineTerminator here] * AssignmentExpression[?In, +Yield, ?Await]
#[derive(Debug)]
pub enum YieldExpression {
    Simple,
    Expression(Box<AssignmentExpression>),
    From(Box<AssignmentExpression>),
}

impl fmt::Display for YieldExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            YieldExpression::Simple => f.write_str("yield"),
            YieldExpression::Expression(node) => write!(f, "yield {}", node),
            YieldExpression::From(node) => write!(f, "yield * {}", node),
        }
    }
}

impl PrettyPrint for YieldExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}YieldExpression: {}", first, self)?;
        match self {
            YieldExpression::Simple => Ok(()),
            YieldExpression::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            YieldExpression::From(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}YieldExpression: {}", first, self).and(Ok(successive))
        };
        match self {
            YieldExpression::Simple => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            YieldExpression::Expression(node) => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            YieldExpression::From(node) => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl YieldExpression {
    fn parse_after_nlt(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new(String::new(), scanner.line, scanner.column))
            .otherwise(|| {
                let after_star = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Star)?;
                let (ae, after_ae) = AssignmentExpression::parse(parser, after_star, in_flag, true, await_flag)?;
                Ok((Box::new(YieldExpression::From(ae)), after_ae))
            })
            .otherwise(|| {
                let (ae, after_ae) = AssignmentExpression::parse(parser, scanner, in_flag, true, await_flag)?;
                Ok((Box::new(YieldExpression::Expression(ae)), after_ae))
            })
    }
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_yield = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Yield)?;
        no_line_terminator(after_yield, parser.source)
            .and_then(|()| Self::parse_after_nlt(parser, after_yield, in_flag, await_flag))
            .otherwise(|| Ok((Box::new(YieldExpression::Simple), after_yield)))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // GENERATOR METHOD
    #[test]
    fn generator_method_test_01() {
        let (node, scanner) = check(GeneratorMethod::parse(&mut newparser("*a(){}"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "GeneratorMethod: * a (  ) {  }", vec!["PropertyName: a", "UniqueFormalParameters: ", "GeneratorBody: "]);
        concise_check(
            &*node,
            "GeneratorMethod: * a (  ) {  }",
            vec!["Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn generator_method_test_02() {
        check_err(GeneratorMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘*’ expected", 1, 1);
    }
    #[test]
    fn generator_method_test_03() {
        check_err(GeneratorMethod::parse(&mut newparser("*"), Scanner::new(), false, false), "PropertyName expected", 1, 2);
    }
    #[test]
    fn generator_method_test_04() {
        check_err(GeneratorMethod::parse(&mut newparser("*a"), Scanner::new(), false, false), "‘(’ expected", 1, 3);
    }
    #[test]
    fn generator_method_test_05() {
        check_err(GeneratorMethod::parse(&mut newparser("*a("), Scanner::new(), false, false), "‘)’ expected", 1, 4);
    }
    #[test]
    fn generator_method_test_06() {
        check_err(GeneratorMethod::parse(&mut newparser("*a()"), Scanner::new(), false, false), "‘{’ expected", 1, 5);
    }
    #[test]
    fn generator_method_test_07() {
        check_err(GeneratorMethod::parse(&mut newparser("*a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
    }
    #[test]
    fn generator_method_test_prettyerrors_1() {
        let (item, _) = GeneratorMethod::parse(&mut newparser("* bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_method_test_conciseerrors_1() {
        let (item, _) = GeneratorMethod::parse(&mut newparser("* bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // GENERATOR DECLARATION
    #[test]
    fn generator_declaration_test_01() {
        let (node, scanner) = check(GeneratorDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "GeneratorDeclaration: function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "]);
        concise_check(
            &*node,
            "GeneratorDeclaration: function * a (  ) {  }",
            vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn generator_declaration_test_02() {
        let (node, scanner) = check(GeneratorDeclaration::parse(&mut newparser("function *(){}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "GeneratorDeclaration: function * (  ) {  }", vec!["FormalParameters: ", "GeneratorBody: "]);
        concise_check(&*node, "GeneratorDeclaration: function * (  ) {  }", vec!["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn generator_declaration_test_03() {
        check_err(GeneratorDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘function’ expected", 1, 1);
    }
    #[test]
    fn generator_declaration_test_04() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true), "‘*’ expected", 1, 9);
    }
    #[test]
    fn generator_declaration_test_05() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function *"), Scanner::new(), false, false, true), "‘(’ expected", 1, 11);
    }
    #[test]
    fn generator_declaration_test_06() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h"), Scanner::new(), false, false, true), "‘(’ expected", 1, 13);
    }
    #[test]
    fn generator_declaration_test_07() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 15);
    }
    #[test]
    fn generator_declaration_test_075() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 13);
    }
    #[test]
    fn generator_declaration_test_076() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, false), "Not an identifier", 1, 11);
    }
    #[test]
    fn generator_declaration_test_08() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u"), Scanner::new(), false, false, true), "‘)’ expected", 1, 17);
    }
    #[test]
    fn generator_declaration_test_09() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u )"), Scanner::new(), false, false, true), "‘{’ expected", 1, 19);
    }
    #[test]
    fn generator_declaration_test_10() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u ) {"), Scanner::new(), false, false, true), "‘}’ expected", 1, 21);
    }
    #[test]
    fn generator_declaration_test_11() {
        check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new(), false, false, true), "‘}’ expected", 1, 24);
    }
    #[test]
    fn generator_declaration_test_prettyerrors_1() {
        let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_declaration_test_prettyerrors_2() {
        let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_declaration_test_conciseerrors_1() {
        let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn generator_declaration_test_conciseerrors_2() {
        let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(*item);
    }

    // GENERATOR EXPRESSION
    #[test]
    fn generator_expression_test_01() {
        let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *a(){}"), Scanner::new()));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "GeneratorExpression: function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "]);
        concise_check(
            &*node,
            "GeneratorExpression: function * a (  ) {  }",
            vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn generator_expression_test_02() {
        let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *(){}"), Scanner::new()));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "GeneratorExpression: function * (  ) {  }", vec!["FormalParameters: ", "GeneratorBody: "]);
        concise_check(&*node, "GeneratorExpression: function * (  ) {  }", vec!["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
        format!("{:?}", node);
        assert!(node.is_function_definition());
    }
    #[test]
    fn generator_expression_test_03() {
        check_err(GeneratorExpression::parse(&mut newparser(""), Scanner::new()), "‘function’ expected", 1, 1);
    }
    #[test]
    fn generator_expression_test_04() {
        check_err(GeneratorExpression::parse(&mut newparser("function"), Scanner::new()), "‘*’ expected", 1, 9);
    }
    #[test]
    fn generator_expression_test_05() {
        check_err(GeneratorExpression::parse(&mut newparser("function *"), Scanner::new()), "‘(’ expected", 1, 11);
    }
    #[test]
    fn generator_expression_test_06() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h"), Scanner::new()), "‘(’ expected", 1, 13);
    }
    #[test]
    fn generator_expression_test_07() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h ("), Scanner::new()), "‘)’ expected", 1, 15);
    }
    #[test]
    fn generator_expression_test_08() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h ( u"), Scanner::new()), "‘)’ expected", 1, 17);
    }
    #[test]
    fn generator_expression_test_09() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h ( u )"), Scanner::new()), "‘{’ expected", 1, 19);
    }
    #[test]
    fn generator_expression_test_10() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h ( u ) {"), Scanner::new()), "‘}’ expected", 1, 21);
    }
    #[test]
    fn generator_expression_test_11() {
        check_err(GeneratorExpression::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new()), "‘}’ expected", 1, 24);
    }
    #[test]
    fn generator_expression_test_prettyerrors_1() {
        let (item, _) = GeneratorExpression::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_expression_test_prettyerrors_2() {
        let (item, _) = GeneratorExpression::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_expression_test_conciseerrors_1() {
        let (item, _) = GeneratorExpression::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn generator_expression_test_conciseerrors_2() {
        let (item, _) = GeneratorExpression::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }

    // GENERATOR BODY
    #[test]
    fn generator_body_test_01() {
        let (node, scanner) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        chk_scan(&scanner, 8);
        pretty_check(&*node, "GeneratorBody: yield 1 ;", vec!["FunctionBody: yield 1 ;"]);
        concise_check(&*node, "ExpressionStatement: yield 1 ;", vec!["YieldExpression: yield 1", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn generator_body_test_prettyerrors_1() {
        let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        pretty_error_validate(*item);
    }
    #[test]
    fn generator_body_test_conciseerrors_1() {
        let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
        concise_error_validate(*item);
    }

    // YIELD EXPRESSION
    #[test]
    fn yield_expression_test_01() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*pn, YieldExpression::Simple));
        pretty_check(&*pn, "YieldExpression: yield", vec![]);
        concise_check(&*pn, "Keyword: yield", vec![]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_02() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield 5"), Scanner::new(), true, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*pn, YieldExpression::Expression(..)));
        pretty_check(&*pn, "YieldExpression: yield 5", vec!["AssignmentExpression: 5"]);
        concise_check(&*pn, "YieldExpression: yield 5", vec!["Keyword: yield", "Numeric: 5"]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_03() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *5"), Scanner::new(), true, false));
        chk_scan(&scanner, 8);
        assert!(matches!(&*pn, YieldExpression::From(..)));
        pretty_check(&*pn, "YieldExpression: yield * 5", vec!["AssignmentExpression: 5"]);
        concise_check(&*pn, "YieldExpression: yield * 5", vec!["Keyword: yield", "Punctuator: *", "Numeric: 5"]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_04() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield \n*5"), Scanner::new(), true, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*pn, YieldExpression::Simple));
        pretty_check(&*pn, "YieldExpression: yield", vec![]);
        concise_check(&*pn, "Keyword: yield", vec![]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_05() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield @"), Scanner::new(), true, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*pn, YieldExpression::Simple));
        pretty_check(&*pn, "YieldExpression: yield", vec![]);
        concise_check(&*pn, "Keyword: yield", vec![]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_06() {
        let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *@"), Scanner::new(), true, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*pn, YieldExpression::Simple));
        pretty_check(&*pn, "YieldExpression: yield", vec![]);
        concise_check(&*pn, "Keyword: yield", vec![]);
        format!("{:?}", pn);
    }
    #[test]
    fn yield_expression_test_07() {
        check_err(YieldExpression::parse(&mut newparser(""), Scanner::new(), true, false), "‘yield’ expected", 1, 1);
    }
    #[test]
    fn yield_expression_test_prettyerrors_1() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn yield_expression_test_prettyerrors_2() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn yield_expression_test_prettyerrors_3() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn yield_expression_test_conciseerrors_1() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn yield_expression_test_conciseerrors_2() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn yield_expression_test_conciseerrors_3() {
        let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
        concise_error_validate(*item);
    }
}

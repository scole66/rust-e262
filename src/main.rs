#![allow(dead_code, unused_variables)]

use num::bigint::BigInt;
use std::env;
use std::io::{self, Write};

mod scanner;

#[derive(Debug, PartialEq)]
pub enum ParseGoal {
    Script,
    Module,
}

pub struct Parser<'a> {
    source: &'a str,
    scanner: Scanner,
    strict: bool,
    goal: ParseGoal,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, strict: bool, goal: ParseGoal) -> Self {
        Self {
            source,
            scanner: Scanner::new(),
            strict,
            goal,
        }
    }
}

pub trait StringValue {
    fn string_value(&self) -> scanner::JSString;
}

pub trait BoundNames {
    fn bound_names(&self) -> Vec<scanner::JSString>;
}

pub trait HasName {
    fn has_name(&self) -> bool;
}

pub trait IsFunctionDefinition {
    fn is_function_definition(&self) -> bool;
}

pub trait IsIdentifierReference {
    fn is_identifier_reference(&self) -> bool;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ATTKind {
    Invalid,
    Simple,
}
pub trait AssignmentTargetType {
    fn assignment_target_type(&self) -> ATTKind;
}

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub struct Identifier {
    identifier_name: scanner::IdentifierData,
}

impl StringValue for Identifier {
    fn string_value(&self) -> scanner::JSString {
        self.identifier_name.string_value.clone()
    }
}

fn identifier(parser: &mut Parser) -> Result<Option<(Box<Identifier>, Scanner)>, String> {
    let tok = scanner::scan_token(
        &parser.scanner,
        parser.source,
        scanner::ScanGoal::InputElementRegExp,
    );
    match tok {
        Err(err) => Err(err),
        Ok(tpl) => match tpl.0 {
            scanner::Token::Identifier(id) => match id.keyword_id {
                Some(scanner::Keyword::Await)
                | Some(scanner::Keyword::Break)
                | Some(scanner::Keyword::Case)
                | Some(scanner::Keyword::Catch)
                | Some(scanner::Keyword::Class)
                | Some(scanner::Keyword::Const)
                | Some(scanner::Keyword::Continue)
                | Some(scanner::Keyword::Debugger)
                | Some(scanner::Keyword::Default)
                | Some(scanner::Keyword::Delete)
                | Some(scanner::Keyword::Do)
                | Some(scanner::Keyword::Else)
                | Some(scanner::Keyword::Enum)
                | Some(scanner::Keyword::Export)
                | Some(scanner::Keyword::Extends)
                | Some(scanner::Keyword::False)
                | Some(scanner::Keyword::Finally)
                | Some(scanner::Keyword::For)
                | Some(scanner::Keyword::Function)
                | Some(scanner::Keyword::If)
                | Some(scanner::Keyword::Import)
                | Some(scanner::Keyword::In)
                | Some(scanner::Keyword::Instanceof)
                | Some(scanner::Keyword::New)
                | Some(scanner::Keyword::Null)
                | Some(scanner::Keyword::Return)
                | Some(scanner::Keyword::Super)
                | Some(scanner::Keyword::Switch)
                | Some(scanner::Keyword::This)
                | Some(scanner::Keyword::Throw)
                | Some(scanner::Keyword::True)
                | Some(scanner::Keyword::Try)
                | Some(scanner::Keyword::Typeof)
                | Some(scanner::Keyword::Var)
                | Some(scanner::Keyword::Void)
                | Some(scanner::Keyword::While)
                | Some(scanner::Keyword::With)
                | Some(scanner::Keyword::Yield) => Ok(None),
                _ => {
                    if parser.strict
                        && (id.string_value == "implements"
                            || id.string_value == "interface"
                            || id.string_value == "let"
                            || id.string_value == "package"
                            || id.string_value == "private"
                            || id.string_value == "protected"
                            || id.string_value == "public"
                            || id.string_value == "static"
                            || id.string_value == "yield")
                    {
                        Err(format!(
                            "{}:{}: ‘{}’ not allowed as an identifier in strict mode",
                            id.line, id.column, id.string_value
                        ))
                    } else if parser.goal == ParseGoal::Module && id.string_value == "await" {
                        Err(format!(
                            "{}:{}: ‘await’ not allowed as an identifier in modules",
                            id.line, id.column
                        ))
                    } else if id.string_value == "break"
                        || id.string_value == "case"
                        || id.string_value == "catch"
                        || id.string_value == "class"
                        || id.string_value == "const"
                        || id.string_value == "continue"
                        || id.string_value == "debugger"
                        || id.string_value == "default"
                        || id.string_value == "delete"
                        || id.string_value == "do"
                        || id.string_value == "else"
                        || id.string_value == "enum"
                        || id.string_value == "export"
                        || id.string_value == "extends"
                        || id.string_value == "false"
                        || id.string_value == "finally"
                        || id.string_value == "for"
                        || id.string_value == "function"
                        || id.string_value == "if"
                        || id.string_value == "import"
                        || id.string_value == "in"
                        || id.string_value == "instanceof"
                        || id.string_value == "new"
                        || id.string_value == "null"
                        || id.string_value == "return"
                        || id.string_value == "super"
                        || id.string_value == "switch"
                        || id.string_value == "this"
                        || id.string_value == "throw"
                        || id.string_value == "true"
                        || id.string_value == "try"
                        || id.string_value == "typeof"
                        || id.string_value == "var"
                        || id.string_value == "void"
                        || id.string_value == "while"
                        || id.string_value == "with"
                    {
                        Err(format!(
                            "{}:{}: ‘{}’ is a reserved word and may not be used as an identifier",
                            id.line, id.column, id.string_value
                        ))
                    } else {
                        let node = Identifier {
                            identifier_name: id,
                        };
                        let boxed = Box::new(node);
                        Ok(Some((boxed, tpl.1)))
                    }
                }
            },
            _ => Ok(None),
        },
    }
}

// IdentifierReference[Yield, Await]:
//      Identifier
//      [~Yield]yield
//      [~Await]await

#[derive(Debug)]
enum IdentifierReferenceKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct IdentifierReference {
    kind: IdentifierReferenceKind,
    strict: bool,
}

impl StringValue for IdentifierReference {
    fn string_value(&self) -> scanner::JSString {
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from_str("yield"),
            Await => scanner::JSString::from_str("await"),
        }
    }
}

impl AssignmentTargetType for IdentifierReference {
    fn assignment_target_type(&self) -> ATTKind {
        use ATTKind::*;
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => {
                if self.strict {
                    let sv = id.string_value();
                    if sv == "eval" || sv == "arguments" {
                        Invalid
                    } else {
                        Simple
                    }
                } else {
                    Simple
                }
            }
            Await | Yield => Simple,
        }
    }
}

fn identifier_reference(
    parser: &mut Parser,
    arg_yield: bool,
    arg_await: bool,
) -> Result<Option<(Box<IdentifierReference>, Scanner)>, String> {
    let production = identifier(parser)?;
    use IdentifierReferenceKind::*;
    match production {
        Some((ident, scanner)) => {
            let node = IdentifierReference {
                kind: Identifier(ident),
                strict: parser.strict,
            };
            let boxed = Box::new(node);
            Ok(Some((boxed, scanner)))
        }
        None => {
            let (token, scan) = scanner::scan_token(
                &parser.scanner,
                parser.source,
                scanner::ScanGoal::InputElementRegExp,
            )?;
            match token {
                scanner::Token::Identifier(id) => match id.keyword_id {
                    Some(scanner::Keyword::Await) => {
                        if !arg_await {
                            Ok(Some((
                                Box::new(IdentifierReference {
                                    kind: Await,
                                    strict: parser.strict,
                                }),
                                scan,
                            )))
                        } else {
                            Ok(None)
                        }
                    }
                    Some(scanner::Keyword::Yield) => {
                        if !arg_yield {
                            Ok(Some((
                                Box::new(IdentifierReference {
                                    kind: Yield,
                                    strict: parser.strict,
                                }),
                                scan,
                            )))
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Ok(None),
                },
                _ => Ok(None),
            }
        }
    }
}

// BindingIdentifier[Yield, Await] :
//    Identifier
//    yield
//    await
#[derive(Debug)]
enum BindingIdentifierKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
struct BindingIdentifier {
    kind: BindingIdentifierKind,
    yield_flag: bool,
    await_flag: bool,
}

impl StringValue for BindingIdentifier {
    fn string_value(&self) -> scanner::JSString {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from_str("yield"),
            Await => scanner::JSString::from_str("await"),
        }
    }
}

impl BoundNames for BindingIdentifier {
    fn bound_names(&self) -> Vec<scanner::JSString> {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => vec![id.string_value()],
            Yield => vec![scanner::JSString::from_str("yield")],
            Await => vec![scanner::JSString::from_str("await")],
        }
    }
}

fn binding_identifier(
    parser: &mut Parser,
    yield_flag: bool,
    await_flag: bool,
) -> Result<Option<(Box<BindingIdentifier>, Scanner)>, String> {
    let production = identifier(parser)?;
    match production {
        Some((ident, scanner)) => {
            let node = BindingIdentifier {
                kind: BindingIdentifierKind::Identifier(ident),
                yield_flag,
                await_flag,
            };
            let boxed = Box::new(node);
            Ok(Some((boxed, scanner)))
        }
        None => {
            let (token, scan) = scanner::scan_token(
                &parser.scanner,
                parser.source,
                scanner::ScanGoal::InputElementRegExp,
            )?;
            match token {
                scanner::Token::Identifier(id) => match id.keyword_id {
                    Some(scanner::Keyword::Await) => Ok(Some((
                        Box::new(BindingIdentifier {
                            kind: BindingIdentifierKind::Await,
                            yield_flag,
                            await_flag,
                        }),
                        scan,
                    ))),
                    Some(scanner::Keyword::Yield) => Ok(Some((
                        Box::new(BindingIdentifier {
                            kind: BindingIdentifierKind::Yield,
                            yield_flag,
                            await_flag,
                        }),
                        scan,
                    ))),
                    _ => Ok(None),
                },
                _ => Ok(None),
            }
        }
    }
}

//////// 12.2 Primary Expression
// PrimaryExpression[Yield, Await] :
//      this
//      IdentifierReference[?Yield, ?Await]
//      Literal
//      ArrayLiteral[?Yield, ?Await]
//      ObjectLiteral[?Yield, ?Await]
//      FunctionExpression
//      ClassExpression[?Yield, ?Await]
//      GeneratorExpression
//      AsyncFunctionExpression
//      AsyncGeneratorExpression
//      RegularExpressionLiteral
//      TemplateLiteral[?Yield, ?Await, ~Tagged]
//      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]

#[derive(Debug)]
pub enum PrimaryExpressionKind {
    This,
    IdentifierReference(Box<IdentifierReference>),
    Literal(Box<Literal>),
    // More to come
}

#[derive(Debug)]
pub struct PrimaryExpression {
    kind: PrimaryExpressionKind,
}

impl IsFunctionDefinition for PrimaryExpression {
    fn is_function_definition(&self) -> bool {
        use PrimaryExpressionKind::*;
        match self.kind {
            This | IdentifierReference(_) | Literal(_) => false,
        }
    }
}

impl IsIdentifierReference for PrimaryExpression {
    fn is_identifier_reference(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) => false,
            IdentifierReference(_) => true,
        }
    }
}

impl AssignmentTargetType for PrimaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) => ATTKind::Invalid,
            IdentifierReference(id) => id.assignment_target_type(),
        }
    }
}

fn primary_expression(
    parser: &mut Parser,
    arg_yield: bool,
    arg_await: bool,
) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String> {
    let idref = identifier_reference(parser, arg_yield, arg_await)?;
    if let Some((idrefbox, new_scanner)) = idref {
        let node = PrimaryExpression {
            kind: PrimaryExpressionKind::IdentifierReference(idrefbox),
        };
        let boxed = Box::new(node);
        return Ok(Some((boxed, new_scanner)));
    }
    let lit = literal(parser)?;
    if let Some((litbox, new_scanner)) = lit {
        let node = PrimaryExpression {
            kind: PrimaryExpressionKind::Literal(litbox),
        };
        let boxed = Box::new(node);
        return Ok(Some((boxed, new_scanner)));
    }
    let tok = scanner::scan_token(
        &parser.scanner,
        parser.source,
        scanner::ScanGoal::InputElementRegExp,
    )?;
    if let (scanner::Token::Identifier(id), newscanner) = tok {
        if let Some(kwd_id) = id.keyword_id {
            if kwd_id == scanner::Keyword::This {
                let node = PrimaryExpression {
                    kind: PrimaryExpressionKind::This,
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
        }
    }
    Ok(None)
}

//////// 12.2.4 Literals
// Literal :
//      NullLiteral
//      BooleanLiteral
//      NumericLiteral
//      StringLiteral
#[derive(Debug)]
pub enum Numeric {
    Number(f64),
    BigInt(BigInt),
}
#[derive(Debug)]
pub enum LiteralKind {
    NullLiteral,
    BooleanLiteral(bool),
    NumericLiteral(Numeric),
    StringLiteral(scanner::JSString),
}
#[derive(Debug)]
pub struct Literal {
    kind: LiteralKind,
}

fn literal(parser: &mut Parser) -> Result<Option<(Box<Literal>, Scanner)>, String> {
    let scan_result = scanner::scan_token(
        &parser.scanner,
        parser.source,
        scanner::ScanGoal::InputElementRegExp,
    )?;
    let (token, newscanner) = scan_result;
    match token {
        scanner::Token::Identifier(id) => match id.keyword_id {
            Some(scanner::Keyword::Null) => {
                let node = Literal {
                    kind: LiteralKind::NullLiteral,
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            Some(scanner::Keyword::True) => {
                let node = Literal {
                    kind: LiteralKind::BooleanLiteral(true),
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            Some(scanner::Keyword::False) => {
                let node = Literal {
                    kind: LiteralKind::BooleanLiteral(false),
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            _ => return Ok(None),
        },
        scanner::Token::Number(num) => {
            let node = Literal {
                kind: LiteralKind::NumericLiteral(Numeric::Number(num)),
            };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        scanner::Token::BigInt(bi) => {
            let node = Literal {
                kind: LiteralKind::NumericLiteral(Numeric::BigInt(bi)),
            };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        scanner::Token::String(s) => {
            let node = Literal {
                kind: LiteralKind::StringLiteral(s),
            };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        _ => return Ok(None),
    }
}

//////// 13.2 Block

// StatementList[Yield, Await, Return]:
//    StatementListItem[?Yield, ?Await, ?Return]
//    StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
struct StatementlistStatementlistitem {
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
struct StatementlistStatementlistStatementlistitem {
    statement_list: Box<StatementList>,
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
enum StatementlistProduction {
    StatementListItem(StatementlistStatementlistitem),
    StatementListStatmentListItem(StatementlistStatementlistStatementlistitem),
}
#[derive(Debug)]
struct StatementList {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: Box<StatementlistProduction>,
}

// StatementListItem[Yield, Await, Return]:
//    Statement[?Yield, ?Await, ?Return]
//    Declaration[?Yield, ?Await]
#[derive(Debug)]
struct StatementlistitemStatement {
    statement: Box<Statement>,
}
#[derive(Debug)]
struct StatementlistitemDeclaration {
    declaration: Box<Declaration>,
}
#[derive(Debug)]
enum StatementlistitemProduction {
    Statement(StatementlistitemStatement),
    Declaration(StatementlistitemDeclaration),
}
#[derive(Debug)]
struct StatementListItem {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: StatementlistitemProduction,
}

// tbd
#[derive(Debug)]
struct Statement {
    faux: String,
}
#[derive(Debug)]
struct Declaration {
    faux: String,
}

//////// 15.1 Scripts

// Script:
//    ScriptBody[opt]
#[derive(Debug)]
struct Script {
    script_body: Option<Box<ScriptBody>>,
}

// ScriptBody:
//    StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
struct ScriptBody {
    statement_list: Box<StatementList>,
}

#[derive(Debug)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between execution contexts winds up here.
}

impl VM {
    fn new() -> VM {
        VM {}
    }

    fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
        Ok(0)
    }

    fn run(&mut self) -> Result<i32, String> {
        Ok(0)
    }
}

use crate::scanner::Scanner;

fn script<'a>(scanner: &'a mut Scanner) -> Result<Box<Script>, String> {
    //let ch = scanner.scan_token(ScanGoal::InputElementRegExp);
    Ok(Box::new(Script { script_body: None }))
}

#[derive(Debug)]
struct AST {
    script: Box<Script>,
}
impl AST {
    fn generate(source: &str) -> Result<Box<AST>, String> {
        let mut scanner = Scanner::new();
        script(&mut scanner).and_then(|s| Ok(Box::new(AST { script: s })))
    }
}

fn generate_ast<'a>(_vm: &'a VM, source: &'a str) -> Result<Box<AST>, String> {
    AST::generate(source)
}

fn interpret(vm: &mut VM, source: &str) -> Result<i32, String> {
    //generate_ast(vm, source).and_then(|ast| vm.compile(&ast)).and_then(|_| vm.run())
    // let result = scanner::scan_token(
    //     &Scanner::new(),
    //     source,
    //     scanner::ScanGoal::InputElementRegExp,
    // );
    let mut parser = Parser::new(source, false, ParseGoal::Script);
    let result = primary_expression(&mut parser, false, false);
    println!("{:#?}", result);
    match result {
        Ok(_) => Ok(0),
        Err(msg) => Err(msg),
    }
}

fn repl(vm: &mut VM) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        let linelen = line.len();
        if linelen == 0 {
            println!("");
            break;
        }

        println!("You entered the string {:?}", line);
        match interpret(vm, &line) {
            Ok(value) => println!("{}", value),
            Err(err) => println!("{}", err),
        }
    }
}

fn run_file(_vm: &mut VM, fname: &str) {
    println!("Running from the file {}", fname);
}

fn run_app() -> Result<(), i32> {
    let mut vm: VM = VM::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("Usage: {} [path]", &args[0]);
            return Err(2);
        }
    }

    Ok(())
}

fn main() {
    std::process::exit(match run_app() {
        Ok(_) => 0,
        Err(err) => err,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    fn id_kwd_test(kwd: &str) {
        let result = super::identifier(&mut super::Parser::new(
            kwd,
            false,
            super::ParseGoal::Script,
        ));
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    #[test]
    fn identifier_test_await() {
        id_kwd_test("await")
    }
    #[test]
    fn identifier_test_break() {
        id_kwd_test("break")
    }
    #[test]
    fn identifier_test_case() {
        id_kwd_test("case")
    }
    #[test]
    fn identifier_test_catch() {
        id_kwd_test("catch")
    }
    #[test]
    fn identifier_test_class() {
        id_kwd_test("class")
    }
    #[test]
    fn identifier_test_const() {
        id_kwd_test("const")
    }
    #[test]
    fn identifier_test_continue() {
        id_kwd_test("continue")
    }
    #[test]
    fn identifier_test_debugger() {
        id_kwd_test("debugger")
    }
    #[test]
    fn identifier_test_default() {
        id_kwd_test("default")
    }
    #[test]
    fn identifier_test_delete() {
        id_kwd_test("delete")
    }
    #[test]
    fn identifier_test_do() {
        id_kwd_test("do")
    }
    #[test]
    fn identifier_test_else() {
        id_kwd_test("else")
    }
    #[test]
    fn identifier_test_enum() {
        id_kwd_test("enum")
    }
    #[test]
    fn identifier_test_export() {
        id_kwd_test("export")
    }
    #[test]
    fn identifier_test_extends() {
        id_kwd_test("extends")
    }
    #[test]
    fn identifier_test_false() {
        id_kwd_test("false")
    }
    #[test]
    fn identifier_test_finally() {
        id_kwd_test("finally")
    }
    #[test]
    fn identifier_test_for() {
        id_kwd_test("for")
    }
    #[test]
    fn identifier_test_function() {
        id_kwd_test("function")
    }
    #[test]
    fn identifier_test_if() {
        id_kwd_test("if")
    }
    #[test]
    fn identifier_test_import() {
        id_kwd_test("import")
    }
    #[test]
    fn identifier_test_in() {
        id_kwd_test("in")
    }
    #[test]
    fn identifier_test_instanceof() {
        id_kwd_test("instanceof")
    }
    #[test]
    fn identifier_test_new() {
        id_kwd_test("new")
    }
    #[test]
    fn identifier_test_null() {
        id_kwd_test("null")
    }
    #[test]
    fn identifier_test_return() {
        id_kwd_test("return")
    }
    #[test]
    fn identifier_test_super() {
        id_kwd_test("super")
    }
    #[test]
    fn identifier_test_switch() {
        id_kwd_test("switch")
    }
    #[test]
    fn identifier_test_this() {
        id_kwd_test("this")
    }
    #[test]
    fn identifier_test_throw() {
        id_kwd_test("throw")
    }
    #[test]
    fn identifier_test_true() {
        id_kwd_test("true")
    }
    #[test]
    fn identifier_test_try() {
        id_kwd_test("try")
    }
    #[test]
    fn identifier_test_typeof() {
        id_kwd_test("typeof")
    }
    #[test]
    fn identifier_test_var() {
        id_kwd_test("var")
    }
    #[test]
    fn identifier_test_void() {
        id_kwd_test("void")
    }
    #[test]
    fn identifier_test_while() {
        id_kwd_test("while")
    }
    #[test]
    fn identifier_test_with() {
        id_kwd_test("with")
    }
    #[test]
    fn identifier_test_yield() {
        id_kwd_test("yield")
    }
    #[test]
    fn identifier_test_err() {
        let result = super::identifier(&mut super::Parser::new(
            "iden\\u{20}tifier",
            false,
            super::ParseGoal::Script,
        ));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "1:5: Invalid Identifier Continuation Character ' '"
        )
    }
    fn identifier_test_strict(kwd: &str) {
        let result =
            super::identifier(&mut super::Parser::new(kwd, true, super::ParseGoal::Script));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            format!("1:1: ‘{}’ not allowed as an identifier in strict mode", kwd)
        );
    }
    #[test]
    fn identifier_test_strict_implements() {
        identifier_test_strict("implements")
    }
    #[test]
    fn identifier_test_strict_interface() {
        identifier_test_strict("interface")
    }
    #[test]
    fn identifier_test_strict_let() {
        identifier_test_strict("let")
    }
    #[test]
    fn identifier_test_strict_package() {
        identifier_test_strict("package")
    }
    #[test]
    fn identifier_test_strict_private() {
        identifier_test_strict("private")
    }
    #[test]
    fn identifier_test_strict_protected() {
        identifier_test_strict("protected")
    }
    #[test]
    fn identifier_test_strict_public() {
        identifier_test_strict("public")
    }
    #[test]
    fn identifier_test_strict_static() {
        identifier_test_strict("static")
    }
    fn identifier_test_keyword(kwd: &str) {
        let firstch = kwd.chars().next().unwrap();
        let id_src = format!("\\u{{{:x}}}{}", firstch as u32, &kwd[firstch.len_utf8()..]);
        let result = super::identifier(&mut super::Parser::new(
            &id_src,
            false,
            super::ParseGoal::Script,
        ));
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            format!(
                "1:1: ‘{}’ is a reserved word and may not be used as an identifier",
                kwd
            )
        );
    }
    #[test]
    fn identifier_test_keyword_break() {
        identifier_test_keyword("break")
    }
    #[test]
    fn identifier_test_keyword_case() {
        identifier_test_keyword("case")
    }
    #[test]
    fn identifier_test_keyword_catch() {
        identifier_test_keyword("catch")
    }
    #[test]
    fn identifier_test_keyword_class() {
        identifier_test_keyword("class")
    }
    #[test]
    fn identifier_test_keyword_const() {
        identifier_test_keyword("const")
    }
    #[test]
    fn identifier_test_keyword_continue() {
        identifier_test_keyword("continue")
    }
    #[test]
    fn identifier_test_keyword_debugger() {
        identifier_test_keyword("debugger")
    }
    #[test]
    fn identifier_test_keyword_default() {
        identifier_test_keyword("default")
    }
    #[test]
    fn identifier_test_keyword_delete() {
        identifier_test_keyword("delete")
    }
    #[test]
    fn identifier_test_keyword_do() {
        identifier_test_keyword("do")
    }
    #[test]
    fn identifier_test_keyword_else() {
        identifier_test_keyword("else")
    }
    #[test]
    fn identifier_test_keyword_enum() {
        identifier_test_keyword("enum")
    }
    #[test]
    fn identifier_test_keyword_export() {
        identifier_test_keyword("export")
    }
    #[test]
    fn identifier_test_keyword_extends() {
        identifier_test_keyword("extends")
    }
    #[test]
    fn identifier_test_keyword_false() {
        identifier_test_keyword("false")
    }
    #[test]
    fn identifier_test_keyword_finally() {
        identifier_test_keyword("finally")
    }
    #[test]
    fn identifier_test_keyword_for() {
        identifier_test_keyword("for")
    }
    #[test]
    fn identifier_test_keyword_function() {
        identifier_test_keyword("function")
    }
    #[test]
    fn identifier_test_keyword_if() {
        identifier_test_keyword("if")
    }
    #[test]
    fn identifier_test_keyword_import() {
        identifier_test_keyword("import")
    }
    #[test]
    fn identifier_test_keyword_in() {
        identifier_test_keyword("in")
    }
    #[test]
    fn identifier_test_keyword_instanceof() {
        identifier_test_keyword("instanceof")
    }
    #[test]
    fn identifier_test_keyword_new() {
        identifier_test_keyword("new")
    }
    #[test]
    fn identifier_test_keyword_null() {
        identifier_test_keyword("null")
    }
    #[test]
    fn identifier_test_keyword_return() {
        identifier_test_keyword("return")
    }
    #[test]
    fn identifier_test_keyword_super() {
        identifier_test_keyword("super")
    }
    #[test]
    fn identifier_test_keyword_switch() {
        identifier_test_keyword("switch")
    }
    #[test]
    fn identifier_test_keyword_this() {
        identifier_test_keyword("this")
    }
    #[test]
    fn identifier_test_keyword_throw() {
        identifier_test_keyword("throw")
    }
    #[test]
    fn identifier_test_keyword_true() {
        identifier_test_keyword("true")
    }
    #[test]
    fn identifier_test_keyword_try() {
        identifier_test_keyword("try")
    }
    #[test]
    fn identifier_test_keyword_typeof() {
        identifier_test_keyword("typeof")
    }
    #[test]
    fn identifier_test_keyword_var() {
        identifier_test_keyword("var")
    }
    #[test]
    fn identifier_test_keyword_void() {
        identifier_test_keyword("void")
    }
    #[test]
    fn identifier_test_keyword_while() {
        identifier_test_keyword("while")
    }
    #[test]
    fn identifier_test_keyword_with() {
        identifier_test_keyword("with")
    }
    #[test]
    fn identifier_test_successful_bob() {
        let result = super::identifier(&mut super::Parser::new(
            "bob",
            true,
            super::ParseGoal::Script,
        ));
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(
            scanner,
            super::Scanner {
                line: 1,
                column: 4,
                start_idx: 3
            }
        );
        let data = identifier.identifier_name;
        assert_eq!(data.string_value, "bob");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }
    #[test]
    fn identifier_test_successful_japanese() {
        let text = "手がける黒田征太郎さんです";
        let result = super::identifier(&mut super::Parser::new(
            text,
            true,
            super::ParseGoal::Script,
        ));
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(
            scanner,
            super::Scanner {
                line: 1,
                column: 14,
                start_idx: 39
            }
        );
        let data = identifier.identifier_name;
        assert_eq!(data.string_value, "手がける黒田征太郎さんです");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }

    fn idref_create(text: &str, strict: bool) -> Box<IdentifierReference> {
        let yield_syntax = false;
        let await_syntax = false;
        let result = identifier_reference(
            &mut Parser::new(text, strict, ParseGoal::Script),
            yield_syntax,
            await_syntax,
        );
        assert!(result.is_ok());
        let optional_idref = result.unwrap();
        assert!(optional_idref.is_some());
        let (idref, scanner) = optional_idref.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len(),
            }
        );
        idref
    }

    #[test]
    fn idref_simple_success() {
        let idref = idref_create("identifier", false);
        assert_eq!(idref.strict, false);
        use IdentifierReferenceKind::*;
        match &idref.kind {
            Yield | Await => assert!(
                false,
                "Wrong IdentifierReference Kind (expected Identifier)"
            ),
            Identifier(_) => (),
        }

        assert_eq!(idref.string_value(), "identifier");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }

    #[test]
    fn idref_yield() {
        let idref = idref_create("yield", false);
        assert_eq!(idref.strict, false);
        use IdentifierReferenceKind::*;
        match &idref.kind {
            Await | Identifier(_) => {
                assert!(false, "Wrong IdentifierReference Kind (expected Yield)")
            }
            Yield => (),
        }

        assert_eq!(idref.string_value(), "yield");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }

    #[test]
    fn idref_await() {
        let idref = idref_create("await", false);
        assert_eq!(idref.strict, false);
        use IdentifierReferenceKind::*;
        match &idref.kind {
            Yield | Identifier(_) => {
                assert!(false, "Wrong IdentifierReference Kind (expected Await)")
            }
            Await => (),
        }

        assert_eq!(idref.string_value(), "await");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }

    #[test]
    fn idref_eval_strict() {
        let idref = idref_create("eval", true);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn idref_eval_loose() {
        let idref = idref_create("eval", false);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn idref_arguments_strict() {
        let idref = idref_create("arguments", true);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn idref_arguments_loose() {
        let idref = idref_create("arguments", false);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }

    fn bindingid_create(text: &str, y: bool, a: bool) -> Box<BindingIdentifier> {
        let yield_syntax = y;
        let await_syntax = a;
        let strict = false;
        let result = binding_identifier(
            &mut Parser::new(text, strict, ParseGoal::Script),
            yield_syntax,
            await_syntax,
        );
        assert!(result.is_ok());
        let optional_bid = result.unwrap();
        assert!(optional_bid.is_some());
        let (bid, scanner) = optional_bid.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len(),
            }
        );
        bid
    }

    fn bid_allflags(text: &str) {
        for yflag in [false, true].iter() {
            for aflag in [false, true].iter() {
                let bid = bindingid_create(text, *yflag, *aflag);
                assert_eq!(bid.string_value(), text);
                assert_eq!(bid.bound_names(), [text]);
                assert_eq!(bid.yield_flag, *yflag);
                assert_eq!(bid.await_flag, *aflag);
            }
        }
    }

    #[test]
    fn binding_identifier_normal() {
        bid_allflags("green");
    }
    #[test]
    fn binding_identifier_yield() {
        bid_allflags("yield");
    }
    #[test]
    fn bindind_identifier_await() {
        bid_allflags("await");
    }
}

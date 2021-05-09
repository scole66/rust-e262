use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::Block;
use super::declarations_and_variables::BindingPattern;
use super::identifiers::BindingIdentifier;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// TryStatement[Yield, Await, Return] :
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum TryStatement {
    Catch(Rc<Block>, Rc<Catch>),
    Finally(Rc<Block>, Rc<Finally>),
    Full(Rc<Block>, Rc<Catch>, Rc<Finally>),
}

impl fmt::Display for TryStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TryStatement::Catch(block, catch) => write!(f, "try {} {}", block, catch),
            TryStatement::Finally(block, finally) => write!(f, "try {} {}", block, finally),
            TryStatement::Full(block, catch, finally) => write!(f, "try {} {} {}", block, catch, finally),
        }
    }
}

impl PrettyPrint for TryStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TryStatement: {}", first, self)?;
        match self {
            TryStatement::Catch(block, catch) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally(block, finally) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full(block, catch, finally) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TryStatement: {}", first, self)?;
        pprint_token(writer, "try", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            TryStatement::Catch(block, catch) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally(block, finally) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full(block, catch, finally) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TryStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_try = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Try)?;
        let (block, after_block) = Block::parse(parser, after_try, yield_flag, await_flag, return_flag)?;
        enum CaseKind {
            Catch(Rc<Catch>),
            Finally(Rc<Finally>),
            Full(Rc<Catch>, Rc<Finally>),
        }
        Err(ParseError::new("Catch or Finally block expected", after_block.line, after_block.column))
            .otherwise(|| {
                let (fin, after_fin) = Finally::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                Ok((CaseKind::Finally(fin), after_fin))
            })
            .otherwise(|| {
                let (catch, after_catch) = Catch::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                match Finally::parse(parser, after_catch, yield_flag, await_flag, return_flag) {
                    Err(_) => Ok((CaseKind::Catch(catch), after_catch)),
                    Ok((fin, after_fin)) => Ok((CaseKind::Full(catch, fin), after_fin)),
                }
            })
            .map(|(kind, scan)| {
                (
                    Rc::new(match kind {
                        CaseKind::Catch(c) => TryStatement::Catch(block, c),
                        CaseKind::Finally(f) => TryStatement::Finally(block, f),
                        CaseKind::Full(c, f) => TryStatement::Full(block, c, f),
                    }),
                    scan,
                )
            })
    }
}

// Catch[Yield, Await, Return] :
//      catch ( CatchParameter[?Yield, ?Await] ) Block[?Yield, ?Await, ?Return]
//      catch Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Catch {
    parameter: Option<Rc<CatchParameter>>,
    block: Rc<Block>,
}

impl fmt::Display for Catch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.parameter {
            None => write!(f, "catch {}", self.block),
            Some(cp) => write!(f, "catch ( {} ) {}", cp, self.block),
        }
    }
}

impl PrettyPrint for Catch {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Catch: {}", first, self)?;
        if let Some(cp) = &self.parameter {
            cp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Catch: {}", first, self)?;
        pprint_token(writer, "catch", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(cp) = &self.parameter {
            pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            cp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Catch {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_catch = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Catch)?;
        Err(ParseError::new("( or { expected", after_catch.line, after_catch.column))
            .otherwise(|| {
                let (block, after_block) = Block::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Catch { parameter: None, block }), after_block))
            })
            .otherwise(|| {
                let after_open = scan_for_punct(after_catch, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (cp, after_cp) = CatchParameter::parse(parser, after_open, yield_flag, await_flag)?;
                let after_close = scan_for_punct(after_cp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (block, after_block) = Block::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Catch { parameter: Some(cp), block }), after_block))
            })
    }
}

// Finally[Yield, Await, Return] :
//      finally Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Finally {
    block: Rc<Block>,
}

impl fmt::Display for Finally {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "finally {}", self.block)
    }
}

impl PrettyPrint for Finally {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Finally: {}", first, self)?;
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Finally: {}", first, self)?;
        pprint_token(writer, "finally", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Finally {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_fin = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Finally)?;
        let (block, after_block) = Block::parse(parser, after_fin, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(Finally { block }), after_block))
    }
}

// CatchParameter[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum CatchParameter {
    Ident(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
}

impl fmt::Display for CatchParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CatchParameter::Ident(node) => node.fmt(f),
            CatchParameter::Pattern(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for CatchParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CatchParameter: {}", first, self)?;
        match self {
            CatchParameter::Ident(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CatchParameter::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            CatchParameter::Ident(node) => node.concise_with_leftpad(writer, pad, state),
            CatchParameter::Pattern(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl CatchParameter {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("CatchParameter expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Ident(bi)), after_bi))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Pattern(bp)), after_bp))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.catch_parameter_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.catch_parameter_cache.insert(key, result.clone());
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

    // TRY STATEMENT
    #[test]
    fn try_statement_test_01() {
        let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } catch {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 19);
        pretty_check(&*node, "TryStatement: try { a ; } catch { }", vec!["Block: { a ; }", "Catch: catch { }"]);
        concise_check(&*node, "TryStatement: try { a ; } catch { }", vec!["Keyword: try", "Block: { a ; }", "Catch: catch { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn try_statement_test_02() {
        let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } finally {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 21);
        pretty_check(&*node, "TryStatement: try { a ; } finally { }", vec!["Block: { a ; }", "Finally: finally { }"]);
        concise_check(&*node, "TryStatement: try { a ; } finally { }", vec!["Keyword: try", "Block: { a ; }", "Finally: finally { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn try_statement_test_03() {
        let (node, scanner) = check(TryStatement::parse(&mut newparser("try { a; } catch { b; } finally { c; }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 38);
        pretty_check(&*node, "TryStatement: try { a ; } catch { b ; } finally { c ; }", vec!["Block: { a ; }", "Catch: catch { b ; }", "Finally: finally { c ; }"]);
        concise_check(&*node, "TryStatement: try { a ; } catch { b ; } finally { c ; }", vec!["Keyword: try", "Block: { a ; }", "Catch: catch { b ; }", "Finally: finally { c ; }"]);
        format!("{:?}", node);
    }
    #[test]
    fn try_statement_test_err_01() {
        check_err(TryStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘try’ expected", 1, 1);
    }
    #[test]
    fn try_statement_test_err_02() {
        check_err(TryStatement::parse(&mut newparser("try"), Scanner::new(), false, false, true), "‘{’ expected", 1, 4);
    }
    #[test]
    fn try_statement_test_err_03() {
        check_err(TryStatement::parse(&mut newparser("try {}"), Scanner::new(), false, false, true), "Catch or Finally block expected", 1, 7);
    }
    #[test]
    fn try_statement_test_prettyerrors_1() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch (e) { console.log(`Got the error ${e}.`); }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn try_statement_test_prettyerrors_2() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } finally { a; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn try_statement_test_prettyerrors_3() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch(foo) { print(foo); } finally { a; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn try_statement_test_conciseerrors_1() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch (e) { console.log(`Got the error ${e}.`); }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn try_statement_test_conciseerrors_2() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } finally { a; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn try_statement_test_conciseerrors_3() {
        let (item, _) = TryStatement::parse(&mut newparser("try { return 3; } catch(foo) { print(foo); } finally { a; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // CATCH
    #[test]
    fn catch_test_01() {
        let (node, scanner) = check(Catch::parse(&mut newparser("catch {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "Catch: catch { }", vec!["Block: { }"]);
        concise_check(&*node, "Catch: catch { }", vec!["Keyword: catch", "Block: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn catch_test_02() {
        let (node, scanner) = check(Catch::parse(&mut newparser("catch (e) {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 12);
        pretty_check(&*node, "Catch: catch ( e ) { }", vec!["CatchParameter: e", "Block: { }"]);
        concise_check(&*node, "Catch: catch ( e ) { }", vec!["Keyword: catch", "Punctuator: (", "IdentifierName: e", "Punctuator: )", "Block: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn catch_test_err_01() {
        check_err(Catch::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘catch’ expected", 1, 1);
    }
    #[test]
    fn catch_test_err_02() {
        check_err(Catch::parse(&mut newparser("catch"), Scanner::new(), false, false, true), "( or { expected", 1, 6);
    }
    #[test]
    fn catch_test_err_03() {
        check_err(Catch::parse(&mut newparser("catch("), Scanner::new(), false, false, true), "CatchParameter expected", 1, 7);
    }
    #[test]
    fn catch_test_err_04() {
        check_err(Catch::parse(&mut newparser("catch(e"), Scanner::new(), false, false, true), "‘)’ expected", 1, 8);
    }
    #[test]
    fn catch_test_err_05() {
        check_err(Catch::parse(&mut newparser("catch(e)"), Scanner::new(), false, false, true), "‘{’ expected", 1, 9);
    }
    #[test]
    fn catch_test_prettyerrors_1() {
        let (item, _) = Catch::parse(&mut newparser("catch { a; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn catch_test_prettyerrors_2() {
        let (item, _) = Catch::parse(&mut newparser("catch (abcd) { a; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn catch_test_conciseerrors_1() {
        let (item, _) = Catch::parse(&mut newparser("catch { a; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn catch_test_conciseerrors_2() {
        let (item, _) = Catch::parse(&mut newparser("catch (abcd) { a; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // FINALLY
    #[test]
    fn finally_test_01() {
        let (node, scanner) = check(Finally::parse(&mut newparser("finally {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 10);
        pretty_check(&*node, "Finally: finally { }", vec!["Block: { }"]);
        concise_check(&*node, "Finally: finally { }", vec!["Keyword: finally", "Block: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn finally_test_err_01() {
        check_err(Finally::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘finally’ expected", 1, 1);
    }
    #[test]
    fn finally_test_err_02() {
        check_err(Finally::parse(&mut newparser("finally"), Scanner::new(), false, false, true), "‘{’ expected", 1, 8);
    }
    #[test]
    fn finally_test_prettyerrors_1() {
        let (item, _) = Finally::parse(&mut newparser("finally { a; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn finally_test_conciseerrors_1() {
        let (item, _) = Finally::parse(&mut newparser("finally { a; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // CATCH PARAMETER
    #[test]
    fn catch_parameter_test_01() {
        let (node, scanner) = check(CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        pretty_check(&*node, "CatchParameter: a", vec!["BindingIdentifier: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn catch_parameter_test_02() {
        let (node, scanner) = check(CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        pretty_check(&*node, "CatchParameter: [ a ]", vec!["BindingPattern: [ a ]"]);
        concise_check(&*node, "ArrayBindingPattern: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
        format!("{:?}", node);
    }
    #[test]
    fn catch_parameter_test_err_01() {
        check_err(CatchParameter::parse(&mut newparser(""), Scanner::new(), false, false), "CatchParameter expected", 1, 1);
    }
    #[test]
    fn catch_parameter_test_prettyerrors_1() {
        let (item, _) = CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn catch_parameter_test_prettyerrors_2() {
        let (item, _) = CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn catch_parameter_test_conciseerrors_1() {
        let (item, _) = CatchParameter::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn catch_parameter_test_conciseerrors_2() {
        let (item, _) = CatchParameter::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn catch_parameter_test_cache_01() {
        let mut parser = newparser("{a,b}");
        let (node, scanner) = check(CatchParameter::parse(&mut parser, Scanner::new(), false, false));
        let (node2, scanner2) = check(CatchParameter::parse(&mut parser, Scanner::new(), false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
}

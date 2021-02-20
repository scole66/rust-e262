use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::comma_operator::Expression;
use super::declarations_and_variables::{BindingPattern, LetOrConst, LexicalDeclaration, VariableDeclarationList};
use super::identifiers::BindingIdentifier;
use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// IterationStatement[Yield, Await, Return] :
//      DoWhileStatement[?Yield, ?Await, ?Return]
//      WhileStatement[?Yield, ?Await, ?Return]
//      ForStatement[?Yield, ?Await, ?Return]
//      ForInOfStatement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum IterationStatement {
    DoWhile(Box<DoWhileStatement>),
    While(Box<WhileStatement>),
    For(Box<ForStatement>),
    ForInOf(Box<ForInOfStatement>),
}

impl fmt::Display for IterationStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IterationStatement::DoWhile(node) => node.fmt(f),
            IterationStatement::While(node) => node.fmt(f),
            IterationStatement::For(node) => node.fmt(f),
            IterationStatement::ForInOf(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for IterationStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IterationStatement: {}", first, self)?;
        match self {
            IterationStatement::DoWhile(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            IterationStatement::While(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            IterationStatement::For(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            IterationStatement::ForInOf(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            IterationStatement::DoWhile(node) => node.concise_with_leftpad(writer, pad, state),
            IterationStatement::While(node) => node.concise_with_leftpad(writer, pad, state),
            IterationStatement::For(node) => node.concise_with_leftpad(writer, pad, state),
            IterationStatement::ForInOf(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl IterationStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("IterationStatement expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (dowhile, after_dowhile) = DoWhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(IterationStatement::DoWhile(dowhile)), after_dowhile))
            })
            .otherwise(|| {
                let (whl, after_whl) = WhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(IterationStatement::While(whl)), after_whl))
            })
            .otherwise(|| {
                let (fr, after_fr) = ForStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(IterationStatement::For(fr)), after_fr))
            })
            .otherwise(|| {
                let (forin, after_forin) = ForInOfStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(IterationStatement::ForInOf(forin)), after_forin))
            })
    }
}

// DoWhileStatement[Yield, Await, Return] :
//      do Statement[?Yield, ?Await, ?Return] while ( Expression[+In, ?Yield, ?Await] ) ;
#[derive(Debug)]
pub enum DoWhileStatement {
    Do(Box<Statement>, Box<Expression>),
}

impl fmt::Display for DoWhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let DoWhileStatement::Do(s, e) = self;
        write!(f, "do {} while ( {} ) ;", s, e)
    }
}

impl PrettyPrint for DoWhileStatement {
    fn pprint_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}DoWhileStatement: {}", first, self)?;
        let DoWhileStatement::Do(s, e) = self;
        pp_two(w, &suc, s, e)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(writer, "{}DoWhileStatement: {}", first, self)?;
        pprint_token(writer, "do", &suc, Spot::NotFinal)?;
        let DoWhileStatement::Do(s, e) = self;
        s.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, "while", &suc, Spot::NotFinal)?;
        pprint_token(writer, "(", &suc, Spot::NotFinal)?;
        e.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, ")", &suc, Spot::NotFinal)?;
        pprint_token(writer, ";", &suc, Spot::Final)
    }
}

impl DoWhileStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_do = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Do)?;
        let (stmt, after_stmt) = Statement::parse(parser, after_do, yield_flag, await_flag, return_flag)?;
        let after_while = scan_for_keyword(after_stmt, parser.source, ScanGoal::InputElementRegExp, Keyword::While)?;
        let after_open = scan_for_punct(after_while, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_semi = scan_for_punct(after_close, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Box::new(DoWhileStatement::Do(stmt, exp)), after_semi))
    }
}

// WhileStatement[Yield, Await, Return] :
//      while ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum WhileStatement {
    While(Box<Expression>, Box<Statement>),
}

impl fmt::Display for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let WhileStatement::While(e, s) = self;
        write!(f, "while ( {} ) {}", e, s)
    }
}

impl PrettyPrint for WhileStatement {
    fn pprint_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}WhileStatement: {}", first, self)?;
        let WhileStatement::While(e, s) = self;
        pp_two(w, &suc, e, s)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(writer, "{}WhileStatement: {}", first, self)?;
        let WhileStatement::While(e, s) = self;
        pprint_token(writer, "while", &suc, Spot::NotFinal)?;
        pprint_token(writer, "(", &suc, Spot::NotFinal)?;
        e.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, ")", &suc, Spot::NotFinal)?;
        s.concise_with_leftpad(writer, &suc, Spot::Final)
    }
}

impl WhileStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_while = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::While)?;
        let after_open = scan_for_punct(after_while, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        Ok((Box::new(WhileStatement::While(exp, stmt)), after_stmt))
    }
}

// ForStatement[Yield, Await, Return] :
//      for ( [lookahead ≠ let [] Expression[~In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum ForStatement {
    For(Option<Box<Expression>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>),
    ForVar(Box<VariableDeclarationList>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>),
    ForLex(Box<LexicalDeclaration>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>),
}

impl fmt::Display for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForStatement::For(Some(e1), Some(e2), Some(e3), s) => {
                write!(f, "for ( {} ; {} ; {} ) {}", e1, e2, e3, s)
            }
            ForStatement::For(Some(e1), Some(e2), None, s) => write!(f, "for ( {} ; {} ; ) {}", e1, e2, s),
            ForStatement::For(Some(e1), None, Some(e3), s) => write!(f, "for ( {} ; ; {} ) {}", e1, e3, s),
            ForStatement::For(Some(e1), None, None, s) => write!(f, "for ( {} ; ; ) {}", e1, s),
            ForStatement::For(None, Some(e2), Some(e3), s) => write!(f, "for ( ; {} ; {} ) {}", e2, e3, s),
            ForStatement::For(None, Some(e2), None, s) => write!(f, "for ( ; {} ; {} )", e2, s),
            ForStatement::For(None, None, Some(e3), s) => write!(f, "for ( ; ; {} ) {}", e3, s),
            ForStatement::For(None, None, None, s) => write!(f, "for ( ; ; ) {}", s),
            ForStatement::ForVar(v, Some(e1), Some(e2), s) => {
                write!(f, "for ( var {} ; {} ; {} ) {}", v, e1, e2, s)
            }
            ForStatement::ForVar(v, Some(e1), None, s) => write!(f, "for ( var {} ; {} ; ) {}", v, e1, s),
            ForStatement::ForVar(v, None, Some(e2), s) => write!(f, "for ( var {} ; ; {} ) {}", v, e2, s),
            ForStatement::ForVar(v, None, None, s) => write!(f, "for ( var {} ; ; ) {}", v, s),
            ForStatement::ForLex(lex, Some(e1), Some(e2), s) => {
                write!(f, "for ( {} {} ; {} ) {}", lex, e1, e2, s)
            }
            ForStatement::ForLex(lex, Some(e1), None, s) => write!(f, "for ( {} {} ; ) {}", lex, e1, s),
            ForStatement::ForLex(lex, None, Some(e2), s) => write!(f, "for ( {} ; {} ) {}", lex, e2, s),
            ForStatement::ForLex(lex, None, None, s) => write!(f, "for ( {} ; ) {}", lex, s),
        }
    }
}

impl PrettyPrint for ForStatement {
    fn pprint_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}ForStatement: {}", first, self)?;
        match self {
            ForStatement::For(Some(e1), Some(e2), Some(e3), s) => pp_four(w, &suc, e1, e2, e3, s),
            ForStatement::For(Some(e1), Some(e2), None, s) => pp_three(w, &suc, e1, e2, s),
            ForStatement::For(Some(e1), None, Some(e3), s) => pp_three(w, &suc, e1, e3, s),
            ForStatement::For(Some(e1), None, None, s) => pp_two(w, &suc, e1, s),
            ForStatement::For(None, Some(e2), Some(e3), s) => pp_three(w, &suc, e2, e3, s),
            ForStatement::For(None, Some(e2), None, s) => pp_two(w, &suc, e2, s),
            ForStatement::For(None, None, Some(e3), s) => pp_two(w, &suc, e3, s),
            ForStatement::For(None, None, None, s) => s.pprint_with_leftpad(w, &suc, Spot::Final),
            ForStatement::ForVar(v, Some(e1), Some(e2), s) => pp_four(w, &suc, v, e1, e2, s),
            ForStatement::ForVar(v, Some(e1), None, s) => pp_three(w, &suc, v, e1, s),
            ForStatement::ForVar(v, None, Some(e2), s) => pp_three(w, &suc, v, e2, s),
            ForStatement::ForVar(v, None, None, s) => pp_two(w, &suc, v, s),
            ForStatement::ForLex(lex, Some(e1), Some(e2), s) => pp_four(w, &suc, lex, e1, e2, s),
            ForStatement::ForLex(lex, Some(e1), None, s) => pp_three(w, &suc, lex, e1, s),
            ForStatement::ForLex(lex, None, Some(e2), s) => pp_three(w, &suc, lex, e2, s),
            ForStatement::ForLex(lex, None, None, s) => pp_two(w, &suc, lex, s),
        }
    }

    fn concise_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}ForStatement: {}", first, self)?;
        pprint_token(w, "for", &suc, Spot::NotFinal)?;
        pprint_token(w, "(", &suc, Spot::NotFinal)?;
        let maybeprint = |w: &mut T, e: &Option<Box<Expression>>| {
            if let Some(exp) = e {
                exp.concise_with_leftpad(w, &suc, Spot::NotFinal)
            } else {
                Ok(())
            }
        };
        match self {
            ForStatement::For(e1, e2, e3, s) => {
                maybeprint(w, e1)?;
                pprint_token(w, ";", &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ";", &suc, Spot::NotFinal)?;
                maybeprint(w, e3)?;
                pprint_token(w, ")", &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
            ForStatement::ForVar(v, e1, e2, s) => {
                pprint_token(w, "var", &suc, Spot::NotFinal)?;
                v.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
                pprint_token(w, ";", &suc, Spot::NotFinal)?;
                maybeprint(w, e1)?;
                pprint_token(w, ";", &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ")", &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
            ForStatement::ForLex(lex, e1, e2, s) => {
                lex.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
                maybeprint(w, e1)?;
                pprint_token(w, ";", &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ")", &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
        }
    }
}

impl ForStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_for = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::For)?;
        let after_open = scan_for_punct(after_for, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        Err(ParseError::new("Badly formed for-statement initializer", after_open.line, after_open.column))
            .otherwise(|| {
                // for ( var VariableDeclarationList ; Expression ; Expression ) Statement
                let after_var = scan_for_keyword(after_open, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
                let (vdl, after_vdl) = VariableDeclarationList::parse(parser, after_var, false, yield_flag, await_flag)?;
                let after_init = scan_for_punct(after_vdl, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                let (exp1, after_exp1) = match Expression::parse(parser, after_init, true, yield_flag, await_flag) {
                    Err(_) => (None, after_init),
                    Ok((node, scan)) => (Some(node), scan),
                };
                let after_test = scan_for_punct(after_exp1, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                let (exp2, after_exp2) = match Expression::parse(parser, after_test, true, yield_flag, await_flag) {
                    Err(_) => (None, after_test),
                    Ok((node, scan)) => (Some(node), scan),
                };
                let after_close = scan_for_punct(after_exp2, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(ForStatement::ForVar(vdl, exp1, exp2, stmt)), after_stmt))
            })
            .otherwise(|| {
                // for ( LexicalDeclaration Expression ; Expression ) Statement
                let (lex, after_lex) = LexicalDeclaration::parse(parser, after_open, false, yield_flag, await_flag)?;
                let (exp1, after_exp1) = match Expression::parse(parser, after_lex, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_lex),
                };
                let after_test = scan_for_punct(after_exp1, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                let (exp2, after_exp2) = match Expression::parse(parser, after_test, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_test),
                };
                let after_close = scan_for_punct(after_exp2, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(ForStatement::ForLex(lex, exp1, exp2, stmt)), after_stmt))
            })
            .otherwise(|| {
                // for ( Expression ; Expression ; Expression ) Statement
                let (lookahead1, after_1) = scan_token(&after_open, parser.source, ScanGoal::InputElementRegExp);
                if lookahead1.matches_keyword(Keyword::Let) {
                    let (lookahead2, _) = scan_token(&after_1, parser.source, ScanGoal::InputElementRegExp);
                    if lookahead2.matches_punct(Punctuator::LeftBracket) {
                        return Err(ParseError::new("Invalid Expression", after_open.line, after_open.column));
                    }
                }
                let (init, after_init) = match Expression::parse(parser, after_open, false, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_open),
                };
                let after_semi1 = scan_for_punct(after_init, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                let (test, after_test) = match Expression::parse(parser, after_semi1, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_semi1),
                };
                let after_semi2 = scan_for_punct(after_test, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                let (inc, after_inc) = match Expression::parse(parser, after_semi2, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_semi2),
                };
                let after_close = scan_for_punct(after_inc, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(ForStatement::For(init, test, inc, stmt)), after_stmt))
            })
    }
}

// ForInOfStatement[Yield, Await, Return] :
//      for ( [lookahead ≠ let [] LeftHandSideExpression[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      for ( var ForBinding[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      for ( ForDeclaration[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      for ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      for ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      for ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      [+Await] for await ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      [+Await] for await ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
//      [+Await] for await ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum ForInOfStatement {
    ForIn(Box<LeftHandSideExpression>, Box<Expression>, Box<Statement>),
    ForVarIn(Box<ForBinding>, Box<Expression>, Box<Statement>),
    ForLexIn(Box<ForDeclaration>, Box<Expression>, Box<Statement>),
    ForOf(Box<LeftHandSideExpression>, Box<AssignmentExpression>, Box<Statement>),
    ForVarOf(Box<ForBinding>, Box<AssignmentExpression>, Box<Statement>),
    ForLexOf(Box<ForDeclaration>, Box<AssignmentExpression>, Box<Statement>),
    ForAwaitOf(Box<LeftHandSideExpression>, Box<AssignmentExpression>, Box<Statement>),
    ForAwaitVarOf(Box<ForBinding>, Box<AssignmentExpression>, Box<Statement>),
    ForAwaitLexOf(Box<ForDeclaration>, Box<AssignmentExpression>, Box<Statement>),
}

impl fmt::Display for ForInOfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForInOfStatement::ForIn(lhs, e, s) => write!(f, "for ( {} in {} ) {}", lhs, e, s),
            ForInOfStatement::ForVarIn(v, e, s) => write!(f, "for ( var {} in {} ) {}", v, e, s),
            ForInOfStatement::ForLexIn(lex, e, s) => write!(f, "for ( {} in {} ) {}", lex, e, s),
            ForInOfStatement::ForOf(lhs, e, s) => write!(f, "for ( {} of {} ) {}", lhs, e, s),
            ForInOfStatement::ForVarOf(v, e, s) => write!(f, "for ( var {} of {} ) {}", v, e, s),
            ForInOfStatement::ForLexOf(lex, e, s) => write!(f, "for ( {} of {} ) {}", lex, e, s),
            ForInOfStatement::ForAwaitOf(lhs, e, s) => write!(f, "for await ( {} of {} ) {}", lhs, e, s),
            ForInOfStatement::ForAwaitVarOf(v, e, s) => write!(f, "for await ( var {} of {} ) {}", v, e, s),
            ForInOfStatement::ForAwaitLexOf(lex, e, s) => write!(f, "for await ( {} of {} ) {}", lex, e, s),
        }
    }
}

impl PrettyPrint for ForInOfStatement {
    fn pprint_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}ForInOfStatement: {}", first, self)?;
        match self {
            ForInOfStatement::ForIn(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::ForVarIn(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::ForLexIn(lex, e, s) => pp_three(w, &suc, lex, e, s),
            ForInOfStatement::ForOf(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::ForVarOf(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::ForLexOf(lex, e, s) => pp_three(w, &suc, lex, e, s),
            ForInOfStatement::ForAwaitOf(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::ForAwaitVarOf(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::ForAwaitLexOf(lex, e, s) => pp_three(w, &suc, lex, e, s),
        }
    }

    fn concise_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let await_present =
            matches!(self, ForInOfStatement::ForAwaitOf(_, _, _)) || matches!(self, ForInOfStatement::ForAwaitVarOf(_, _, _)) || matches!(self, ForInOfStatement::ForAwaitLexOf(_, _, _));
        let var_present =
            matches!(self, ForInOfStatement::ForVarIn(_, _, _)) || matches!(self, ForInOfStatement::ForVarOf(_, _, _)) || matches!(self, ForInOfStatement::ForAwaitVarOf(_, _, _));

        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}ForInOfStatement: {}", first, self)?;
        pprint_token(w, "for", &suc, Spot::NotFinal)?;

        if await_present {
            pprint_token(w, "await", &suc, Spot::NotFinal)?;
        }

        pprint_token(w, "(", &suc, Spot::NotFinal)?;

        if var_present {
            pprint_token(w, "var", &suc, Spot::NotFinal)?;
        }

        match self {
            ForInOfStatement::ForIn(lhs, _, _) | ForInOfStatement::ForOf(lhs, _, _) | ForInOfStatement::ForAwaitOf(lhs, _, _) => lhs.concise_with_leftpad(w, &suc, Spot::NotFinal),
            ForInOfStatement::ForVarIn(v, _, _) | ForInOfStatement::ForVarOf(v, _, _) | ForInOfStatement::ForAwaitVarOf(v, _, _) => v.concise_with_leftpad(w, &suc, Spot::NotFinal),
            ForInOfStatement::ForLexIn(lex, _, _) | ForInOfStatement::ForLexOf(lex, _, _) | ForInOfStatement::ForAwaitLexOf(lex, _, _) => lex.concise_with_leftpad(w, &suc, Spot::NotFinal),
        }?;

        match self {
            ForInOfStatement::ForIn(_, e, _) | ForInOfStatement::ForLexIn(_, e, _) | ForInOfStatement::ForVarIn(_, e, _) => {
                pprint_token(w, "in", &suc, Spot::NotFinal)?;
                e.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
            }
            ForInOfStatement::ForVarOf(_, ae, _)
            | ForInOfStatement::ForOf(_, ae, _)
            | ForInOfStatement::ForAwaitVarOf(_, ae, _)
            | ForInOfStatement::ForLexOf(_, ae, _)
            | ForInOfStatement::ForAwaitOf(_, ae, _)
            | ForInOfStatement::ForAwaitLexOf(_, ae, _) => {
                pprint_token(w, "of", &suc, Spot::NotFinal)?;
                ae.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
            }
        }

        pprint_token(w, ")", &suc, Spot::NotFinal)?;

        match self {
            ForInOfStatement::ForIn(_, _, s)
            | ForInOfStatement::ForOf(_, _, s)
            | ForInOfStatement::ForAwaitOf(_, _, s)
            | ForInOfStatement::ForVarIn(_, _, s)
            | ForInOfStatement::ForVarOf(_, _, s)
            | ForInOfStatement::ForAwaitVarOf(_, _, s)
            | ForInOfStatement::ForLexIn(_, _, s)
            | ForInOfStatement::ForLexOf(_, _, s)
            | ForInOfStatement::ForAwaitLexOf(_, _, s) => s.concise_with_leftpad(w, &suc, Spot::Final),
        }
    }
}

impl ForInOfStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_for = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::For)?;
        let (await_seen, after_await) = match await_flag {
            true => match scan_for_keyword(after_for, parser.source, ScanGoal::InputElementDiv, Keyword::Await) {
                Ok(scan) => (true, scan),
                Err(_) => (false, after_for),
            },
            false => (false, after_for),
        };
        let after_open = scan_for_punct(after_await, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        Err(ParseError::new("Badly constructed InOf for-expression", after_open.line, after_open.column))
            .otherwise(|| {
                // for var
                let after_var = scan_for_keyword(after_open, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
                let (for_binding, after_fb) = ForBinding::parse(parser, after_var, yield_flag, await_flag)?;
                let (kwd, after_kwd) = scan_for_keywords(after_fb, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?;
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Box::new(ForInOfStatement::ForAwaitVarOf(for_binding, ae, stmt)), after_stmt))
                        } else {
                            Ok((Box::new(ForInOfStatement::ForVarOf(for_binding, ae, stmt)), after_stmt))
                        }
                    }
                    Keyword::In | _ => {
                        if await_seen {
                            Err(ParseError::new("await not allowed in \"for-in\" expressions", after_for.line, after_for.column))
                        } else {
                            let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                            let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                            let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                            Ok((Box::new(ForInOfStatement::ForVarIn(for_binding, exp, stmt)), after_stmt))
                        }
                    }
                }
            })
            .otherwise(|| {
                // for lex
                let (decl, after_decl) = ForDeclaration::parse(parser, after_open, yield_flag, await_flag)?;
                let (kwd, after_kwd) = scan_for_keywords(after_decl, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?;
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Box::new(ForInOfStatement::ForAwaitLexOf(decl, ae, stmt)), after_stmt))
                        } else {
                            Ok((Box::new(ForInOfStatement::ForLexOf(decl, ae, stmt)), after_stmt))
                        }
                    }
                    Keyword::In | _ => {
                        if await_seen {
                            Err(ParseError::new("await not allowed in \"for-in\" expressions", after_for.line, after_for.column))
                        } else {
                            let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                            let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                            let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                            Ok((Box::new(ForInOfStatement::ForLexIn(decl, exp, stmt)), after_stmt))
                        }
                    }
                }
            })
            .otherwise(|| {
                // for ( LHS in/of ... )
                let (lookahead1, after_lh1) = scan_token(&after_open, parser.source, ScanGoal::InputElementRegExp);
                if lookahead1.matches_keyword(Keyword::Let)
                    && (await_seen || {
                        let (lookahead2, _) = scan_token(&after_lh1, parser.source, ScanGoal::InputElementDiv);
                        lookahead2.matches_punct(Punctuator::LeftBracket)
                    })
                {
                    return Err(ParseError::new("Badly formed LHS", after_open.line, after_open.column));
                }
                let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, after_open, yield_flag, await_flag)?;
                let (kwd, after_kwd) = scan_for_keywords(after_lhs, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?;
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Box::new(ForInOfStatement::ForAwaitOf(lhs, ae, stmt)), after_stmt))
                        } else {
                            Ok((Box::new(ForInOfStatement::ForOf(lhs, ae, stmt)), after_stmt))
                        }
                    }
                    Keyword::In | _ => {
                        if await_seen {
                            Err(ParseError::new("await not allowed in \"for-in\" expressions", after_for.line, after_for.column))
                        } else {
                            let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                            let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                            let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                            Ok((Box::new(ForInOfStatement::ForIn(lhs, exp, stmt)), after_stmt))
                        }
                    }
                }
            })
    }
}

fn pp_two<T, U, V>(writer: &mut T, pad: &str, n1: &Box<U>, n2: &Box<V>) -> IoResult<()>
where
    T: Write,
    U: PrettyPrint,
    V: PrettyPrint,
{
    n1.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.pprint_with_leftpad(writer, pad, Spot::Final)
}

fn pp_three<T, U, V, W>(writer: &mut T, pad: &str, n1: &Box<U>, n2: &Box<V>, n3: &Box<W>) -> IoResult<()>
where
    T: Write,
    U: PrettyPrint,
    V: PrettyPrint,
    W: PrettyPrint,
{
    n1.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n3.pprint_with_leftpad(writer, pad, Spot::Final)
}

fn pp_four<T, U, V, W, X>(writer: &mut T, pad: &str, n1: &Box<U>, n2: &Box<V>, n3: &Box<W>, n4: &Box<X>) -> IoResult<()>
where
    T: Write,
    U: PrettyPrint,
    V: PrettyPrint,
    W: PrettyPrint,
    X: PrettyPrint,
{
    n1.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n3.pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n4.pprint_with_leftpad(writer, pad, Spot::Final)
}

// ForDeclaration[Yield, Await] :
//      LetOrConst ForBinding[?Yield, ?Await]
#[derive(Debug)]
pub enum ForDeclaration {
    Binding(LetOrConst, Box<ForBinding>),
}

impl fmt::Display for ForDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ForDeclaration::Binding(loc, node) = self;
        write!(f, "{} {}", loc, node)
    }
}

impl PrettyPrint for ForDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ForDeclaration: {}", first, self)?;
        let ForDeclaration::Binding(loc, node) = self;
        loc.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ForDeclaration: {}", first, self)?;
        let ForDeclaration::Binding(loc, node) = self;
        loc.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        node.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl ForDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tok, after_tok) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Let, Keyword::Const])?;
        let loc = match tok {
            Keyword::Let => LetOrConst::Let,
            Keyword::Const | _ => LetOrConst::Const,
        };
        let (binding, after_binding) = ForBinding::parse(parser, after_tok, yield_flag, await_flag)?;
        Ok((Box::new(ForDeclaration::Binding(loc, binding)), after_binding))
    }
}

// ForBinding[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum ForBinding {
    Identifier(Box<BindingIdentifier>),
    Pattern(Box<BindingPattern>),
}

impl fmt::Display for ForBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForBinding::Identifier(node) => node.fmt(f),
            ForBinding::Pattern(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for ForBinding {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ForBinding: {}", first, self)?;
        match self {
            ForBinding::Identifier(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ForBinding::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ForBinding::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            ForBinding::Pattern(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl ForBinding {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("ForBinding expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (id, after_id) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(ForBinding::Identifier(id)), after_id))
            })
            .otherwise(|| {
                let (pat, after_pat) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(ForBinding::Pattern(pat)), after_pat))
            })
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

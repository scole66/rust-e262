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
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// IterationStatement[Yield, Await, Return] :
//      DoWhileStatement[?Yield, ?Await, ?Return]
//      WhileStatement[?Yield, ?Await, ?Return]
//      ForStatement[?Yield, ?Await, ?Return]
//      ForInOfStatement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum IterationStatement {
    DoWhile(Rc<DoWhileStatement>),
    While(Rc<WhileStatement>),
    For(Rc<ForStatement>),
    ForInOf(Rc<ForInOfStatement>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("IterationStatement expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (dowhile, after_dowhile) = DoWhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IterationStatement::DoWhile(dowhile)), after_dowhile))
            })
            .otherwise(|| {
                let (whl, after_whl) = WhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IterationStatement::While(whl)), after_whl))
            })
            .otherwise(|| {
                let (fr, after_fr) = ForStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IterationStatement::For(fr)), after_fr))
            })
            .otherwise(|| {
                let (forin, after_forin) = ForInOfStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IterationStatement::ForInOf(forin)), after_forin))
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            IterationStatement::DoWhile(node) => node.var_declared_names(),
            IterationStatement::While(node) => node.var_declared_names(),
            IterationStatement::For(node) => node.var_declared_names(),
            IterationStatement::ForInOf(node) => node.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            IterationStatement::DoWhile(node) => node.contains_undefined_break_target(label_set),
            IterationStatement::While(node) => node.contains_undefined_break_target(label_set),
            IterationStatement::For(node) => node.contains_undefined_break_target(label_set),
            IterationStatement::ForInOf(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            IterationStatement::DoWhile(node) => node.contains(kind),
            IterationStatement::While(node) => node.contains(kind),
            IterationStatement::For(node) => node.contains(kind),
            IterationStatement::ForInOf(node) => node.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            IterationStatement::DoWhile(node) => node.contains_duplicate_labels(label_set),
            IterationStatement::While(node) => node.contains_duplicate_labels(label_set),
            IterationStatement::For(node) => node.contains_duplicate_labels(label_set),
            IterationStatement::ForInOf(node) => node.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            IterationStatement::DoWhile(node) => node.contains_undefined_continue_target(iteration_set),
            IterationStatement::While(node) => node.contains_undefined_continue_target(iteration_set),
            IterationStatement::For(node) => node.contains_undefined_continue_target(iteration_set),
            IterationStatement::ForInOf(node) => node.contains_undefined_continue_target(iteration_set),
        }
    }
}

// DoWhileStatement[Yield, Await, Return] :
//      do Statement[?Yield, ?Await, ?Return] while ( Expression[+In, ?Yield, ?Await] ) ;
#[derive(Debug)]
pub enum DoWhileStatement {
    Do(Rc<Statement>, Rc<Expression>),
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
        pprint_token(writer, "do", TokenType::Keyword, &suc, Spot::NotFinal)?;
        let DoWhileStatement::Do(s, e) = self;
        s.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, "while", TokenType::Keyword, &suc, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &suc, Spot::NotFinal)?;
        e.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &suc, Spot::Final)
    }
}

impl DoWhileStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_do = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Do)?;
        let (stmt, after_stmt) = Statement::parse(parser, after_do, yield_flag, await_flag, return_flag)?;
        let after_while = scan_for_keyword(after_stmt, parser.source, ScanGoal::InputElementRegExp, Keyword::While)?;
        let after_open = scan_for_punct(after_while, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_semi = scan_for_punct(after_close, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon).unwrap_or(after_close);
        Ok((Rc::new(DoWhileStatement::Do(stmt, exp)), after_semi))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let DoWhileStatement::Do(s, _) = self;
        s.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let DoWhileStatement::Do(s, _) = self;
        s.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let DoWhileStatement::Do(s, e) = self;
        s.contains(kind) || e.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let DoWhileStatement::Do(s, _) = self;
        s.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        let DoWhileStatement::Do(s, _) = self;
        s.contains_undefined_continue_target(iteration_set, &[])
    }
}

// WhileStatement[Yield, Await, Return] :
//      while ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum WhileStatement {
    While(Rc<Expression>, Rc<Statement>),
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
        pprint_token(writer, "while", TokenType::Keyword, &suc, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &suc, Spot::NotFinal)?;
        e.concise_with_leftpad(writer, &suc, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;
        s.concise_with_leftpad(writer, &suc, Spot::Final)
    }
}

impl WhileStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_while = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::While)?;
        let after_open = scan_for_punct(after_while, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(WhileStatement::While(exp, stmt)), after_stmt))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let WhileStatement::While(_, s) = self;
        s.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let WhileStatement::While(_, s) = self;
        s.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let WhileStatement::While(e, s) = self;
        e.contains(kind) || s.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let WhileStatement::While(_, s) = self;
        s.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        let WhileStatement::While(_, s) = self;
        s.contains_undefined_continue_target(iteration_set, &[])
    }
}

// ForStatement[Yield, Await, Return] :
//      for ( [lookahead ≠ let [] Expression[~In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum ForStatement {
    For(Option<Rc<Expression>>, Option<Rc<Expression>>, Option<Rc<Expression>>, Rc<Statement>),
    ForVar(Rc<VariableDeclarationList>, Option<Rc<Expression>>, Option<Rc<Expression>>, Rc<Statement>),
    ForLex(Rc<LexicalDeclaration>, Option<Rc<Expression>>, Option<Rc<Expression>>, Rc<Statement>),
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
            ForStatement::For(None, Some(e2), None, s) => write!(f, "for ( ; {} ; ) {}", e2, s),
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
        pprint_token(w, "for", TokenType::Keyword, &suc, Spot::NotFinal)?;
        pprint_token(w, "(", TokenType::Punctuator, &suc, Spot::NotFinal)?;
        let maybeprint = |w: &mut T, e: &Option<Rc<Expression>>| {
            if let Some(exp) = e {
                exp.concise_with_leftpad(w, &suc, Spot::NotFinal)
            } else {
                Ok(())
            }
        };
        match self {
            ForStatement::For(e1, e2, e3, s) => {
                maybeprint(w, e1)?;
                pprint_token(w, ";", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ";", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                maybeprint(w, e3)?;
                pprint_token(w, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
            ForStatement::ForVar(v, e1, e2, s) => {
                pprint_token(w, "var", TokenType::Keyword, &suc, Spot::NotFinal)?;
                v.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
                pprint_token(w, ";", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                maybeprint(w, e1)?;
                pprint_token(w, ";", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
            ForStatement::ForLex(lex, e1, e2, s) => {
                lex.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
                maybeprint(w, e1)?;
                pprint_token(w, ";", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                maybeprint(w, e2)?;
                pprint_token(w, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;
                s.concise_with_leftpad(w, &suc, Spot::Final)
            }
        }
    }
}

impl ForStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
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
                Ok((Rc::new(ForStatement::ForVar(vdl, exp1, exp2, stmt)), after_stmt))
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
                Ok((Rc::new(ForStatement::ForLex(lex, exp1, exp2, stmt)), after_stmt))
            })
            .otherwise(|| {
                // for ( Expression ; Expression ; Expression ) Statement
                let (lookahead1, after_1) = scan_token(&after_open, parser.source, ScanGoal::InputElementRegExp);
                if lookahead1.matches_keyword(Keyword::Let) {
                    let (lookahead2, _) = scan_token(&after_1, parser.source, ScanGoal::InputElementRegExp);
                    if lookahead2.matches_punct(Punctuator::LeftBracket) {
                        // We return an error here, and stop processing, but it will never be seen by our callers
                        // because the error from the Lexical Binding parse happens first, and takes precedence.
                        return Err(ParseError::new(String::new(), after_open.line, after_open.column));
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
                Ok((Rc::new(ForStatement::For(init, test, inc, stmt)), after_stmt))
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            ForStatement::For(_, _, _, s) => s.var_declared_names(),
            ForStatement::ForVar(v, _, _, s) => {
                let mut names = v.bound_names();
                names.extend(s.var_declared_names());
                names
            }
            ForStatement::ForLex(_, _, _, s) => s.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            ForStatement::For(_, _, _, s) | ForStatement::ForVar(_, _, _, s) | ForStatement::ForLex(_, _, _, s) => s.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ForStatement::For(opt1, opt2, opt3, s) => {
                opt1.as_ref().map_or(false, |n| n.contains(kind))
                    || opt2.as_ref().map_or(false, |n| n.contains(kind))
                    || opt3.as_ref().map_or(false, |n| n.contains(kind))
                    || s.contains(kind)
            }
            ForStatement::ForVar(v, opt1, opt2, s) => {
                v.contains(kind) || opt1.as_ref().map_or(false, |n| n.contains(kind)) || opt2.as_ref().map_or(false, |n| n.contains(kind)) || s.contains(kind)
            }
            ForStatement::ForLex(lex, opt1, opt2, s) => {
                lex.contains(kind) || opt1.as_ref().map_or(false, |n| n.contains(kind)) || opt2.as_ref().map_or(false, |n| n.contains(kind)) || s.contains(kind)
            }
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            ForStatement::For(_, _, _, s) | ForStatement::ForVar(_, _, _, s) | ForStatement::ForLex(_, _, _, s) => s.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            ForStatement::For(_, _, _, s) | ForStatement::ForVar(_, _, _, s) | ForStatement::ForLex(_, _, _, s) => s.contains_undefined_continue_target(iteration_set, &[]),
        }
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
    In(Rc<LeftHandSideExpression>, Rc<Expression>, Rc<Statement>),
    VarIn(Rc<ForBinding>, Rc<Expression>, Rc<Statement>),
    LexIn(Rc<ForDeclaration>, Rc<Expression>, Rc<Statement>),
    Of(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>, Rc<Statement>),
    VarOf(Rc<ForBinding>, Rc<AssignmentExpression>, Rc<Statement>),
    LexOf(Rc<ForDeclaration>, Rc<AssignmentExpression>, Rc<Statement>),
    AwaitOf(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>, Rc<Statement>),
    AwaitVarOf(Rc<ForBinding>, Rc<AssignmentExpression>, Rc<Statement>),
    AwaitLexOf(Rc<ForDeclaration>, Rc<AssignmentExpression>, Rc<Statement>),
}

impl fmt::Display for ForInOfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForInOfStatement::In(lhs, e, s) => write!(f, "for ( {} in {} ) {}", lhs, e, s),
            ForInOfStatement::VarIn(v, e, s) => write!(f, "for ( var {} in {} ) {}", v, e, s),
            ForInOfStatement::LexIn(lex, e, s) => write!(f, "for ( {} in {} ) {}", lex, e, s),
            ForInOfStatement::Of(lhs, e, s) => write!(f, "for ( {} of {} ) {}", lhs, e, s),
            ForInOfStatement::VarOf(v, e, s) => write!(f, "for ( var {} of {} ) {}", v, e, s),
            ForInOfStatement::LexOf(lex, e, s) => write!(f, "for ( {} of {} ) {}", lex, e, s),
            ForInOfStatement::AwaitOf(lhs, e, s) => write!(f, "for await ( {} of {} ) {}", lhs, e, s),
            ForInOfStatement::AwaitVarOf(v, e, s) => write!(f, "for await ( var {} of {} ) {}", v, e, s),
            ForInOfStatement::AwaitLexOf(lex, e, s) => write!(f, "for await ( {} of {} ) {}", lex, e, s),
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
            ForInOfStatement::In(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::VarIn(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::LexIn(lex, e, s) => pp_three(w, &suc, lex, e, s),
            ForInOfStatement::Of(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::VarOf(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::LexOf(lex, e, s) => pp_three(w, &suc, lex, e, s),
            ForInOfStatement::AwaitOf(lhs, e, s) => pp_three(w, &suc, lhs, e, s),
            ForInOfStatement::AwaitVarOf(v, e, s) => pp_three(w, &suc, v, e, s),
            ForInOfStatement::AwaitLexOf(lex, e, s) => pp_three(w, &suc, lex, e, s),
        }
    }

    fn concise_with_leftpad<T>(&self, w: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let await_present =
            matches!(self, ForInOfStatement::AwaitOf(_, _, _)) || matches!(self, ForInOfStatement::AwaitVarOf(_, _, _)) || matches!(self, ForInOfStatement::AwaitLexOf(_, _, _));
        let var_present = matches!(self, ForInOfStatement::VarIn(_, _, _)) || matches!(self, ForInOfStatement::VarOf(_, _, _)) || matches!(self, ForInOfStatement::AwaitVarOf(_, _, _));

        let (first, suc) = prettypad(pad, state);
        writeln!(w, "{}ForInOfStatement: {}", first, self)?;
        pprint_token(w, "for", TokenType::Keyword, &suc, Spot::NotFinal)?;

        if await_present {
            pprint_token(w, "await", TokenType::Keyword, &suc, Spot::NotFinal)?;
        }

        pprint_token(w, "(", TokenType::Punctuator, &suc, Spot::NotFinal)?;

        if var_present {
            pprint_token(w, "var", TokenType::Keyword, &suc, Spot::NotFinal)?;
        }

        match self {
            ForInOfStatement::In(lhs, _, _) | ForInOfStatement::Of(lhs, _, _) | ForInOfStatement::AwaitOf(lhs, _, _) => lhs.concise_with_leftpad(w, &suc, Spot::NotFinal),
            ForInOfStatement::VarIn(v, _, _) | ForInOfStatement::VarOf(v, _, _) | ForInOfStatement::AwaitVarOf(v, _, _) => v.concise_with_leftpad(w, &suc, Spot::NotFinal),
            ForInOfStatement::LexIn(lex, _, _) | ForInOfStatement::LexOf(lex, _, _) | ForInOfStatement::AwaitLexOf(lex, _, _) => lex.concise_with_leftpad(w, &suc, Spot::NotFinal),
        }?;

        match self {
            ForInOfStatement::In(_, e, _) | ForInOfStatement::LexIn(_, e, _) | ForInOfStatement::VarIn(_, e, _) => {
                pprint_token(w, "in", TokenType::Keyword, &suc, Spot::NotFinal)?;
                e.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
            }
            ForInOfStatement::VarOf(_, ae, _)
            | ForInOfStatement::Of(_, ae, _)
            | ForInOfStatement::AwaitVarOf(_, ae, _)
            | ForInOfStatement::LexOf(_, ae, _)
            | ForInOfStatement::AwaitOf(_, ae, _)
            | ForInOfStatement::AwaitLexOf(_, ae, _) => {
                pprint_token(w, "of", TokenType::Keyword, &suc, Spot::NotFinal)?;
                ae.concise_with_leftpad(w, &suc, Spot::NotFinal)?;
            }
        }

        pprint_token(w, ")", TokenType::Punctuator, &suc, Spot::NotFinal)?;

        match self {
            ForInOfStatement::In(_, _, s)
            | ForInOfStatement::Of(_, _, s)
            | ForInOfStatement::AwaitOf(_, _, s)
            | ForInOfStatement::VarIn(_, _, s)
            | ForInOfStatement::VarOf(_, _, s)
            | ForInOfStatement::AwaitVarOf(_, _, s)
            | ForInOfStatement::LexIn(_, _, s)
            | ForInOfStatement::LexOf(_, _, s)
            | ForInOfStatement::AwaitLexOf(_, _, s) => s.concise_with_leftpad(w, &suc, Spot::Final),
        }
    }
}

impl ForInOfStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_for = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::For)?;
        let (await_seen, after_await) = match await_flag {
            true => match scan_for_keyword(after_for, parser.source, ScanGoal::InputElementDiv, Keyword::Await) {
                Ok(scan) => (true, scan),
                Err(_) => (false, after_for),
            },
            false => (false, after_for),
        };
        let after_open = scan_for_punct(after_await, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        Err(ParseError::new("‘let’, ‘var’, or a LeftHandSideExpression expected", after_open.line, after_open.column))
            .otherwise(|| {
                // for var
                let after_var = scan_for_keyword(after_open, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
                let (for_binding, after_fb) = ForBinding::parse(parser, after_var, yield_flag, await_flag)?;
                let (kwd, after_kwd) = if await_seen {
                    (Keyword::Of, scan_for_keyword(after_fb, parser.source, ScanGoal::InputElementRegExp, Keyword::Of)?)
                } else {
                    scan_for_keywords(after_fb, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?
                };
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Rc::new(ForInOfStatement::AwaitVarOf(for_binding, ae, stmt)), after_stmt))
                        } else {
                            Ok((Rc::new(ForInOfStatement::VarOf(for_binding, ae, stmt)), after_stmt))
                        }
                    }
                    _ => {
                        let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        Ok((Rc::new(ForInOfStatement::VarIn(for_binding, exp, stmt)), after_stmt))
                    }
                }
            })
            .otherwise(|| {
                // for lex
                let (decl, after_decl) = ForDeclaration::parse(parser, after_open, yield_flag, await_flag)?;
                let (kwd, after_kwd) = if await_seen {
                    (Keyword::Of, scan_for_keyword(after_decl, parser.source, ScanGoal::InputElementRegExp, Keyword::Of)?)
                } else {
                    scan_for_keywords(after_decl, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?
                };
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Rc::new(ForInOfStatement::AwaitLexOf(decl, ae, stmt)), after_stmt))
                        } else {
                            Ok((Rc::new(ForInOfStatement::LexOf(decl, ae, stmt)), after_stmt))
                        }
                    }
                    _ => {
                        let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        Ok((Rc::new(ForInOfStatement::LexIn(decl, exp, stmt)), after_stmt))
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
                    // Any error message here is masked by the lexical production above, so don't bother writing one.
                    return Err(ParseError::new(String::new(), after_open.line, after_open.column));
                }
                let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, after_open, yield_flag, await_flag)?;
                let (kwd, after_kwd) = if await_seen {
                    (Keyword::Of, scan_for_keyword(after_lhs, parser.source, ScanGoal::InputElementRegExp, Keyword::Of)?)
                } else {
                    scan_for_keywords(after_lhs, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Of, Keyword::In])?
                };
                match kwd {
                    Keyword::Of => {
                        let (ae, after_ae) = AssignmentExpression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if await_seen {
                            Ok((Rc::new(ForInOfStatement::AwaitOf(lhs, ae, stmt)), after_stmt))
                        } else {
                            Ok((Rc::new(ForInOfStatement::Of(lhs, ae, stmt)), after_stmt))
                        }
                    }
                    _ => {
                        let (exp, after_exp) = Expression::parse(parser, after_kwd, true, yield_flag, await_flag)?;
                        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                        let (stmt, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        Ok((Rc::new(ForInOfStatement::In(lhs, exp, stmt)), after_stmt))
                    }
                }
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            ForInOfStatement::In(_, _, s) => s.var_declared_names(),
            ForInOfStatement::LexIn(_, _, s) => s.var_declared_names(),
            ForInOfStatement::Of(_, _, s) => s.var_declared_names(),
            ForInOfStatement::LexOf(_, _, s) => s.var_declared_names(),
            ForInOfStatement::AwaitOf(_, _, s) => s.var_declared_names(),
            ForInOfStatement::AwaitLexOf(_, _, s) => s.var_declared_names(),
            ForInOfStatement::VarIn(v, _, s) | ForInOfStatement::VarOf(v, _, s) | ForInOfStatement::AwaitVarOf(v, _, s) => {
                let mut names = v.bound_names();
                names.extend(s.var_declared_names());
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            ForInOfStatement::In(_, _, s)
            | ForInOfStatement::LexIn(_, _, s)
            | ForInOfStatement::Of(_, _, s)
            | ForInOfStatement::LexOf(_, _, s)
            | ForInOfStatement::AwaitOf(_, _, s)
            | ForInOfStatement::AwaitLexOf(_, _, s)
            | ForInOfStatement::VarIn(_, _, s)
            | ForInOfStatement::VarOf(_, _, s)
            | ForInOfStatement::AwaitVarOf(_, _, s) => s.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ForInOfStatement::In(lhs, e, s) => lhs.contains(kind) || e.contains(kind) || s.contains(kind),
            ForInOfStatement::VarIn(v, e, s) => v.contains(kind) || e.contains(kind) || s.contains(kind),
            ForInOfStatement::LexIn(lex, e, s) => lex.contains(kind) || e.contains(kind) || s.contains(kind),
            ForInOfStatement::Of(lhs, e, s) | ForInOfStatement::AwaitOf(lhs, e, s) => lhs.contains(kind) || e.contains(kind) || s.contains(kind),
            ForInOfStatement::VarOf(v, e, s) | ForInOfStatement::AwaitVarOf(v, e, s) => v.contains(kind) || e.contains(kind) || s.contains(kind),
            ForInOfStatement::LexOf(lex, e, s) | ForInOfStatement::AwaitLexOf(lex, e, s) => lex.contains(kind) || e.contains(kind) || s.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            ForInOfStatement::In(_, _, s)
            | ForInOfStatement::LexIn(_, _, s)
            | ForInOfStatement::Of(_, _, s)
            | ForInOfStatement::LexOf(_, _, s)
            | ForInOfStatement::AwaitOf(_, _, s)
            | ForInOfStatement::AwaitLexOf(_, _, s)
            | ForInOfStatement::VarIn(_, _, s)
            | ForInOfStatement::VarOf(_, _, s)
            | ForInOfStatement::AwaitVarOf(_, _, s) => s.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            ForInOfStatement::In(_, _, s)
            | ForInOfStatement::LexIn(_, _, s)
            | ForInOfStatement::Of(_, _, s)
            | ForInOfStatement::LexOf(_, _, s)
            | ForInOfStatement::AwaitOf(_, _, s)
            | ForInOfStatement::AwaitLexOf(_, _, s)
            | ForInOfStatement::VarIn(_, _, s)
            | ForInOfStatement::VarOf(_, _, s)
            | ForInOfStatement::AwaitVarOf(_, _, s) => s.contains_undefined_continue_target(iteration_set, &[]),
        }
    }
}

fn pp_two<T, U, UU, V, VV>(writer: &mut T, pad: &str, n1: &U, n2: &V) -> IoResult<()>
where
    T: Write,
    U: AsRef<UU>,
    UU: PrettyPrint,
    V: AsRef<VV>,
    VV: PrettyPrint,
{
    n1.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.as_ref().pprint_with_leftpad(writer, pad, Spot::Final)
}

fn pp_three<T, U, V, W, X, Y, Z>(writer: &mut T, pad: &str, n1: &U, n2: &V, n3: &W) -> IoResult<()>
where
    T: Write,
    U: AsRef<X>,
    X: PrettyPrint,
    V: AsRef<Y>,
    Y: PrettyPrint,
    W: AsRef<Z>,
    Z: PrettyPrint,
{
    n1.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n3.as_ref().pprint_with_leftpad(writer, pad, Spot::Final)
}

fn pp_four<T, U, UU, V, VV, W, WW, X, XX>(writer: &mut T, pad: &str, n1: &U, n2: &V, n3: &W, n4: &X) -> IoResult<()>
where
    T: Write,
    U: AsRef<UU>,
    UU: PrettyPrint,
    V: AsRef<VV>,
    VV: PrettyPrint,
    W: AsRef<WW>,
    WW: PrettyPrint,
    X: AsRef<XX>,
    XX: PrettyPrint,
{
    n1.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n2.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n3.as_ref().pprint_with_leftpad(writer, pad, Spot::NotFinal)?;
    n4.as_ref().pprint_with_leftpad(writer, pad, Spot::Final)
}

// ForDeclaration[Yield, Await] :
//      LetOrConst ForBinding[?Yield, ?Await]
#[derive(Debug)]
pub enum ForDeclaration {
    Binding(LetOrConst, Rc<ForBinding>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (tok, after_tok) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Let, Keyword::Const])?;
        let loc = match tok {
            Keyword::Let => LetOrConst::Let,
            _ => LetOrConst::Const,
        };
        let (binding, after_binding) = ForBinding::parse(parser, after_tok, yield_flag, await_flag)?;
        Ok((Rc::new(ForDeclaration::Binding(loc, binding)), after_binding))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let ForDeclaration::Binding(loc, node) = self;
        loc.contains(kind) || node.contains(kind)
    }
}

// ForBinding[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum ForBinding {
    Identifier(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("ForBinding expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (id, after_id) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(ForBinding::Identifier(id)), after_id))
            })
            .otherwise(|| {
                let (pat, after_pat) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(ForBinding::Pattern(pat)), after_pat))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.for_binding_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.for_binding_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ForBinding::Identifier(node) => node.bound_names(),
            ForBinding::Pattern(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ForBinding::Identifier(node) => node.contains(kind),
            ForBinding::Pattern(node) => node.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;

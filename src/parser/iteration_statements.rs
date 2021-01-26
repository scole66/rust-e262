use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::comma_operator::Expression;
use super::declarations_and_variables::{BindingPattern, LetOrConst, LexicalDeclaration, VariableDeclarationList};
use super::identifiers::BindingIdentifier;
use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::Scanner;
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
}

impl IterationStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_dowhile = DoWhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((dowhile, after_dowhile)) = pot_dowhile {
            return Ok(Some((Box::new(IterationStatement::DoWhile(dowhile)), after_dowhile)));
        }

        let pot_while = WhileStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((whl, after_whl)) = pot_while {
            return Ok(Some((Box::new(IterationStatement::While(whl)), after_whl)));
        }

        let pot_for = ForStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((fr, after_fr)) = pot_for {
            return Ok(Some((Box::new(IterationStatement::For(fr)), after_fr)));
        }

        let pot_forin = ForInOfStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((forin, after_forin)) = pot_forin {
            return Ok(Some((Box::new(IterationStatement::ForInOf(forin)), after_forin)));
        }

        Ok(None)
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
}

impl DoWhileStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Do)) {
            // do Statement while ( Expression ) ;
            let pot_stmt = Statement::parse(parser, after_tok, yield_flag, await_flag, return_flag)?;
            if let Some((stmt, after_stmt)) = pot_stmt {
                let (tok, after_tok) =
                    scanner::scan_token(&after_stmt, parser.source, scanner::ScanGoal::InputElementRegExp);
                if matches!(tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::While)) {
                    let (tok, after_tok) =
                        scanner::scan_token(&after_tok, parser.source, scanner::ScanGoal::InputElementRegExp);
                    if tok == scanner::Token::LeftParen {
                        let pot_exp = Expression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                        if let Some((exp, after_exp)) = pot_exp {
                            let (tok, after_tok) =
                                scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementDiv);
                            if tok == scanner::Token::RightParen {
                                let (tok, after_tok) = scanner::scan_token(
                                    &after_tok,
                                    parser.source,
                                    scanner::ScanGoal::InputElementRegExp,
                                );
                                if tok == scanner::Token::Semicolon {
                                    return Ok(Some((Box::new(DoWhileStatement::Do(stmt, exp)), after_tok)));
                                }
                            }
                        }
                    }
                }
            }
            return Ok(None);
        }
        Ok(None)
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
}

impl WhileStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok1, after_tok1) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(tok1, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::While)) {
            let (tok2, after_tok2) =
                scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            if tok2 == scanner::Token::LeftParen {
                let pot_exp = Expression::parse(parser, after_tok2, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (tok3, after_tok3) =
                        scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    if tok3 == scanner::Token::RightParen {
                        let pot_stmt = Statement::parse(parser, after_tok3, yield_flag, await_flag, return_flag)?;
                        if let Some((stmt, after_stmt)) = pot_stmt {
                            return Ok(Some((Box::new(WhileStatement::While(exp, stmt)), after_stmt)));
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}

// ForStatement[Yield, Await, Return] :
//      for ( [lookahead ≠ let [] Expression[~In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
//      for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum ForStatement {
    For(
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    ForVar(
        Box<VariableDeclarationList>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    ForLex(
        Box<LexicalDeclaration>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
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
                write!(f, "for ( {} ; {} ; {} ) {}", lex, e1, e2, s)
            }
            ForStatement::ForLex(lex, Some(e1), None, s) => write!(f, "for ( {} ; {} ; ) {}", lex, e1, s),
            ForStatement::ForLex(lex, None, Some(e2), s) => write!(f, "for ( {} ; ; {} ) {}", lex, e2, s),
            ForStatement::ForLex(lex, None, None, s) => write!(f, "for ( {} ; ; ) {}", lex, s),
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
}

fn keyword_matches(tok: &scanner::Token, kwd: scanner::Keyword) -> bool {
    matches!(&tok, scanner::Token::Identifier(id) if id.keyword_id == Some(kwd))
}

impl ForStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok1, after_tok1) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if keyword_matches(&tok1, scanner::Keyword::For) {
            let (tok2, after_tok2) =
                scanner::scan_token(&after_tok1, parser.source, scanner::ScanGoal::InputElementDiv);
            if tok2 == scanner::Token::LeftParen {
                let (tok3, after_tok3) =
                    scanner::scan_token(&after_tok2, parser.source, scanner::ScanGoal::InputElementRegExp);
                // for ( var VariableDeclarationList ; Expression ; Expression ) Statement
                if keyword_matches(&tok3, scanner::Keyword::Var) {
                    let pot_vdl = VariableDeclarationList::parse(parser, after_tok3, false, yield_flag, await_flag)?;
                    if let Some((vdl, after_vdl)) = pot_vdl {
                        let (semi1, after_semi1) =
                            scanner::scan_token(&after_vdl, parser.source, scanner::ScanGoal::InputElementDiv);
                        if semi1 == scanner::Token::Semicolon {
                            let pot_condition = Expression::parse(parser, after_semi1, true, yield_flag, await_flag)?;
                            let (condition, after_condition) = match pot_condition {
                                None => (None, after_semi1),
                                Some((c, s)) => (Some(c), s),
                            };
                            let (semi2, after_semi2) = scanner::scan_token(
                                &after_condition,
                                parser.source,
                                scanner::ScanGoal::InputElementDiv,
                            );
                            if semi2 == scanner::Token::Semicolon {
                                let pot_increment =
                                    Expression::parse(parser, after_semi2, true, yield_flag, await_flag)?;
                                let (increment, after_increment) = match pot_increment {
                                    None => (None, after_semi2),
                                    Some((i, s)) => (Some(i), s),
                                };
                                let (closing, after_closing) = scanner::scan_token(
                                    &after_increment,
                                    parser.source,
                                    scanner::ScanGoal::InputElementDiv,
                                );
                                if closing == scanner::Token::RightParen {
                                    let pot_stmt =
                                        Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                    if let Some((stmt, after_stmt)) = pot_stmt {
                                        return Ok(Some((
                                            Box::new(ForStatement::ForVar(vdl, condition, increment, stmt)),
                                            after_stmt,
                                        )));
                                    }
                                }
                            }
                        }
                    }
                }

                // for ( LexicalDeclaration Expression ; Expression ) Statement
                if keyword_matches(&tok3, scanner::Keyword::Let) | keyword_matches(&tok3, scanner::Keyword::Const) {
                    let pot_decl = LexicalDeclaration::parse(parser, after_tok2, false, yield_flag, await_flag)?;
                    if let Some((decl, after_decl)) = pot_decl {
                        let pot_condition = Expression::parse(parser, after_decl, true, yield_flag, await_flag)?;
                        let (condition, after_condition) = match pot_condition {
                            None => (None, after_decl),
                            Some((c, s)) => (Some(c), s),
                        };
                        let (semi2, after_semi2) =
                            scanner::scan_token(&after_condition, parser.source, scanner::ScanGoal::InputElementDiv);
                        if semi2 == scanner::Token::Semicolon {
                            let pot_increment = Expression::parse(parser, after_semi2, true, yield_flag, await_flag)?;
                            let (increment, after_increment) = match pot_increment {
                                None => (None, after_semi2),
                                Some((i, s)) => (Some(i), s),
                            };
                            let (closing, after_closing) = scanner::scan_token(
                                &after_increment,
                                parser.source,
                                scanner::ScanGoal::InputElementDiv,
                            );
                            if closing == scanner::Token::RightParen {
                                let pot_stmt =
                                    Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                if let Some((stmt, after_stmt)) = pot_stmt {
                                    return Ok(Some((
                                        Box::new(ForStatement::ForLex(decl, condition, increment, stmt)),
                                        after_stmt,
                                    )));
                                }
                            }
                        }
                    }
                }

                // for ( Expression ; Expression ; Expression ) Statement
                let (tok4, after_tok4) =
                    scanner::scan_token(&after_tok3, parser.source, scanner::ScanGoal::InputElementDiv);
                if !keyword_matches(&tok3, scanner::Keyword::Let) || tok4 != scanner::Token::LeftBracket {
                    let pot_init = Expression::parse(parser, after_tok2, false, yield_flag, await_flag)?;
                    let (init, after_init) = match pot_init {
                        None => (None, after_tok2),
                        Some((i, s)) => (Some(i), s),
                    };
                    let (semi1, after_semi1) =
                        scanner::scan_token(&after_init, parser.source, scanner::ScanGoal::InputElementDiv);
                    if semi1 == scanner::Token::Semicolon {
                        let pot_condition = Expression::parse(parser, after_semi1, true, yield_flag, await_flag)?;
                        let (condition, after_condition) = match pot_condition {
                            None => (None, after_semi1),
                            Some((c, s)) => (Some(c), s),
                        };
                        let (semi2, after_semi2) =
                            scanner::scan_token(&after_condition, parser.source, scanner::ScanGoal::InputElementDiv);
                        if semi2 == scanner::Token::Semicolon {
                            let pot_increment = Expression::parse(parser, after_semi2, true, yield_flag, await_flag)?;
                            let (increment, after_increment) = match pot_increment {
                                None => (None, after_semi2),
                                Some((i, s)) => (Some(i), s),
                            };
                            let (closing, after_closing) = scanner::scan_token(
                                &after_increment,
                                parser.source,
                                scanner::ScanGoal::InputElementDiv,
                            );
                            if closing == scanner::Token::RightParen {
                                let pot_stmt =
                                    Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                if let Some((stmt, after_stmt)) = pot_stmt {
                                    return Ok(Some((
                                        Box::new(ForStatement::For(init, condition, increment, stmt)),
                                        after_stmt,
                                    )));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
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
}

impl ForInOfStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (for_token, after_for) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if keyword_matches(&for_token, scanner::Keyword::For) {
            let (paren_or_await, after_poa) =
                scanner::scan_token(&after_for, parser.source, scanner::ScanGoal::InputElementDiv);
            let await_seen = await_flag && keyword_matches(&paren_or_await, scanner::Keyword::Await);
            let (opening, after_opening) = match await_seen {
                true => scanner::scan_token(&after_poa, parser.source, scanner::ScanGoal::InputElementDiv),
                false => (paren_or_await, after_poa),
            };
            if opening == scanner::Token::LeftParen {
                let (first, after_first) =
                    scanner::scan_token(&after_opening, parser.source, scanner::ScanGoal::InputElementRegExp);
                let (second, after_second) =
                    scanner::scan_token(&after_opening, parser.source, scanner::ScanGoal::InputElementDiv);
                if keyword_matches(&first, scanner::Keyword::Var) {
                    let pot_fb = ForBinding::parse(parser, after_first, yield_flag, await_flag)?;
                    if let Some((fb, after_fb)) = pot_fb {
                        let (tok_in_of, after_inof) =
                            scanner::scan_token(&after_fb, parser.source, scanner::ScanGoal::InputElementDiv);
                        if !await_seen && keyword_matches(&tok_in_of, scanner::Keyword::In) {
                            let pot_exp = Expression::parse(parser, after_inof, true, yield_flag, await_flag)?;
                            if let Some((exp, after_exp)) = pot_exp {
                                let (closing, after_closing) =
                                    scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementDiv);
                                if closing == scanner::Token::RightParen {
                                    let pot_stmt =
                                        Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                    if let Some((stmt, after_stmt)) = pot_stmt {
                                        return Ok(Some((
                                            Box::new(ForInOfStatement::ForVarIn(fb, exp, stmt)),
                                            after_stmt,
                                        )));
                                    }
                                }
                            }
                        } else if keyword_matches(&tok_in_of, scanner::Keyword::Of) {
                            let pot_ae = AssignmentExpression::parse(parser, after_inof, true, yield_flag, await_flag)?;
                            if let Some((ae, after_ae)) = pot_ae {
                                let (closing, after_closing) =
                                    scanner::scan_token(&after_ae, parser.source, scanner::ScanGoal::InputElementDiv);
                                if closing == scanner::Token::RightParen {
                                    let pot_stmt =
                                        Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                    if let Some((stmt, after_stmt)) = pot_stmt {
                                        if await_seen {
                                            return Ok(Some((
                                                Box::new(ForInOfStatement::ForAwaitVarOf(fb, ae, stmt)),
                                                after_stmt,
                                            )));
                                        } else {
                                            return Ok(Some((
                                                Box::new(ForInOfStatement::ForVarOf(fb, ae, stmt)),
                                                after_stmt,
                                            )));
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    let pot_fd = ForDeclaration::parse(parser, after_opening, yield_flag, await_flag)?;
                    if let Some((fd, after_fd)) = pot_fd {
                        let (inof, after_inof) =
                            scanner::scan_token(&after_fd, parser.source, scanner::ScanGoal::InputElementDiv);
                        if !await_seen && keyword_matches(&inof, scanner::Keyword::In) {
                            let pot_exp = Expression::parse(parser, after_inof, true, yield_flag, await_flag)?;
                            if let Some((exp, after_exp)) = pot_exp {
                                let (closing, after_closing) =
                                    scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementDiv);
                                if closing == scanner::Token::RightParen {
                                    let pot_stmt =
                                        Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                    if let Some((stmt, after_stmt)) = pot_stmt {
                                        return Ok(Some((
                                            Box::new(ForInOfStatement::ForLexIn(fd, exp, stmt)),
                                            after_stmt,
                                        )));
                                    }
                                }
                            }
                        } else if keyword_matches(&inof, scanner::Keyword::Of) {
                            let pot_ae = AssignmentExpression::parse(parser, after_inof, true, yield_flag, await_flag)?;
                            if let Some((ae, after_ae)) = pot_ae {
                                let (closing, after_closing) =
                                    scanner::scan_token(&after_ae, parser.source, scanner::ScanGoal::InputElementDiv);
                                if closing == scanner::Token::RightParen {
                                    let pot_stmt =
                                        Statement::parse(parser, after_closing, yield_flag, await_flag, return_flag)?;
                                    if let Some((stmt, after_stmt)) = pot_stmt {
                                        if await_seen {
                                            return Ok(Some((
                                                Box::new(ForInOfStatement::ForAwaitLexOf(fd, ae, stmt)),
                                                after_stmt,
                                            )));
                                        } else {
                                            return Ok(Some((
                                                Box::new(ForInOfStatement::ForLexOf(fd, ae, stmt)),
                                                after_stmt,
                                            )));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if !await_seen
                        && (!keyword_matches(&first, scanner::Keyword::Let) || second != scanner::Token::LeftBracket)
                    {
                        let pot_lhs = LeftHandSideExpression::parse(parser, after_opening, yield_flag, await_flag)?;
                        if let Some((lhs, after_lhs)) = pot_lhs {
                            let (tok_in, after_in) =
                                scanner::scan_token(&after_lhs, parser.source, scanner::ScanGoal::InputElementDiv);
                            if keyword_matches(&tok_in, scanner::Keyword::In) {
                                let pot_exp = Expression::parse(parser, after_in, true, yield_flag, await_flag)?;
                                if let Some((exp, after_exp)) = pot_exp {
                                    let (closing, after_closing) = scanner::scan_token(
                                        &after_exp,
                                        parser.source,
                                        scanner::ScanGoal::InputElementDiv,
                                    );
                                    if closing == scanner::Token::RightParen {
                                        let pot_stmt = Statement::parse(
                                            parser,
                                            after_closing,
                                            yield_flag,
                                            await_flag,
                                            return_flag,
                                        )?;
                                        if let Some((stmt, after_stmt)) = pot_stmt {
                                            return Ok(Some((
                                                Box::new(ForInOfStatement::ForIn(lhs, exp, stmt)),
                                                after_stmt,
                                            )));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if !keyword_matches(&first, scanner::Keyword::Let) {
                        let pot_lhs = LeftHandSideExpression::parse(parser, after_opening, yield_flag, await_flag)?;
                        if let Some((lhs, after_lhs)) = pot_lhs {
                            let (tok_of, after_of) =
                                scanner::scan_token(&after_lhs, parser.source, scanner::ScanGoal::InputElementDiv);
                            if keyword_matches(&tok_of, scanner::Keyword::Of) {
                                let pot_exp =
                                    AssignmentExpression::parse(parser, after_of, true, yield_flag, await_flag)?;
                                if let Some((exp, after_exp)) = pot_exp {
                                    let (closing, after_closing) = scanner::scan_token(
                                        &after_exp,
                                        parser.source,
                                        scanner::ScanGoal::InputElementDiv,
                                    );
                                    if closing == scanner::Token::RightParen {
                                        let pot_stmt = Statement::parse(
                                            parser,
                                            after_closing,
                                            yield_flag,
                                            await_flag,
                                            return_flag,
                                        )?;
                                        if let Some((stmt, after_stmt)) = pot_stmt {
                                            if await_seen {
                                                return Ok(Some((
                                                    Box::new(ForInOfStatement::ForAwaitOf(lhs, exp, stmt)),
                                                    after_stmt,
                                                )));
                                            } else {
                                                return Ok(Some((
                                                    Box::new(ForInOfStatement::ForOf(lhs, exp, stmt)),
                                                    after_stmt,
                                                )));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
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
}

impl ForDeclaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        let loc = match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Let) => LetOrConst::Let,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Const) => LetOrConst::Const,
            _ => {
                return Ok(None);
            }
        };
        let pot_binding = ForBinding::parse(parser, after_tok, yield_flag, await_flag)?;
        if let Some((binding, after_binding)) = pot_binding {
            return Ok(Some((Box::new(ForDeclaration::Binding(loc, binding)), after_binding)));
        }
        Ok(None)
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
}

impl ForBinding {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_id = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((id, after_id)) = pot_id {
            return Ok(Some((Box::new(ForBinding::Identifier(id)), after_id)));
        }

        let pot_pat = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((pat, after_pat)) = pot_pat {
            return Ok(Some((Box::new(ForBinding::Pattern(pat)), after_pat)));
        }

        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}

use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

// Script :
//      ScriptBody opt
#[derive(Debug)]
pub struct Script(Option<Rc<ScriptBody>>);

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            None => Ok(()),
            Some(n) => n.fmt(f),
        }
    }
}

impl PrettyPrint for Script {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Script: {}", first, self)?;
        if let Some(body) = &self.0 {
            body.pprint_with_leftpad(writer, &successive, Spot::Final)
        } else {
            Ok(())
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.0 {
            None => {
                let (first, _) = prettypad(pad, state);
                writeln!(writer, "{}Script:", first)
            }
            Some(body) => body.concise_with_leftpad(writer, pad, state),
        }
    }
}

use ahash::AHashSet;
use std::hash::Hash;
fn has_unique_elements<T>(iter: T) -> bool
where
    T: IntoIterator,
    T::Item: Eq + Hash,
{
    let mut uniq = AHashSet::new();
    iter.into_iter().all(move |x| uniq.insert(x))
}

impl Script {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (script, after_script, err) = match ScriptBody::parse(parser, scanner) {
            Ok((node, scan)) => (Some(node), scan, None),
            Err(err) => (None, scanner, Some(err)),
        };
        let end = scan_for_eof(after_script, parser.source);
        match end {
            Ok(scan) => Ok((Rc::new(Script(script)), scan)),
            Err(e) => match err {
                Some(x) => Err(x),
                None => Err(e),
            },
        }
    }

    // Static Semantics: Early Errors
    //      Script : ScriptBody
    // * It is a Syntax Error if the LexicallyDeclaredNames of ScriptBody contains any duplicate entries.
    // * It is a Syntax Error if any element of the LexicallyDeclaredNames of ScriptBody also occurs in the
    //   VarDeclaredNames of ScriptBody.
    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        match &self.0 {
            Some(body) => {
                let mut errs: Vec<Object> = Vec::new();
                let lex_names = body.lexically_declared_names();
                let var_names = body.var_declared_names();
                if !has_unique_elements(lex_names.clone()) {
                    errs.push(create_syntax_error_object(agent, "Duplicate lexically declared names"));
                }
                let lex_names_set: AHashSet<JSString> = lex_names.into_iter().collect();
                let var_names_set: AHashSet<JSString> = var_names.into_iter().collect();
                if !lex_names_set.is_disjoint(&var_names_set) {
                    errs.push(create_syntax_error_object(agent, "Name defined both lexically and var-style"));
                }
                errs.extend(body.early_errors(agent));
                errs
            }
            None => vec![],
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.0 {
            None => false,
            Some(n) => kind == ParseNodeKind::ScriptBody || n.contains(kind),
        }
    }
}

// ScriptBody :
//      StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
pub struct ScriptBody {
    statement_list: Rc<StatementList>,
    direct: bool,
}

impl fmt::Display for ScriptBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statement_list.fmt(f)
    }
}

impl PrettyPrint for ScriptBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ScriptBody: {}", first, self)?;
        self.statement_list.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.statement_list.concise_with_leftpad(writer, pad, state)
    }
}

impl ScriptBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (sl, after_sl) = StatementList::parse(parser, scanner, false, false, false)?;
        Ok((Rc::new(ScriptBody { statement_list: sl, direct: parser.direct }), after_sl))
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        self.statement_list.top_level_lexically_declared_names()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.statement_list.top_level_var_declared_names()
    }

    // ScriptBody : StatementList
    //  * It is a Syntax Error if StatementList Contains super unless the source code containing super is eval code that
    //    is being processed by a direct eval. Additional early error rules for super within direct eval are defined in
    //    19.2.1.1.
    //  * It is a Syntax Error if StatementList Contains NewTarget unless the source code containing NewTarget is eval
    //    code that is being processed by a direct eval. Additional early error rules for NewTarget in direct eval are
    //    defined in 19.2.1.1.
    //  * It is a Syntax Error if ContainsDuplicateLabels of StatementList with argument « » is true.
    //  * It is a Syntax Error if ContainsUndefinedBreakTarget of StatementList with argument « » is true.
    //  * It is a Syntax Error if ContainsUndefinedContinueTarget of StatementList with arguments « » and « » is true.
    //  * It is a Syntax Error if AllPrivateIdentifiersValid of StatementList with argument « » is false unless the
    //    source code containing ScriptBody is eval code that is being processed by a direct eval.
    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        let mut errs = vec![];
        if !self.direct {
            if self.statement_list.contains(ParseNodeKind::Super) {
                errs.push(create_syntax_error_object(agent, "`super' not allowed in top-level code"));
            }
            if self.statement_list.contains(ParseNodeKind::NewTarget) {
                errs.push(create_syntax_error_object(agent, "`new.target` not allowed in top-level code"));
            }
        }
        if self.statement_list.contains_duplicate_labels(&[]) {
            errs.push(create_syntax_error_object(agent, "duplicate labels detected"));
        }
        if self.statement_list.contains_undefined_break_target(&[]) {
            errs.push(create_syntax_error_object(agent, "undefined break target detected"));
        }
        if self.statement_list.contains_undefined_continue_target(&[], &[]) {
            errs.push(create_syntax_error_object(agent, "undefined continue target detected"));
        }
        //if !self.direct && !self.statement_list.all_private_identifier_valid(vec![])  {
        //    errs.push(create_syntax_error_object(agent, "invalid private identifier detected"));
        //}
        //errs.extend(self.statement_list.early_errors(agent));
        errs
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::StatementList || self.statement_list.contains(kind)
    }
}

#[cfg(test)]
mod tests;

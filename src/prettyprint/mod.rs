use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

#[derive(Copy, Clone)]
pub enum Spot {
    Initial,
    NotFinal,
    Final,
}

pub fn prettypad(pad: &str, state: Spot) -> (String, String) {
    let mut first = String::from(pad);
    let mut successive = String::from(pad);
    match state {
        Spot::Initial => {}
        Spot::NotFinal => {
            first.push_str("├── ");
            successive.push_str("│   ");
        }
        Spot::Final => {
            first.push_str("└── ");
            successive.push_str("    ");
        }
    }
    (first, successive)
}

#[derive(Debug, Copy, Clone)]
pub enum TokenType {
    Keyword,
    Punctuator,
    IdentifierName,
    RegularExpression,
    Numeric,
    String,
    NoSubTemplate,
    TemplateHead,
    TemplateMiddle,
    TemplateTail,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            TokenType::Keyword => "Keyword",
            TokenType::Punctuator => "Punctuator",
            TokenType::IdentifierName => "IdentifierName",
            TokenType::RegularExpression => "RegularExpressionLiteral",
            TokenType::Numeric => "Numeric",
            TokenType::String => "String",
            TokenType::NoSubTemplate => "NoSubTemplate",
            TokenType::TemplateHead => "TemplateHead",
            TokenType::TemplateMiddle => "TemplateMiddle",
            TokenType::TemplateTail => "TemplateTail",
        })
    }
}

pub fn pprint_token<T, U>(writer: &mut T, tokstr: U, kind: TokenType, pad: &str, state: Spot) -> IoResult<()>
where
    T: Write,
    U: fmt::Display,
{
    let (first, _) = prettypad(pad, state);
    writeln!(writer, "{}{}: {}", first, kind, tokstr)
}

pub trait PrettyPrint {
    fn pprint<T>(&self, writer: &mut T) -> IoResult<()>
    where
        T: Write,
    {
        self.pprint_with_leftpad(writer, "", Spot::Initial)
    }
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write;

    fn pprint_concise<T>(&self, writer: &mut T) -> IoResult<()>
    where
        T: Write,
    {
        self.concise_with_leftpad(writer, "", Spot::Initial)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write;
}

#[cfg(test)]
pub mod testhelp;

#[cfg(test)]
pub mod tests;

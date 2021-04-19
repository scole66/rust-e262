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
        match self {
            TokenType::Keyword => f.write_str("Keyword"),
            TokenType::Punctuator => f.write_str("Punctuator"),
            TokenType::IdentifierName => f.write_str("IdentifierName"),
            TokenType::RegularExpression => f.write_str("RegularExpressionLiteral"),
            TokenType::Numeric => f.write_str("Numeric"),
            TokenType::String => f.write_str("String"),
            TokenType::NoSubTemplate => f.write_str("NoSubTemplate"),
            TokenType::TemplateHead => f.write_str("TemplateHead"),
            TokenType::TemplateMiddle => f.write_str("TemplateMiddle"),
            TokenType::TemplateTail => f.write_str("TemplateTail"),
        }
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
pub mod testhelp {
    use super::*;
    use std::str;

    fn check_message(msg: &str, selfstring: &str, childstrings: Vec<&str>) {
        let mut line_iter = msg.split('\n');
        let first = line_iter.next().unwrap();
        assert_eq!(first, selfstring);

        let mut expected_iter = childstrings.iter();
        for line in line_iter {
            if let Some(0) = line.find(|c| c == '└' || c == '├') {
                println!("Checking child line {:?}", line);
                let expected = expected_iter.next().expect("Too many child strings");
                println!("Against the expected line {:?}", expected);
                // swallow the first 4 chars:
                let mut ch_iter = line.chars();
                for _ in 0..4 {
                    ch_iter.next().unwrap();
                }
                let child_content: Vec<char> = ch_iter.collect();
                let expected_content: Vec<char> = expected.chars().collect();
                assert_eq!(child_content, expected_content);
            }
        }
        assert!(expected_iter.next().is_none());
    }

    pub fn pretty_check<T>(item: &T, selfstring: &str, childstrings: Vec<&str>)
    where
        T: PrettyPrint,
    {
        let mut msg = Vec::new();
        item.pprint(&mut msg).unwrap();
        let whole_message = str::from_utf8(&msg).unwrap();
        testhelp::check_message(&whole_message, selfstring, childstrings);
    }

    pub fn concise_check<T>(item: &T, selfstring: &str, childstrings: Vec<&str>)
    where
        T: PrettyPrint,
    {
        let mut msg = Vec::new();
        item.pprint_concise(&mut msg).unwrap();
        let whole_message = str::from_utf8(&msg).unwrap();
        testhelp::check_message(&whole_message, selfstring, childstrings);
    }

    struct MockWriter<T>
    where
        T: Write,
    {
        writer: T,
        pub count: usize,
        target: usize,
        pub error_generated: bool,
    }
    impl<T> std::io::Write for MockWriter<T>
    where
        T: Write,
    {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.count += 1;
            if self.count >= self.target {
                self.error_generated = true;
                Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"))
            } else {
                self.writer.write(buf)
            }
        }
        fn flush(&mut self) -> std::io::Result<()> {
            self.writer.flush()
        }
    }
    impl<T> MockWriter<T>
    where
        T: Write,
    {
        fn new(writer: T, errat: usize) -> Self {
            MockWriter { writer, count: 0, target: errat, error_generated: false }
        }
    }
    fn printer_validate<U>(func: U)
    where
        U: Fn(&mut MockWriter<Vec<u8>>) -> IoResult<()>,
    {
        let mut target = 1;
        loop {
            let mut writer = MockWriter::new(Vec::new(), target);
            let result = func(&mut writer);
            assert!(result.is_err() || !writer.error_generated);
            if !writer.error_generated {
                break;
            }
            target += 1;
        }
    }
    pub fn pretty_error_validate<T>(item: &T)
    where
        T: PrettyPrint,
    {
        printer_validate(|w| item.pprint(w))
    }
    pub fn concise_error_validate<T>(item: &T)
    where
        T: PrettyPrint,
    {
        printer_validate(|w| item.pprint_concise(w))
    }
}

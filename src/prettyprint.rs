use std::io::Result as IoResult;
use std::io::Write;

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
}

#[cfg(test)]
pub mod testhelp {
    use super::*;
    use std::str;
    pub fn pretty_check<T>(item: &T, selfstring: &str, childstrings: Vec<&str>)
    where
        T: PrettyPrint,
    {
        let mut msg = Vec::new();
        item.pprint(&mut msg).unwrap();
        let whole_message = str::from_utf8(&msg).unwrap();

        let mut line_iter = whole_message.split("\n");
        let first = line_iter.next().unwrap();
        assert_eq!(first, selfstring);

        let mut expected_iter = childstrings.iter();
        for line in line_iter {
            match line.find(|c| c == '└' || c == '├') {
                Some(0) => {
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
                _ => {}
            }
        }
        assert!(expected_iter.next().is_none());
    }
}

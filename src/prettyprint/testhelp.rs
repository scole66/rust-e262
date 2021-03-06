use super::*;
use crate::tests::printer_validate;
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
    testhelp::check_message(whole_message, selfstring, childstrings);
}

pub fn concise_check<T>(item: &T, selfstring: &str, childstrings: Vec<&str>)
where
    T: PrettyPrint,
{
    let mut msg = Vec::new();
    item.pprint_concise(&mut msg).unwrap();
    let whole_message = str::from_utf8(&msg).unwrap();
    testhelp::check_message(whole_message, selfstring, childstrings);
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

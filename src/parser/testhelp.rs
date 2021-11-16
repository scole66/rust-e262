use super::*;
use std::fmt;
pub fn check<T>(res: ParseResult<T>) -> (Rc<T>, Scanner) {
    assert!(res.is_ok());
    res.unwrap()
}
pub fn check_err<T>(res: ParseResult<T>, msg: &str, line: u32, column: u32)
where
    T: fmt::Debug,
{
    assert!(res.is_err());
    let err = res.unwrap_err();
    assert_eq!((err.msg, err.line, err.column), (String::from(msg), line, column));
}
pub fn expected_scan(count: u32) -> Scanner {
    // Expected Scanner for tests. (The real world will be more varied.)
    Scanner { line: 1, column: count + 1, start_idx: count as usize }
}
pub fn sv(strings: &[&str]) -> Vec<String> {
    strings.iter().map(|s| String::from(*s)).collect()
}
pub fn chk_scan(scanner: &Scanner, count: u32) {
    assert_eq!(*scanner, expected_scan(count));
}
pub fn newparser(text: &str) -> Parser {
    Parser::new(text, false, false, ParseGoal::Script)
}
pub fn check_parse_error<T, U>(result: ParseResult<T>, msg: U)
where
    T: fmt::Debug,
    U: Into<String>,
{
    assert!(result.is_err());
    let pe = result.unwrap_err();
    assert_eq!(pe.line, 1);
    assert_eq!(pe.column, 1);
    assert_eq!(pe.msg, msg.into());
}

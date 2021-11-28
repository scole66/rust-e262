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
    let err = res.unwrap_err();
    assert_eq!(err.location.starting_line, line);
    assert_eq!(err.location.starting_column, column);
    assert_eq!(format!("{}", err), String::from(msg));
}
pub fn expected_scan(count: u32) -> Scanner {
    // Expected Scanner for tests. (The real world will be more varied.)
    Scanner { line: 1, column: count + 1, start_idx: count as usize }
}
pub fn sv(strings: &[&str]) -> Vec<String> {
    strings.iter().map(|s| String::from(*s)).collect()
}
pub fn expected_err(code: PECode, column: u32) -> ParseError2 {
    ParseError2 { code, location: Location { starting_line: 1, starting_column: column, span: Span { starting_index: column as usize - 1, length: 0 } } }
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
    let pe = result.unwrap_err();
    assert_eq!(pe.location, Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } });
    assert_eq!(format!("{}", pe), msg.into());
}

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

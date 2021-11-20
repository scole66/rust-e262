mod statething;
use statething::State;

fn main() {
    let s = State::parse("test_string");
    println!("{:?}", s.has("nonsense"));
}

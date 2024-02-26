pub mod ast;
pub mod parse;
pub mod rt;

fn get_only_one<R, I: Iterator<Item = R>>(mut iter: I) -> R {
    let one = iter.next().unwrap();
    assert!(iter.next().is_none());
    one
}

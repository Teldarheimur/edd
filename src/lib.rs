pub mod parse;
pub mod rt;
pub mod ttype;
pub mod flat;

#[track_caller]
fn get_only_one<R, I: Iterator<Item = R>>(mut iter: I) -> R {
    let one = iter.next().unwrap();
    assert!(iter.next().is_none());
    one
}

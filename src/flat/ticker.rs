use super::Global;

pub struct Ticker(u64);
impl Ticker {
    pub fn new() -> Self {
        Self(0)
    }
    pub fn tick(&mut self) -> u64 {
        let ret = self.0;
        self.0 += 1;
        ret
    }
}

pub struct StaticNamer<'a> {
    ticker: Ticker,
    prefix: &'a str,
}
impl<'a> StaticNamer<'a> {
    pub fn new(prefix: &'a str) -> Self {
        Self {
            prefix,
            ticker: Ticker::new(),
        }
    }
    pub fn new_global(&mut self, purpose: &str) -> Global {
        let s = format!("{}__{purpose}{}", self.prefix, self.ticker.tick()).into();
        Global(s)
    }
}

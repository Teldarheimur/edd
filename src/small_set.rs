#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmallSet<T> {
    inner: Vec<T>,
}

impl<T> SmallSet<T> {
    pub const fn new() -> Self {
        SmallSet { inner: Vec::new() }
    }
    pub fn into_vec(self) -> Vec<T> {
        self.inner
    }
    pub fn clear(&mut self) {
        self.inner.clear();
    }
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter()
    }
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.inner.into_iter()
    }
}

impl<T: Eq> SmallSet<T> {
    pub fn add(&mut self, elem: T) {
        if !self.inner.contains(&elem) {
            self.inner.push(elem);
        }
    }
    pub fn remove(&mut self, elem: &T) {
        for (i, e) in self.inner.iter().enumerate() {
            if e == elem {
                self.inner.remove(i);
                return;
            }
        }
    }
    pub fn contains(&self, elem: &T) -> bool {
        self.inner.contains(elem)
    }
    pub fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut ret = SmallSet::new();
        ret.extend(iter.into_iter());
        ret
    }
    pub fn extend(&mut self, iter: impl Iterator<Item=T>) {
        for elem in iter {
            self.add(elem);
        }
    }
    pub fn union(self, other: Self) -> Self {
        let mut ret = self;
        ret.extend(other.into_iter());
        ret
    }
    pub fn diff<'a, I: 'a + IntoIterator<Item=&'a T>>(mut self, other: I) -> Self where T: 'a {
        for elem in other.into_iter() {
            self.remove(elem);
        }
        self
    }
}

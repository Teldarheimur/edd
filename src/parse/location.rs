use std::{
    fmt::{self, Display},
    path::Path,
    rc::Rc,
};

use pest::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub source_file: Rc<Path>,
    pub line_start: u16,
    pub col_start: u16,
    pub line_end: u16,
    pub col_end: u16,
}
impl Location {
    pub fn new(source_file: Rc<Path>) -> Self {
        Location {
            source_file,
            line_start: 0,
            col_start: 0,
            line_end: 0,
            col_end: 0,
        }
    }
    pub fn from_span(sf: &Rc<Path>, span: Span) -> Self {
        let (line_start, col_start) = span.start_pos().line_col();
        let (line_end, col_end) = span.end_pos().line_col();

        Self {
            source_file: sf.clone(),
            line_start: line_start.try_into().unwrap(),
            col_start: col_start.try_into().unwrap(),
            line_end: line_end.try_into().unwrap(),
            col_end: col_end.try_into().unwrap(),
        }
    }
}
impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // if self.col_start == 0 || self.line_start == 0 || self.col_end == 0 || self.line_end == 0 {
        //     return write!(f, "?:?")
        // }
        write!(
            f,
            "{}:{}:{} - {}:{}",
            self.source_file.display(),
            self.line_start,
            self.col_start,
            self.line_end,
            self.col_start
        )
    }
}

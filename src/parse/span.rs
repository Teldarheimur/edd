use std::fmt::{self, Display};

use pest::Span as PestSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub line_start: u16,
    pub col_start: u16,
    pub line_end: u16,
    pub col_end: u16,
}

impl From<PestSpan<'_>> for Span {
    fn from(span: PestSpan) -> Self {
        let (line_start, col_start) = span.start_pos().line_col();
        let (line_end, col_end) = span.end_pos().line_col();

        Self {
            line_start: line_start.try_into().unwrap(),
            col_start: col_start.try_into().unwrap(),
            line_end: line_end.try_into().unwrap(),
            col_end: col_end.try_into().unwrap(),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // if self.col_start == 0 || self.line_start == 0 || self.col_end == 0 || self.line_end == 0 {
        //     return write!(f, "?:?")
        // }
        write!(f, "{}:{} - {}:{}", self.line_start, self.col_start, self.line_end, self.col_start)
    }
}

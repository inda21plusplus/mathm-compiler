mod combinators;
mod error;
mod parser;
#[cfg(test)]
mod tests;

use std::{cmp, fmt, ops::Range};

pub mod parsers;

pub use error::Error;
pub use parser::Parser;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
    // todo: in which file?
}

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
    pub fn first(input: &Input<'_>) -> Self {
        Self {
            start: input.location,
            end: input.location + input.s.chars().next().map(|ch| ch.len_utf8()).unwrap_or(1),
        }
    }
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    pub fn merge_optional(self, other: Option<Self>) -> Self {
        other.map_or(self, |other| self.merge(other))
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::new(0..0)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.start < other.start {
            cmp::Ordering::Less
        } else if self.start > other.start {
            cmp::Ordering::Greater
        } else if self.end < other.end {
            cmp::Ordering::Less
        } else if self.end > other.end {
            cmp::Ordering::Greater
        } else {
            cmp::Ordering::Equal
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Input<'s> {
    /// the location of the input, in bytes
    pub location: usize,
    pub s: &'s str,
}

impl<'s> Input<'s> {
    pub fn new(s: &'s str) -> Input<'s> {
        Self { location: 0, s }
    }
    pub fn next(&self) -> Option<(Self, char)> {
        self.s.chars().next().map(|c| {
            (
                Self {
                    location: self.location + c.len_utf8(),
                    s: &self.s[c.len_utf8()..],
                },
                c,
            )
        })
    }
}

impl<'s> std::ops::Index<Span> for Input<'s> {
    type Output = str;

    fn index(&self, i: Span) -> &Self::Output {
        &self.s[i.start - self.location..i.end - self.location]
    }
}

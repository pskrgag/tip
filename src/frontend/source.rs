#[derive(Debug, Clone, Copy)]
pub struct SourceLoc {
    pub start: u32,
    pub end: u32,
}

#[macro_export]
macro_rules! loc {
    ($l:expr, $r:expr) => { crate::frontend::source::SourceLoc::new($l as u32, $r as u32) }
}

impl SourceLoc {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

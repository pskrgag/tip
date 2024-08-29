use std::fs::File;
use std::io::read_to_string;

#[derive(Debug, Clone, Copy)]
pub struct SourceLoc {
    pub start: u32,
    pub end: u32,
}

#[macro_export]
macro_rules! loc {
    ($l:expr, $r:expr) => {
        crate::frontend::source::SourceLoc::new($l as u32, $r as u32)
    };
}

impl SourceLoc {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

static mut CURRENT_FILE: Option<SourceFile> = None;

pub fn get_current_source() -> &'static SourceFile {
    unsafe { &CURRENT_FILE.as_ref().unwrap() }
}

pub fn set_current_source(f: SourceFile) {
    unsafe { CURRENT_FILE = Some(f) }
}

pub struct SourceFile {
    name: String,
    data: String,
}

impl SourceFile {
    pub fn new<P: AsRef<std::path::Path>>(name: &P) -> std::io::Result<Self> {
        let file = File::open(name)?;
        let s = read_to_string(&file)?;

        Ok(Self {
            name: name.as_ref().as_os_str().to_str().unwrap().to_owned(),
            data: s,
        })
    }

    pub fn data<'a>(&'a self) -> &'a str {
        self.data.as_str()
    }

    pub fn loc_to_lex(&self, loc: SourceLoc) -> &str {
        &self.data[loc.start as usize..loc.end as usize]
    }

    pub fn loc_to_line(&self, loc: SourceLoc) -> (&str, SourceLoc, usize) {
        let prev = &self.data[..loc.start as usize];
        let next = &self.data[loc.end as usize..];

        let prevline = prev.rfind('\n').or(Some(0)).unwrap();
        let nextline = next.find('\n').or(Some(0)).unwrap();

        let prevline = if prevline == 0 {
            prevline
        } else {
            prevline + 1
        };

        let lines = prev.chars().filter(|x| *x == '\n').count();

        (
            &self.data[prevline..loc.end as usize + nextline],
            SourceLoc {
                start: loc.start - prevline as u32,
                end: loc.end - prevline as u32,
            },
            lines + 1
        )
    }

    pub fn name(&self) -> &String {
        &self.name
    }
}

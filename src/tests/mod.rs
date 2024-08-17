use crate::frontend::Ast;
use lalrpop_util::lalrpop_mod;
use regex::{Captures, Regex};
use std::fs;
use std::path::Path;

lalrpop_mod!(pub tip);

pub fn for_each_prog_parsed<P: AsRef<str>, F: Fn(&Captures, &mut Ast, &Path)>(
    relative_path: P,
    r: &Regex,
    f: F,
) {
    for entry in fs::read_dir(format!("./test_programs/{}", relative_path.as_ref())).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.is_dir() {
            continue;
        }

        let code = fs::read_to_string(&path).unwrap();
        let mut lines = code.lines();
        let first_line = lines.next().unwrap();

        if let Some(caps) = r.captures(first_line) {
            let mut ast = tip::TipParser::new().parse(code.as_str()).unwrap();
            f(&caps, &mut ast, &path);
        }
    }
}

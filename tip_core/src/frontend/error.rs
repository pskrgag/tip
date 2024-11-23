use super::source::*;
use colored::*;

#[macro_export]
macro_rules! report_undefined {
    ($e:expr, $node:tt, $name:expr) => {
        $crate::failable!($e, $node, "Use of undefined variable {:?}", $name)
    };
}

#[macro_export]
macro_rules! report_undefined_opt {
    ($e:expr, $node:tt, $name:expr) => {
        $crate::failable_opt!($e, $node, "Use of undefined variable {:?}", $name)
    };
}

#[macro_export]
macro_rules! report_uninitialized {
    ($e:expr, $node:tt) => {
        $crate::failable_opt!($e, $node, "Use of undefined variable")
    };
}

#[macro_export]
macro_rules! failable_opt {
    ($e:expr, $node:expr, $($arg:tt)*) => {
        if let Some(v) = $e {
            v
        } else {
            report_error($node.loc, format!($($arg)*), "");
            anyhow::bail!("")
        }
    };
}

#[macro_export]
macro_rules! failable {
    ($e:expr, $node:expr, $($arg:tt)*) => {
        if let Ok(v) = $e {
            v
        } else {
            report_error($node.loc, format!($($arg)*), "");
            anyhow::bail!("")
        }
    };
}

#[macro_export]
macro_rules! failable_match {
    ($val:expr, $expected:pat, $node:expr, $expected_tp:expr) => {
        if matches!($val, $expected) {
            true
        } else {
            report_error(
                $node.loc,
                format!("{} -- {:?}", $expected_tp, $val),
                "",
            );
            anyhow::bail!("")
        }
    };
}

#[macro_export]
macro_rules! bail_with_error {
    ($node:expr, $($arg:tt)*) => {
        report_error($node.loc, format!($($arg)*), "");
        anyhow::bail!("")
    };
}

const SOURCE_OFFSET: usize = 6;

pub fn report_error<S: AsRef<str>>(wh: SourceLoc, reason: S, meta: &str) {
    let mut res = String::new();

    let (line, pos, line_n) = get_current_source().loc_to_line(wh);

    res.push_str("Error:\n");

    res.push_str(
        format!(
            "{}:{line_n}:{} : {}: {meta}\n",
            get_current_source().name(),
            pos.start + 1,
            reason.as_ref().red(),
        )
        .as_str(),
    );
    res.push_str(format!("{} | {line}\n", " ".repeat(SOURCE_OFFSET)).as_str());

    res.push_str(
        format!(
            "{}{}{}",
            " ".repeat(pos.start as usize + SOURCE_OFFSET + 3),
            "-".repeat(pos.end as usize - pos.start as usize - 1).red(),
            "^".red()
        )
        .as_str(),
    );

    println!("{res}");
}

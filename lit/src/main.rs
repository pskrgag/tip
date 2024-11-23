use clap::Parser;
use colored::*;
use std::process::{Command, Stdio};
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Directory with tests
    dir: String,

    /// Path to TIP exe
    tip_path: String,

    /// Path to FileCheck
    file_check_path: String,
}

fn main() {
    let args = Args::parse();
    let mut succ = true;

    for i in WalkDir::new(args.dir) {
        let i = i.unwrap();

        if i.file_type().is_file() {
            let mut child = Command::new(&args.file_check_path)
                .arg(i.path())
                .arg(&args.tip_path)
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();

            let status = child.wait().unwrap().code().unwrap();
            let output = std::io::read_to_string(child.stdout.take().unwrap()).unwrap();

            if status == 254 {
                continue;
            }

            println!(
                "Test {:?}: {}{}",
                i.path(),
                if status == 0 {
                    "SUCCESS".green()
                } else {
                    "FAIL".red()
                },
                if status == 0 {
                    String::new()
                } else {
                    format!("\n{}", output)
                }
            );

            succ &= status == 0;
        }
    }

    std::process::exit(if succ { 0 } else { 1 });
}

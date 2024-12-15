use clap::Parser;
use regex::Regex;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::io::Result;
use std::process::{Command, Stdio};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source file
    prog: String,

    /// Path to TIP exe
    tip_path: String,
}

#[derive(Debug)]
enum RunSetup {
    Diagnostics(HashMap<usize, String>, String), // Maps line to error message + command line
    Interpret(i32),                              // Result code
    Skip,                                        // Don't do anything
}

#[derive(Debug)]
struct RunResult {
    pub output: String,
    pub error_code: i32,
}

// Runs tip and collects stdout
fn run_tip(tip: &String, prog: &String, args: &Vec<String>) -> Result<RunResult> {
    let mut cmd = Command::new(tip);
    let mut child = cmd.arg(prog).stdout(Stdio::piped());

    child = child.args(args);

    let mut child = child.spawn()?;
    let error_code = child.wait()?.code().unwrap();
    let output = std::io::read_to_string(child.stdout.take().unwrap())?;

    Ok(RunResult { error_code, output })
}

fn proccess_source(path: &String) -> Result<RunSetup> {
    let sourse = read_to_string(path)?;
    let error_regex =
        Regex::new(r".*\/\/ expected-error\{\{(.*)\}\}").expect("Failed to compile regex");
    let no_err = Regex::new(r".*\/\/ expect-no-errors").expect("Failed to compile regex");
    let interpret = Regex::new(r"// *TEST-INTERPRET: *(\d+)").unwrap();
    let skip = Regex::new(r"// *SKIP-FILE-CHECK").unwrap();
    let cmd = Regex::new(r"// *CMD: (.*)").unwrap();
    let mut errors = HashMap::new();
    let mut no_errs = false;
    let mut command_line = String::new();

    for (i, line) in sourse.split('\n').enumerate() {
        if i == 0 {
            if let Some(caps) = interpret.captures(line) {
                let result = caps[1].parse::<i32>().unwrap();

                return Ok(RunSetup::Interpret(result));
            } else if skip.captures(line).is_some() {
                return Ok(RunSetup::Skip);
            } else if let Some(caps) = cmd.captures(line) {
                command_line = caps[1].to_owned();
            }
        }

        if let Some(caps) = error_regex.captures(line) {
            errors.insert(i + 1, caps[1].to_owned().clone());
        } else if no_err.captures(line).is_some() {
            no_errs = true;
        }
    }

    if no_errs && errors.is_empty() {
        Ok(RunSetup::Diagnostics(errors, command_line))
    } else if errors.is_empty() {
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "No directives found. Add expect-no-errors",
        ))
    } else {
        Ok(RunSetup::Diagnostics(errors, command_line))
    }
}

fn proccess_diagnostics(
    output: String,
    mut errors: HashMap<usize, String>,
    source: String,
) -> bool {
    let regex = Regex::new(format!(r"^{source}:(\d+):(\d+) : (.*)").as_str())
        .expect("Failed to compile regex");
    let mut success = true;
    let mut seen = HashMap::new();

    for i in output.split('\n') {
        if let Some(caps) = regex.captures(i) {
            let line = caps[1].parse::<usize>().unwrap();
            let error = &caps[3];

            if let Some(expected_err) = errors.remove(&line) {
                let err_regex = format!("^{expected_err}");

                // Stupid way to add escapes before ( and )
                let err_regex = err_regex.chars().fold(String::new(), |acc, c| {
                    if c == ')' || c == '(' {
                        format!("{acc}\\{c}")
                    } else {
                        format!("{acc}{c}")
                    }
                });
                let err_regex = Regex::new(err_regex.as_str()).unwrap();

                if err_regex.captures(error).is_none() {
                    eprintln!("Unexpected error:\n'{error}', expected\n'{expected_err}'");
                    success = false;
                }

                seen.insert(line, error.to_owned());
            } else {
                // TODO: This is a hack for diagnostics printed more than once.
                // Maybe one day i will fix it.
                if let Some(seen) = seen.get(&line) {
                    if seen != error {
                        eprintln!("Unspecified error '{error}' on line {line}");
                        success = false;
                    }
                }
            }
        }
    }

    for (line, error) in &errors {
        eprintln!("Unseen error {error} on line {line}");
    }

    success && errors.len() == 0
}

fn proccess_iterpret(expected_code: i32, code: i32) -> bool {
    let res = expected_code == code;

    if !res {
        eprintln!("Expected exit code {expected_code}, but got {code}");
    }

    res
}

fn run_test(result: RunResult, setup: RunSetup, source: String) -> bool {
    match setup {
        RunSetup::Diagnostics(diag, _) => proccess_diagnostics(result.output, diag, source),
        RunSetup::Interpret(x) => proccess_iterpret(x, result.error_code),
        _ => panic!("Should not happen"),
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    let run_setup = proccess_source(&args.prog)?;
    let extra_args = match run_setup {
        RunSetup::Diagnostics(_, ref args) => {
            if !args.is_empty() {
                args.split(' ').map(|x| x.to_owned()).collect()
            } else {
                vec![]
            }
        }
        RunSetup::Interpret(_) => vec!["-i".to_owned()],
        RunSetup::Skip => std::process::exit(-2),
    };

    let out = run_tip(&args.tip_path, &args.prog, &extra_args)?;
    if run_test(out, run_setup, args.prog) {
        Ok(())
    } else {
        Err(std::io::Error::new(std::io::ErrorKind::NotFound, ""))
    }
}

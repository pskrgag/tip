#![allow(clippy::vec_box)]

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::Result;

lalrpop_mod!(pub tip);

mod frontend;
mod interpreter;
mod analisys;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    prog: String,

    #[arg(short, long)]
    dump_ast: bool,

    #[arg(short, long)]
    interpret: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let code_str = read_to_string(args.prog)?;
    let ast = tip::TipParser::new().parse(code_str.as_str()).unwrap();

    if args.dump_ast {
        let printer = frontend::ast_printer::AstPriner::new(&ast);
        printer.dump(&mut std::io::stdout());
    }

    let res = if args.interpret {
        let i = interpreter::Interpreter::new(&ast);
        i.run()
    } else {
        0
    };

    std::process::exit(res as i32)
}

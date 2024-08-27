#![allow(clippy::vec_box)]
#![feature(box_into_inner)]

use clap::Parser;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::Result;

lalrpop_mod!(pub tip);

mod analisys;

#[macro_use]
mod frontend;
mod interpreter;
mod solvers;

#[cfg(test)]
mod tests;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source file
    prog: String,

    /// Dumps AST of a program
    #[arg(short, long)]
    dump_ast: bool,

    /// Interprets progra
    #[arg(short, long)]
    interpret: bool,

    /// Dumps cfg of a function into "out.dot"
    #[arg(long, value_name = "FUNCTION NAME")]
    dump_cfg: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let code_str = read_to_string(args.prog)?;
    let mut ast = tip::TipParser::new().parse(code_str.as_str()).unwrap();

    let res = analisys::analyze_ast(&mut ast);
    if res.is_err() {
        println!("Cannot proccess futher because of previous error");
        std::process::exit(-1)
    }

    if args.dump_ast {
        let printer = frontend::ast_printer::AstPriner::new(&ast);
        printer.dump(&mut std::io::stdout());
    }

    if let Some(name) = args.dump_cfg {
        let f = ast.function(name).unwrap();
        let cfg = frontend::cfg::Cfg::new(ast.function_by_index(f).unwrap());

        std::fs::write("out.dot", format!("{:?}", cfg))?;
    }

    let res = if args.interpret {
        let i = interpreter::Interpreter::new(&ast);
        i.run()
    } else {
        0
    };

    std::process::exit(res as i32)
}

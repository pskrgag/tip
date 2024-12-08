#![allow(clippy::vec_box)]

use anyhow::Result;
use clap::Parser;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub tip);

mod analisys;

#[macro_use]
mod frontend;
mod interpreter;
mod solvers;

use frontend::source::*;

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

    /// Skip analisys pass
    #[arg(short, long)]
    skip_analisys: bool,
}

fn main() {
    // The way to prevent anyhow from writing shit to console
    if run().is_err() {
        std::process::exit(-1_i32)
    }
}

fn run() -> Result<()> {
    let args = Args::parse();

    set_current_source(SourceFile::new(&args.prog)?);
    let mut ast = tip::TipParser::new()
        .parse(get_current_source().data())
        .unwrap();

    if !args.skip_analisys {
        let res = analisys::analyze_ast(&mut ast);
        if res.is_err() {
            println!("Cannot proccess futher because of previous error");
            std::process::exit(-1)
        }
    }

    if args.dump_ast {
        let printer = frontend::ast_printer::AstPriner::new(&ast);
        printer.dump(&mut std::io::stdout());
    }

    if let Some(name) = args.dump_cfg {
        let f = ast.function(name).unwrap();
        let cfg = frontend::cfg::Cfg::new(ast.function_by_index(f).unwrap());

        // println!("{:?}", cfg);
        std::fs::write("out.dot", format!("{}", cfg))?;
    }

    let res = if args.interpret {
        let i = interpreter::Interpreter::new(&ast);
        i.run()?
    } else {
        0
    };

    std::process::exit(res as i32)
}

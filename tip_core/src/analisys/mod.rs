use crate::frontend::*;
use anyhow::Result;

pub mod typing;
pub mod dataflow;
pub mod map;

pub trait AstAnalisys {
    fn run(&mut self, f: &mut Ast) -> Result<()>;
}

pub fn analyze_ast(ast: &mut Ast) -> Result<()> {
    let passes: Vec<Box<dyn AstAnalisys>> = vec![Box::new(typing::TypeAnalysis::new())];

    for mut i in passes {
        i.run(ast)?;
    }

    Ok(())
}

pub fn per_fn_dataflow(ast: &mut Ast) -> Result<()> {
    for i in ast.functions_mut() {
        dataflow::dataflow(i)?;
    }

    Ok(())
}

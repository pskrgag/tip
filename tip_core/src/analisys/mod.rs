use crate::frontend::*;
use anyhow::Result;

pub mod ast_visitor;
pub mod dataflow_analysis;
pub mod typing;

pub trait AstAnalisys {
    fn run(&mut self, f: &mut Ast) -> Result<()>;
}

pub fn analyze_ast(ast: &mut Ast) -> Result<()> {
    let passes: Vec<Box<dyn AstAnalisys>> = vec![
        Box::new(typing::TypeAnalysis::new()),
        Box::new(dataflow_analysis::dataflow::DataFlowWrapper::<
            dataflow_analysis::uninit::UninitAnalisys,
        >::default()),
    ];

    for mut i in passes {
        i.run(ast)?;
    }

    Ok(())
}

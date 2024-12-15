use crate::frontend::*;
use anyhow::Result;

pub mod ast_visitor;
pub mod dataflow_analysis;
pub mod typing;

pub trait AstAnalisys {
    fn run(&mut self, f: &mut Ast) -> Result<()>;
    fn name(&self) -> &'static str;
}

pub fn analyze_ast(ast: &mut Ast, to_run: Option<Vec<String>>) -> Result<()> {
    let passes: Vec<Box<dyn AstAnalisys>> = vec![
        Box::new(typing::TypeAnalysis::new()),
        Box::new(dataflow_analysis::dataflow::DataFlowWrapper::<
            dataflow_analysis::uninit::UninitAnalisys,
        >::default()),
        Box::new(dataflow_analysis::dataflow::DataFlowWrapper::<
            dataflow_analysis::unused::UnusedVars,
        >::default()),
    ];

    for mut i in passes {
        if let Some(ref to_run) = to_run {
            if to_run.iter().any(|x| *x == i.name()) {
                i.run(ast)?;
            }
        } else {
            i.run(ast)?;
        }
    }

    Ok(())
}

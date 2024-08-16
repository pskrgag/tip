use crate::frontend::*;

pub mod typing;

pub type AnalisysError<'ast> = (&'ast Statement, String);
pub type AnalisysResult<'ast> = Result<(), ()>;

pub trait AstAnalisys {
    fn run(&mut self) -> AnalisysResult;
}

pub fn analyze_ast<'ast>(ast: &'ast Ast) -> AnalisysResult {
    let passes: Vec<Box<dyn AstAnalisys>> = vec![Box::new(typing::TypeAnalysis::new(ast))];

    for mut i in passes {
        i.run()?;
    }

    Ok(())
}

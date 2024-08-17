use crate::frontend::*;

pub mod typing;

pub type AnalisysError<'ast> = (&'ast Statement, String);
pub type AnalisysResult<'ast> = Result<(), ()>;

pub trait AstAnalisys {
    fn run(&mut self, f: &mut Ast) -> AnalisysResult;
}

pub fn analyze_ast(ast: &mut Ast) -> AnalisysResult {
    let passes: Vec<Box<dyn AstAnalisys>> = vec![Box::new(typing::TypeAnalysis::new())];

    for mut i in passes {
        i.run(ast)?;
    }

    Ok(())
}

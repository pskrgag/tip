use super::bitvec::NamedBitvec;
use super::dataflow::{DataFlow, FlowDirection};
use crate::analisys::ast_visitor::AstVisitor;
use crate::frontend::error::report_error;
use crate::frontend::*;

#[derive(Default)]
pub struct UninitAnalisys;

impl DataFlow for UninitAnalisys {
    type Domain = NamedBitvec;

    const NAME: &'static str = "uninit_vars";
    const DIRECTION: FlowDirection = FlowDirection::Forward;

    fn init(&mut self, f: &Function) -> Self::Domain {
        let mut locals = f
            .locals()
            .iter()
            .flatten()
            .map(|x| x.id().clone())
            .collect::<Vec<_>>();

        if let Some(params) = f.params() {
            locals.extend(params.iter().map(|x| x.id().clone()));
        }

        let mut vec = NamedBitvec::new(locals.as_slice());

        if let Some(params) = f.params() {
            for i in params {
                vec.set(i.id());
            }
        }

        vec
    }
}

impl AstVisitor for UninitAnalisys {
    type Context = <Self as DataFlow>::Domain;

    fn visit_identifier(&self, expr: &Expression, ctx: Self::Context) -> Self::Context {
        let id = expr.kind.as_indentifier().unwrap();
        let init = ctx.get(id);

        if let Some(init) = init {
            if !init {
                crate::report_error!(expr, "Use of unitialized variable {:?}", id);
            }
        }

        ctx
    }

    fn visit_assign(&self, st: &Assign, mut d: Self::Context) -> Self::Context {
        match &st.lhs.kind {
            ExpressionKind::Indentifier(id) => {
                d.set(id);
                d
            }
            _ => d,
        }
    }
}

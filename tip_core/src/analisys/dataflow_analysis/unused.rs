use super::bitvec::NamedBitvec;
use super::dataflow::{DataFlow, FlowDirection};
use crate::analisys::ast_visitor::AstVisitor;
use crate::frontend::error::report_error;
use crate::frontend::*;
use std::collections::HashMap;

enum Kind {
    Var,
    Pointer,
}

#[derive(Default)]
pub struct UnusedVars {
    depended_vars: HashMap<Indentifier, Kind>,
}

struct DependantMarker;

impl UnusedVars {
    fn init_domain(f: &Function) -> <Self as DataFlow>::Domain {
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

impl DataFlow for UnusedVars {
    type Domain = NamedBitvec;

    const NAME: &'static str = "unused_expr";
    const DIRECTION: FlowDirection = FlowDirection::Backward;

    fn init(&mut self, f: &Function) -> Self::Domain {
        let dom = Self::init_domain(f);

        if let Some(params) = f.params() {
            for param in params {
                if param.r#type().is_pointer() {
                    self.depended_vars.insert(param.id().clone(), Kind::Pointer);
                }
            }
        }

        dom
    }
}

impl AstVisitor for UnusedVars {
    type Context = <Self as DataFlow>::Domain;

    fn visit_assign(&self, assign: &Assign, ctx: Self::Context) -> Self::Context {
        match &assign.lhs.kind {
            ExpressionKind::Indentifier(id) => {
                if let Some(set) = ctx.get(id) {
                    // Assigned variable is dependant
                    if set {
                        DependantMarker {}.visit_expression(&assign.rhs, ctx)
                    } else {
                        crate::report_error!(assign.lhs, "Expression has no effect");
                        ctx
                    }
                } else {
                    crate::report_error!(assign.lhs, "Expression has no effect");
                    ctx
                }
            }
            _ => ctx,
        }
    }

    fn visit_return(&self, exp: &Expression, ctx: Self::Context) -> Self::Context {
        // Mark all variables as dependant
        DependantMarker {}.visit_expression(exp, ctx)
    }
}

impl AstVisitor for DependantMarker {
    type Context = <UnusedVars as DataFlow>::Domain;

    fn visit_identifier(&self, expr: &Expression, mut ctx: Self::Context) -> Self::Context {
        let id = expr.kind.as_indentifier().unwrap();

        // Guard against assignemt of global vars (like functions)
        if ctx.get(id).is_some() {
            ctx.set(id);
        }

        ctx
    }
}

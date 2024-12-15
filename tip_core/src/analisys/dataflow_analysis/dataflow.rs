use super::lattice::*;
use crate::analisys::ast_visitor::AstVisitor;
use crate::analisys::AstAnalisys;
use crate::frontend::cfg::Cfg;
use crate::frontend::*;
use crate::frontend::{Function, Statement};
use anyhow::Result;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum FlowDirection {
    Forward,
    Backward,
}

pub trait DataFlow: AstVisitor<Context = Self::Domain> {
    type Domain: Lattice + Clone + PartialEq + Debug;

    const NAME: &'static str;
    const DIRECTION: FlowDirection;

    fn init(&mut self, f: &Function) -> Self::Domain;

    fn flow(&self, st: &Statement, d: Self::Domain) -> Self::Domain {
        self.visit_statement(st, d)
    }
}

#[derive(Default)]
pub struct DataFlowWrapper<T: DataFlow + Default>(T);

impl<T: DataFlow + Default> AstAnalisys for DataFlowWrapper<T> {
    fn name(&self) -> &'static str {
        T::NAME
    }

    fn run(&mut self, ast: &mut Ast) -> Result<()> {
        for i in ast.functions() {
            run_function_dataflow(i, &mut self.0)
        }

        Ok(())
    }
}

pub fn run_function_dataflow<T: DataFlow>(f: &Function, analysis: &mut T) {
    let mut domain = analysis.init(f);
    let cfg = Cfg::new(f);
    let mut wq = VecDeque::new();
    let mut map = HashMap::new();

    if T::DIRECTION == FlowDirection::Forward {
        wq.push_front(cfg.start());

        while let Some(node_handle) = wq.pop_front() {
            let mut changed = true;
            let node = cfg.node(node_handle);

            for i in node.predecessors() {
                if let Some(d) = map.get(i) {
                    domain = domain.meet(d);
                }
            }

            for stmt in node.stmts() {
                domain = analysis.flow(stmt, domain);
            }

            if let Some(old) = map.get(&node_handle) {
                if *old == domain {
                    changed = false;
                }
            }

            if changed {
                map.insert(node_handle, domain.clone());
                for i in node.successors() {
                    wq.push_back(*i);
                }
            }
        }
    } else if T::DIRECTION == FlowDirection::Backward {
        wq.push_front(cfg.last());

        while let Some(node_handle) = wq.pop_front() {
            let mut changed = true;
            let node = cfg.node(node_handle);

            for i in node.successors() {
                if let Some(d) = map.get(i) {
                    domain = domain.join(d);
                }
            }

            for stmt in node.stmts().iter().rev() {
                domain = analysis.flow(stmt, domain);
            }

            if let Some(old) = map.get(&node_handle) {
                if *old == domain {
                    changed = false;
                }
            }

            if changed {
                map.insert(node_handle, domain.clone());
                for i in node.predecessors() {
                    wq.push_back(*i);
                }
            }
        }
    }
}

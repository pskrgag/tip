use super::lattice::{Join, Lattice, Meet};
use crate::analisys::dataflow::*;
use crate::frontend::cfg::{Cfg, CfgNode, CfgNodeHandle};
use crate::frontend::{Indentifier, StatementKind};
use anyhow::Result;
use std::collections::HashMap;
use std::collections::VecDeque;

pub struct FPSolver<'ast, A: DataFlowAnalisys> {
    cfg: &'ast Cfg<'ast>,
    solver: A,
    map: HashMap<CfgNodeHandle, A::Domain>,
}

impl<'ast, A: DataFlowAnalisys> FPSolver<'ast, A> {
    pub fn new(cfg: &'ast Cfg, solver: A) -> Self {
        Self {
            cfg,
            solver,
            map: HashMap::new(),
        }
    }

    pub fn proccess_bb(&mut self, bb: &CfgNode, state: &mut A::Domain) {
        let stmt = bb.stmts();

        for e in stmt {
            if matches!(e.kind, StatementKind::If(_)) {
            } else {
                self.solver.proccess_statement(state, e);
            }
        }
    }

    pub fn analyze(mut self) -> Result<()>
    where
        <A as crate::analisys::dataflow::AnalisysDomain>::Domain: std::fmt::Debug,
    {
        let mut wq = VecDeque::new();
        let mut vis = vec![false; self.cfg.size()];
        let mut state = A::Domain::bottom();

        wq.push_back(self.cfg.start());

        while let Some(handle) = wq.pop_front() {
            if vis[handle] == true {
                continue;
            }

            let current = self.cfg.node(handle);
            let succ = current.successors();
            let pred = current.predecessors();

            for i in pred {
                state.join(self.map.get(i).unwrap());
            }

            self.proccess_bb(current, &mut state);
            self.map.insert(handle, state.clone());

            for i in succ {
                wq.push_back(*i);
            }

            vis[handle] = true;
        }

        state.dump();
        Ok(())
    }
}

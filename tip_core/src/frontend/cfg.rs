use super::*;
use std::fmt::{Debug, Display, Formatter, Result};

pub type CfgNodeHandle = usize;
const INVALID_HANDLE: usize = CfgNodeHandle::MAX;

#[derive(Debug)]
pub struct CfgNode<'ast> {
    stmt: Vec<&'ast Statement>,

    pred: Vec<CfgNodeHandle>,
    succ: [CfgNodeHandle; 2],
}

impl<'ast> CfgNode<'ast> {
    pub fn new() -> Self {
        Self {
            stmt: Vec::new(),
            pred: Vec::with_capacity(3), // Expecting no more than 3
            succ: [INVALID_HANDLE; 2],
        }
    }

    pub fn push(&mut self, stmt: &'ast Statement) {
        self.stmt.push(stmt);
    }

    pub fn push_succ(&mut self, handle: CfgNodeHandle) {
        if self.succ[0] != INVALID_HANDLE {
            assert!(self.succ[1] == INVALID_HANDLE);

            self.succ[1] = handle;
        } else {
            self.succ[0] = handle;
        }
    }

    pub fn push_pred(&mut self, handle: CfgNodeHandle) {
        self.pred.push(handle);
    }

    pub fn predecessors(&self) -> &[CfgNodeHandle] {
        self.pred.as_slice()
    }

    pub fn successors(&self) -> &[CfgNodeHandle] {
        if self.succ[0] != INVALID_HANDLE {
            if self.succ[1] != INVALID_HANDLE {
                &self.succ
            } else {
                &self.succ[..1]
            }
        } else {
            &[]
        }
    }

    pub fn stmts(&self) -> &[&Statement] {
        self.stmt.as_slice()
    }
}

#[derive(Debug)]
pub struct Cfg<'ast> {
    nodes: Vec<CfgNode<'ast>>,
    f: &'ast Function,
}

impl<'ast> Cfg<'ast> {
    pub fn new(ast: &'ast Function) -> Self {
        let mut ret = Self {
            nodes: Vec::new(),
            f: ast,
        };

        ret.build();
        ret
    }

    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    pub fn start(&self) -> CfgNodeHandle {
        0
    }

    pub fn node(&self, h: CfgNodeHandle) -> &CfgNode {
        &self.nodes[h]
    }

    fn new_bb(&mut self) -> CfgNodeHandle {
        self.nodes.push(CfgNode::new());
        self.nodes.len() - 1
    }

    fn current_bb(&self) -> CfgNodeHandle {
        self.nodes.len() - 1
    }

    fn push_to_last_bb(&mut self, stmt: &'ast Statement) {
        self.nodes.last_mut().unwrap().push(stmt);
    }

    fn handle_if(&mut self, iff: &'ast If) {
        let mut else_handle = None;
        let current_handle = self.current_bb();

        // Handle then block
        let then_handle = self.new_bb();
        self.proccess_statement(iff.then.as_ref());

        self.nodes[then_handle].push_pred(current_handle);
        self.nodes[current_handle].push_succ(then_handle);
        let next = self.new_bb();

        // Handle else block
        if let Some(elsee) = iff.elsee.as_ref() {
            else_handle = Some(self.new_bb());
            self.proccess_statement(elsee);

            self.nodes[else_handle.unwrap()].push_pred(current_handle);
            self.nodes[current_handle].push_succ(else_handle.unwrap());
        } else {
            self.nodes[next].push_pred(current_handle);
            self.nodes[current_handle].push_succ(next);
        }

        // Figure out how to be if bb after if does not exists
        self.nodes[next].push_pred(then_handle);
        self.nodes[then_handle].push_succ(next);

        if let Some(else_handle) = else_handle {
            self.nodes[next].push_pred(else_handle);
            self.nodes[else_handle].push_succ(next);
        }
    }

    fn handle_while(&mut self, body: &'ast Statement) {
        let while_bb = self.current_bb();
        let b = self.new_bb();

        self.nodes[b].push_pred(while_bb);
        self.nodes[b].push_succ(while_bb);

        self.nodes[while_bb].push_succ(b);
        self.nodes[while_bb].push_pred(b);

        self.proccess_statement(body);

        let next = self.new_bb();

        self.nodes[next].push_pred(while_bb);
        self.nodes[next].push_pred(b);

        self.nodes[while_bb].push_succ(next);
    }

    fn proccess_statement(&mut self, f: &'ast Statement) {
        match &f.kind {
            StatementKind::Compound(x) => {
                for i in x {
                    self.proccess_statement(i);
                }
            }
            StatementKind::Assign(_)
            | StatementKind::Output(_)
            | StatementKind::Return(_)
            | StatementKind::Expression(_) => {
                self.push_to_last_bb(f);
            }
            StatementKind::If(iff) => {
                self.push_to_last_bb(f);
                self.handle_if(iff);
            }
            StatementKind::While(wl) => {
                self.push_to_last_bb(f);
                self.handle_while(&wl.body);
            }
            StatementKind::Function(_) => panic!("There should not be function inside function"),
        }
    }

    fn build(&mut self) {
        if let Some(body) = self.f.body() {
            self.new_bb();
            self.proccess_statement(body);
        }
    }
}

impl Display for CfgNode<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for i in &self.stmt {
            write!(f, "{:?}\\n", i)?;
        }

        Ok(())
    }
}

impl Display for Cfg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "digraph Cfg {{")?;

        for (i, node) in self.nodes.iter().enumerate() {
            writeln!(f, "{i} [label=\"",)?;

            for i in &node.stmt {
                writeln!(f, "{}", i)?;
            }

            writeln!(f, "\"]\n")?;
        }

        for (i, node) in self.nodes.iter().enumerate() {
            let succ = node.successors();

            if succ.len() == 1 {
                writeln!(f, "{i} -> {}\n", succ[0])?;
            } else if succ.len() == 2 {
                writeln!(f, "{i} -> {} [label=\"true\"] \n", succ[0])?;
                writeln!(f, "{i} -> {} [label=\"false\"]\n", succ[1])?;
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

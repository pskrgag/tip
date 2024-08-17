use super::*;
use std::fmt::{Debug, Formatter, Result};

pub type CfgNodeHandle = usize;
const INVALID_HANDLE: usize = CfgNodeHandle::MAX;

struct CfgNode<'ast> {
    stmt: Vec<&'ast Statement>,
    pred: [CfgNodeHandle; 2],
    succ: [CfgNodeHandle; 2],
}

impl<'ast> CfgNode<'ast> {
    pub fn new() -> Self {
        Self {
            stmt: Vec::new(),
            pred: [INVALID_HANDLE; 2],
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
        if self.pred[0] != INVALID_HANDLE {
            assert!(self.pred[1] == INVALID_HANDLE);

            self.pred[1] = handle;
        } else {
            self.pred[0] = handle;
        }
    }

    pub fn successors(&self) -> &[CfgNodeHandle] {
        &self.succ
    }
}

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

    fn new_bb(&mut self) -> CfgNodeHandle {
        self.nodes.push(CfgNode::new());
        self.nodes.len() - 1
    }

    fn current_bb(&mut self) -> CfgNodeHandle {
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

        // Handle else block
        if let Some(elsee) = iff.elsee.as_ref() {
            else_handle = Some(self.new_bb());
            self.proccess_statement(elsee);

            self.nodes[else_handle.unwrap()].push_pred(current_handle);
            self.nodes[current_handle].push_succ(else_handle.unwrap());
        }

        // Update next bb
        //
        // NOTE: it's known to exist, since all functions must have a "return" Statement.
        let next = self.new_bb();

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
        self.nodes[b].push_succ(next);
    }

    fn proccess_statement(&mut self, f: &'ast Statement) {
        match f {
            Statement::Compound(x) => {
                for i in x {
                    self.proccess_statement(i);
                }
            }
            Statement::Assign(_) | Statement::Output(_) | Statement::Return(_) => {
                self.push_to_last_bb(f);
            }
            Statement::If(iff) => {
                self.push_to_last_bb(f);
                self.handle_if(iff);
            }
            Statement::While(wl) => {
                self.push_to_last_bb(f);
                self.handle_while(&wl.body);
            }
            _ => panic!("Unexpected stmt {:?}", f),
        }
    }

    fn build(&mut self) {
        if let Some(body) = self.f.body() {
            self.new_bb();
            self.proccess_statement(body);
        }

        self.proccess_statement(self.f.ret_e());
    }
}

impl<'ast> Debug for CfgNode<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for i in &self.stmt {
            write!(f, "{:?}\\n", i)?;
        }

        Ok(())
    }
}

impl<'ast> Debug for Cfg<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "digraph Cfg {{")?;

        for (i, node) in self.nodes.iter().enumerate() {
            writeln!(f, "{i} [label=\"{:?}\"]\n", node)?;
        }

        for (i, node) in self.nodes.iter().enumerate() {
            for s in node.successors() {
                if *s != INVALID_HANDLE {
                    writeln!(f, "{i} -> {s}\n")?;
                }
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

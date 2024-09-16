use super::map::*;
use crate::frontend::cfg::Cfg;
use crate::frontend::*;
use crate::frontend::{Function, Statement, StatementKind};
use crate::solvers::fixedpoint::FPSolver;
use crate::solvers::lattice::*;
use anyhow::Result;
use std::collections::HashMap;

pub mod zero;

// ==== Generic dataflow ===
pub enum AnalisysDirection {
    Backward,
    Forward,
}

pub trait DomainDump {
    fn dump(&self);
}

pub trait AnalisysDomain {
    type Domain: Lattice + Clone + DomainDump;

    const NAME: &'static str;
    const DIRECTION: AnalisysDirection = AnalisysDirection::Forward;
}

pub trait DataFlowAnalisys: AnalisysDomain {
    fn proccess_statement(&mut self, state: &mut Self::Domain, s: &Statement);
}

// === Value analisys ===

pub trait ValueAnalisys {
    type Value: Lattice + Clone + Eq + std::fmt::Debug;
    const NAME: &'static str;

    fn env(&mut self) -> &mut Enviroment<Self::Value>;

    fn proccess_rvalue(&mut self, val: i64) -> Self::Value {
        Self::Value::top()
    }

    fn proccess_lhs(&mut self, e: &Expression) -> Self::Value {
        match &e.kind {
            ExpressionKind::Indentifier(x) => {
                let env = self.env();
                let loc = env.get_loc(x, Self::Value::bottom());

                match env.val(loc) {
                    ValueOrLoc::Value(x) => x,
                    _ => panic!(""),
                }
            }
            ExpressionKind::Number(x) => self.proccess_rvalue(*x),
            ExpressionKind::Call(x) => Self::Value::top(),
            _ => panic!("{e}"),
        }
    }

    fn proccess_rhs(&mut self, e: &Expression) -> Location {
        match &e.kind {
            ExpressionKind::Indentifier(x) => {
                let env = self.env();
                env.get_loc(x, Self::Value::bottom())
            }
            ExpressionKind::Unary(x) => match x.as_ref() {
                Unary::Deref(x) => self.proccess_rhs(x),
                _ => panic!(""),
            },
            _ => panic!("{:?}", e),
        }
    }
}

#[derive(Default)]
pub struct ValueAnalisysWrapper<T>(pub T);

impl<T: ValueAnalisys> ValueAnalisysWrapper<T> {
    pub fn change_state(
        state: &mut <ValueAnalisysWrapper<T> as AnalisysDomain>::Domain,
        var: Indentifier,
        val: T::Value,
    ) {
        state
            .entry(var)
            .and_modify(|x| *x = val.clone())
            .or_insert(val);
    }
}

impl<T: ValueAnalisys> AnalisysDomain for ValueAnalisysWrapper<T> {
    type Domain = HashMap<Indentifier, T::Value>;
    const NAME: &'static str = T::NAME;
}

impl<T: ValueAnalisys> DataFlowAnalisys for ValueAnalisysWrapper<T> {
    fn proccess_statement(&mut self, state: &mut Self::Domain, s: &Statement) {
        match &s.kind {
            StatementKind::Assign(assign) => {
                let loc = self.0.proccess_rhs(assign.lhs.as_ref());
                let val = self.0.proccess_lhs(assign.rhs.as_ref());

                self.0.env().assign_loc(loc, ValueOrLoc::Value(val.clone()));
                Self::change_state(state, self.0.env().loc_val(loc), val);
            }
            _ => {}
        }
    }
}

impl<I: std::hash::Hash + Eq + Clone + std::fmt::Debug, T: Lattice + std::fmt::Debug> DomainDump for HashMap<I, T> {
    fn dump(&self) {
        for (id, val) in self {
            println!("{:?}: {:?}", id, val);
        }
    }
}

pub fn dataflow(func: &mut Function) -> Result<()> {
    let cfg = Cfg::new(func);
    let mut f = ValueAnalisysWrapper(zero::ZeroAnalisys::new());
    let fp = FPSolver::new(&cfg, f);

    fp.analyze()
}

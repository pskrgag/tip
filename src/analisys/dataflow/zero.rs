use super::*;
use crate::analisys::map::Enviroment;
use crate::frontend::Indentifier;
use crate::solvers::lattice::*;
use std::collections::HashMap;

#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub enum SignAnalisysDomain {
    Positive,
    Zero,
    Negative,
}

pub struct ZeroAnalisys {
    env: Enviroment<FlatSet<SignAnalisysDomain>>,
}

impl ZeroAnalisys {
    pub fn new() -> Self {
        Self {
            env: Enviroment::new(),
        }
    }
}

impl ValueAnalisys for ZeroAnalisys {
    type Value = FlatSet<SignAnalisysDomain>;

    const NAME: &'static str = "zero";

    fn env(&mut self) -> &mut Enviroment<Self::Value> {
        &mut self.env
    }

    fn proccess_rvalue(&mut self, val: i64) -> Self::Value {
        if val == 0 {
            FlatSet::Element(SignAnalisysDomain::Zero)
        } else if val < 0 {
            FlatSet::Element(SignAnalisysDomain::Negative)
        } else {
            FlatSet::Element(SignAnalisysDomain::Positive)
        }
    }
}

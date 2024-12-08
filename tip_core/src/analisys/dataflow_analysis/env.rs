use super::lattice::*;
use crate::frontend::Indentifier;
use std::collections::HashMap;

pub struct Env<T: Clone> {
    map: HashMap<Indentifier, T>,
}

impl<T: Clone> Env<T> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
}

impl<T: Clone + MeetSemilattice> MeetSemilattice for Env<T> {
    fn meet(&self, rhs: &Self) -> Self {
        Self {
            map: self
                .map
                .clone()
                .into_iter()
                .filter(|x| rhs.map.get(&x.0).is_some())
                .map(|(key, val)| {
                    let new_val = val.meet(rhs.map.get(&key).unwrap());
                    (key, new_val)
                })
                .collect(),
        }
    }
}

impl<T: Clone + JoinSemilattice> JoinSemilattice for Env<T> {
    fn join(&self, _rhs: &Self) -> Self {
        todo!()
    }
}

use super::lattice::*;
use crate::frontend::Indentifier;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Bitvec {
    data: Vec<u64>,
    bits: usize,
}

impl Bitvec {
    pub fn new(bits: usize) -> Self {
        Self {
            bits,
            data: vec![0; ((bits + 7) & !7usize) >> 3],
        }
    }

    pub fn set(&mut self, bit: usize) {
        assert!(self.bits > bit);

        self.data[bit >> 3] |= 1 << (bit & 7);
    }

    pub fn get(&self, bit: usize) -> bool {
        assert!(self.bits > bit);

        (self.data[bit >> 3] & 1 << (bit & 7)) != 0
    }
}

impl JoinSemilattice for Bitvec {
    fn join(&self, rhs: &Self) -> Self {
        // Actually this may not be the case, but for simplicity...
        assert!(self.bits == rhs.bits);

        Self {
            bits: self.bits,
            data: std::iter::zip(&self.data, &rhs.data)
                .map(|(x1, x2)| x1 | x2)
                .collect(),
        }
    }
}

impl MeetSemilattice for Bitvec {
    fn meet(&self, rhs: &Self) -> Self {
        // Actually this may not be the case, but for simplicity...
        assert!(self.bits == rhs.bits);

        Self {
            bits: self.bits,
            data: std::iter::zip(&self.data, &rhs.data)
                .map(|(x1, x2)| x1 & x2)
                .collect(),
        }
    }
}

impl Lattice for Bitvec {}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct NamedBitvec {
    vec: Bitvec,
    map: HashMap<Indentifier, usize>,
}

impl NamedBitvec {
    pub fn new(ids: &[Indentifier]) -> Self {
        let mut counter = 0;
        let map = ids
            .iter()
            .map(|x| {
                let res = (x.clone(), counter);
                counter += 1;
                res
            })
            .collect();

        Self {
            vec: Bitvec::new(counter),
            map,
        }
    }

    pub fn set(&mut self, id: &Indentifier) {
        self.vec.set(*self.map.get(id).unwrap());
    }

    pub fn get(&self, id: &Indentifier) -> Option<bool> {
        Some(self.vec.get(*self.map.get(id)?))
    }
}

impl JoinSemilattice for NamedBitvec {
    fn join(&self, rhs: &Self) -> Self {
        assert!(self.map == rhs.map);

        Self {
            map: self.map.clone(),
            vec: self.vec.join(&rhs.vec),
        }
    }
}

impl MeetSemilattice for NamedBitvec {
    fn meet(&self, rhs: &Self) -> Self {
        assert!(self.map == rhs.map);

        Self {
            map: self.map.clone(),
            vec: self.vec.meet(&rhs.vec),
        }
    }
}

impl Lattice for NamedBitvec {}

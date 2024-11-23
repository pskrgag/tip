use std::collections::HashMap;
use std::hash::Hash;

// Least upper bound
pub trait Join {
    fn join(&mut self, other: &Self);
}

// Greates lower bound
pub trait Meet {
    fn meet(&mut self, other: &Self);
}

pub trait Lattice: Join + Meet {
    fn top() -> Self;
    fn bottom() -> Self;
}

#[derive(Clone, Default, Eq, PartialEq, Debug)]
pub enum FlatSet<T: PartialEq + Eq + Clone> {
    Top,
    Element(T),

    #[default]
    Bottom,
}

impl<T: PartialEq + Eq + Clone + std::fmt::Debug> Join for FlatSet<T> {
    fn join(&mut self, other: &Self) {
        let result = match (&self, other) {
            (Self::Top, _) | (_, Self::Bottom) => return,
            (Self::Element(_), Self::Element(_)) => return,
            (Self::Bottom, Self::Element(x)) => Self::Element(x.clone()),
            _ => Self::Top,
        };

        *self = result;
    }
}

impl<T: PartialEq + Eq + Clone> Meet for FlatSet<T> {
    fn meet(&mut self, other: &Self) {
        let result = match (&self, other) {
            (Self::Bottom, _) | (_, Self::Top) => return,
            (Self::Element(_), Self::Element(_)) => return,
            (Self::Top, Self::Element(x)) => Self::Element(x.clone()),
            _ => Self::Bottom,
        };

        *self = result;
    }
}

impl<T: PartialEq + Eq + Clone + std::fmt::Debug> Lattice for FlatSet<T> {
    fn top() -> Self {
        Self::Top
    }

    fn bottom() -> Self {
        Self::Bottom
    }
}

impl<I: Hash + Eq + Clone, T: PartialEq + Eq + Clone + Meet> Meet for HashMap<I, T> {
    fn meet(&mut self, other: &Self) {
        for (i, val) in other {
            if let Some(v) = self.get_mut(i) {
                v.meet(val);
            } else {
                self.insert(i.clone(), val.clone());
            }
        }
    }
}

impl<I: Hash + Eq + Clone, T: PartialEq + Eq + Clone + Join> Join for HashMap<I, T> {
    fn join(&mut self, other: &Self) {
        for (i, val) in other {
            if let Some(v) = self.get_mut(i) {
                v.join(val);
            } else {
                self.insert(i.clone(), val.clone());
            }
        }
    }
}

impl<I: Hash + Eq + Clone, T: PartialEq + Eq + Clone + Join + Meet> Lattice for HashMap<I, T> {
    fn top() -> Self {
        Self::default()
    }

    fn bottom() -> Self {
        HashMap::new()
    }
}

pub enum AnalisysDirection {
    Backward,
    Forward,
}

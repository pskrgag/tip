// Infinum
pub trait MeetSemilattice {
    fn meet(&self, rhs: &Self) -> Self
    where
        Self: Sized;
}

// Supremum
pub trait JoinSemilattice {
    fn join(&self, rhs: &Self) -> Self
    where
        Self: Sized;
}

// Just wrapper around both
pub trait Lattice: JoinSemilattice + MeetSemilattice {}

#[derive(Clone)]
pub enum FlatSet<T: Clone> {
    Top,
    Value(T),
    Bottom,
}

impl<T: Clone> JoinSemilattice for FlatSet<T> {
    fn join(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (_, Self::Top) | (Self::Top, _) => Self::Top,
            (Self::Value(_), Self::Value(_)) => Self::Top,
            (x, Self::Bottom) | (Self::Bottom, x) => x.clone(),
        }
    }
}

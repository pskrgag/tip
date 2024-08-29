use crate::frontend::Indentifier;
use std::sync::atomic::{AtomicUsize, Ordering};
use enum_as_inner::EnumAsInner;

pub type TypeVariable = usize;

static UNBOUND_CNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Type {
    Unbound(TypeVariable),
    Int,
    Function(Box<Type>, Vec<Type>),
    Pointer(Box<Type>),
    Record(Vec<(Indentifier, Type)>),
}

impl Type {
    pub fn new_unbound() -> Self {
        let count = UNBOUND_CNT.fetch_add(1, Ordering::Relaxed);

        Self::Unbound(count)
    }
}

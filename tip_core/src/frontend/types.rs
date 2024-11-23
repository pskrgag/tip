use crate::frontend::Indentifier;
use enum_as_inner::EnumAsInner;

pub type TypeVariable = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Type {
    Unbound(TypeVariable),
    Int,
    Function(Box<Type>, Vec<Type>),
    Pointer(Box<Type>),
    Record(Vec<(Indentifier, Type)>),
    Void,
}

impl Type {
    pub fn is_poly(&self) -> bool {
        match self {
            Type::Int | Type::Void => false,
            Type::Unbound(_) => true,
            Type::Function(ret, args) => {
                ret.is_poly() || args.iter().fold(false, |accum, x| accum | x.is_poly())
            }
            Type::Pointer(x) => x.is_poly(),
            Type::Record(x) => x.iter().fold(false, |accum, x| accum | x.1.is_poly()),
        }
    }
}

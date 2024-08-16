use std::fmt::Debug;

pub trait UnionKey: PartialEq + Eq + Clone {
    type Value: PartialEq + Clone + Debug;

    fn idx(&self) -> usize;
    fn from_idx(i: usize) -> Self;

    fn unify(_: &Self::Value, _: &Self::Value) -> Option<Self::Value> {
        None
    }
}

#[derive(PartialEq, Eq, Clone)]
struct UnionValue<K: UnionKey> {
    value: K::Value,
    pub parent: K,
    pub rank: usize,
}

impl<K: UnionKey> UnionValue<K> {
    pub fn new(value: K::Value, parent: K) -> Self {
        Self {
            value,
            parent,
            rank: 0,
        }
    }
}

pub struct UnionSolver<T: UnionKey> {
    storage: Vec<UnionValue<T>>,
}

impl<T: UnionKey> UnionSolver<T> {
    pub fn new() -> Self {
        Self {
            storage: Vec::new(),
        }
    }

    pub fn add(&mut self, t: T::Value) -> T {
        let i = self.storage.len();
        self.storage.push(UnionValue::new(t, T::from_idx(i)));
        T::from_idx(i)
    }

    fn parent(&self, t: &T) -> &T {
        &self.storage[t.idx()].parent
    }

    fn rank(&self, t: &T) -> usize {
        self.storage[t.idx()].rank
    }

    fn value(&self, t: &T) -> &UnionValue<T> {
        &self.storage[t.idx()]
    }

    fn update_val<F: FnOnce(&mut UnionValue<T>)>(&mut self, t: &T, op: F) {
        op(&mut self.storage[t.idx()])
    }

    pub fn get_value(&self, t: T) -> T::Value {
        let t = self.parent(&t);
        self.value(&t).value.clone()
    }

    pub fn update_value(&mut self, t: T, val: T::Value) {
        let t = self.parent(&t);

        self.update_val(&t.clone(), |x| x.value = val);
    }

    pub fn find(&self, t: T) -> T {
        let val = self.value(&t);

        if t == val.parent {
            val.parent.clone()
        } else {
            self.find(val.parent.clone())
        }
    }

    pub(crate) fn union(&mut self, t: T, t1: T) {
        if self.parent(&t) == self.parent(&t1) {
            return;
        }

        if self.rank(&t) < self.rank(&t1) {
            self.update_val(&t, |val| val.parent = t1);
        } else {
            self.update_val(&t1, |val| val.parent = t.clone());

            if self.rank(&t1) == self.rank(&t) {
                self.update_val(&t, |val| val.rank += 1);
            }
        }
    }

    pub fn unify_var_val(&mut self, key: T, val: T::Value) -> Result<(), ()> {
        let par = self.parent(&key);
        let v = self.value(par);

        let res = T::unify(&v.value, &val).ok_or(())?;
        self.update_val(&par.clone(), |x| x.value = res);
        Ok(())
    }

    pub fn unify_var_var(&mut self, key2: T, key1: T) -> Result<(), ()> {
        let par1 = self.parent(&key1);
        let par2 = self.parent(&key2);
        let v1 = self.value(par1);
        let v2 = self.value(par2);

        T::unify(&v1.value, &v2.value).ok_or(())?;
        self.union(key1, key2);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(PartialEq, Eq, Clone, Debug, Copy)]
    struct TestVal(pub usize);

    impl UnionKey for TestVal {
        type Value = Option<TestVal>;

        fn from_idx(i: usize) -> Self {
            Self(i)
        }

        fn idx(&self) -> usize {
            self.0
        }
    }

    #[test]
    fn basic() {
        let mut un = UnionSolver::<TestVal>::new();

        let h1 = un.add(Some(TestVal(1)));
        let h2 = un.add(Some(TestVal(2)));
        let h3 = un.add(Some(TestVal(100)));

        assert_eq!(un.find(h1), h1);
        assert_eq!(un.find(h2), h2);
        assert_eq!(un.find(h3), h3);
    }

    #[test]
    fn union() {
        let mut un = UnionSolver::<TestVal>::new();

        let h1 = un.add(Some(TestVal(1)));
        let h2 = un.add(Some(TestVal(2)));
        let h3 = un.add(Some(TestVal(2)));

        un.union(h1, h2);

        assert_eq!(un.find(h1), un.find(h2));
        assert_ne!(un.find(h1), un.find(h3));
    }

    #[test]
    fn values() {
        let mut un = UnionSolver::<TestVal>::new();

        let h1 = un.add(Some(TestVal(1)));
        let h2 = un.add(Some(TestVal(2)));
        let h3 = un.add(None);

        un.union(h1, h2);

        let f = un.find(h1);
        let val = un.get_value(f);

        let f1 = un.find(h2);
        let val1 = un.get_value(f1);

        assert_eq!(val, val1);
        assert!(matches!(val, Some(_)));

        let f3 = un.find(h3);
        let val3 = un.get_value(f3);

        assert_eq!(val3, None);
    }

    #[derive(PartialEq, Eq, Clone, Debug, Copy)]
    struct TypeVar(pub usize);

    #[derive(Eq, PartialEq, Debug, Clone)]
    enum Type {
        Var(TestVal),
        Int,
    }

    impl UnionKey for TypeVar {
        type Value = Option<Type>;

        fn from_idx(i: usize) -> Self {
            Self(i)
        }

        fn idx(&self) -> usize {
            self.0
        }

        fn unify(lhs: &Option<Type>, rhs: &Option<Type>) -> Option<Option<Type>> {
            match (rhs, lhs) {
                (None, None) => Some(None),
                (Some(v), None) | (None, Some(v)) => Some(Some(v.clone())),
                (Some(a), Some(b)) => {
                    if a == b {
                        Some(Some(a.clone()))
                    } else {
                        None
                    }
                }
            }
        }
    }

    #[test]
    fn solver() {
        let mut un = UnionSolver::<TypeVar>::new();

        let new = un.add(None);
        let new1 = un.add(None);

        {
            let f = un.find(new);
            assert_eq!(un.get_value(f), None);
        }

        {
            let f = un.find(new1);
            assert_eq!(un.get_value(f), None);
        }

        assert!(un.unify_var_var(new, new1).is_ok());
        assert!(un.unify_var_val(new, Some(Type::Int)).is_ok());

        {
            let f = un.find(new);
            assert_eq!(un.get_value(f), Some(Type::Int));
        }

        {
            let f = un.find(new1);
            let f1 = un.find(new);
            assert_eq!(f1, f);
        }

        {
            let f = un.find(new1);
            assert_eq!(un.get_value(f), Some(Type::Int));
        }
    }
}

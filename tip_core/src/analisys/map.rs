use crate::frontend::Indentifier;
use std::collections::HashMap;
use std::fmt::Debug;

pub type Location = usize;

#[derive(Clone, Debug)]
pub enum ValueOrLoc<T: Clone> {
    Value(T),
    Location(Location),
}

#[derive(Clone)]
pub struct Enviroment<T: Clone> {
    loc: Location,
    id_loc: HashMap<Indentifier, Location>,
    loc_id: HashMap<Location, Indentifier>,
    loc_val: HashMap<Location, ValueOrLoc<T>>,
}

impl<T: Clone> Enviroment<T> {
    pub fn new() -> Self {
        Self {
            loc: 0,
            id_loc: HashMap::new(),
            loc_val: HashMap::new(),
            loc_id: HashMap::new(),
        }
    }

    pub fn get_loc(&mut self, id: &Indentifier, def: T) -> Location {
        if let Some(e) = self.id_loc.get(&id) {
            *e
        } else {
            let loc = self.loc;
            self.id_loc.insert(id.clone(), loc);
            self.loc_id.insert(loc, id.clone());

            self.loc_val.insert(self.loc, ValueOrLoc::Value(def));

            self.loc += 1;
            loc
        }
    }

    pub fn loc_val(&mut self, loc: Location) -> Indentifier {
        self.loc_id.get(&loc).unwrap().clone()
    }

    pub fn val(&mut self, loc: Location) -> ValueOrLoc<T> {
        self.loc_val.get(&loc).unwrap().clone()
    }

    pub fn assign_loc(&mut self, loc: Location, val: ValueOrLoc<T>) {
        *self.loc_val.get_mut(&loc).unwrap() = val;
    }
}

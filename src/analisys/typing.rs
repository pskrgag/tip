use super::{AnalisysResult, AstAnalisys};
use crate::frontend::*;
use crate::frontend::{
    types::{Type, TypeVariable},
    Ast,
};
use crate::solvers::union::{UnionKey, UnionSolver};
use std::collections::HashMap;

impl UnionKey for TypeVariable {
    type Value = Option<Type>;

    fn idx(&self) -> usize {
        *self
    }

    fn from_idx(i: usize) -> Self {
        i
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

#[derive(Debug, Clone)]
struct Enviroment {
    global: HashMap<Indentifier, TypeVariable>,
    local: HashMap<Indentifier, TypeVariable>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            global: HashMap::new(),
            local: HashMap::new(),
        }
    }

    pub fn add_local(&mut self, id: Indentifier, t: TypeVariable) {
        self.local.insert(id, t);
    }

    pub fn add_global(&mut self, id: Indentifier, t: TypeVariable) {
        self.global.insert(id, t);
    }

    pub fn find_local(&self, id: &Indentifier) -> Option<TypeVariable> {
        self.local.get(id).cloned()
    }

    pub fn find_glocal(&self, id: &Indentifier) -> Option<TypeVariable> {
        self.global.get(id).cloned()
    }

    pub fn clear_local(&mut self) {
        self.local.clear()
    }
}

pub struct TypeAnalysis {
    solver: UnionSolver<TypeVariable>,
    env: Enviroment,
}

impl TypeAnalysis {
    pub fn new() -> Self {
        Self {
            env: Enviroment::new(),
            solver: UnionSolver::new(),
        }
    }

    fn infer(&mut self, s: &Expression) -> Option<Type> {
        match s {
            Expression::Indentifier(x) => {
                let val = self.env.find_local(x).unwrap();
                self.solver.get_value(val).or(Some(Type::Unbound(val)))
            }
            Expression::Number(_) | Expression::Input => Some(Type::Int),
            Expression::Binary(b) => {
                let lhs = self.infer(b.lhs.as_ref()).unwrap();
                let rhs = self.infer(b.rhs.as_ref()).unwrap();

                self.unify(&lhs, &Type::Int).ok().or_else(|| {
                    println!(
                        "{:?} cannot be type lhs of binary expression in '{:?}'",
                        lhs, s
                    );
                    None
                })?;

                self.unify(&rhs, &Type::Int).ok().or_else(|| {
                    println!(
                        "{:?} cannot be type rhs of binary expression in '{:?}'",
                        rhs, s
                    );
                    None
                })?;

                Some(Type::Int)
            }
            Expression::Unary(x) => match x.as_ref() {
                Unary::Addressof(name) => {
                    let tp = self.env.find_local(name).unwrap();
                    let tp = self.solver.get_value(tp).unwrap();
                    Some(Type::Pointer(Box::new(tp)))
                }
                Unary::Deref(e) => {
                    let tp = self.infer(e).unwrap();
                    let new_type = self.solver.add(None);

                    self.unify(&tp, &Type::Pointer(Box::new(Type::Unbound(new_type))))
                        .ok()
                        .or_else(|| {
                            println!("Cannot derefence type {:?}", tp);
                            None
                        })?;

                    if let Type::Pointer(x) = tp {
                        Some(x.as_ref().clone())
                    } else if let Type::Unbound(x) = tp {
                        if let Some(Type::Pointer(x)) = self.solver.get_value(x) {
                            Some(x.as_ref().clone())
                        } else {
                            panic!("Should not happen {:?}", tp);
                        }
                    } else {
                        panic!("Should not happen {:?}", tp)
                    }
                }
            },
            Expression::Call(call) => {
                let t = match call.call.as_ref() {
                    Expression::Indentifier(x) => {
                        let var = self.env.find_glocal(x).unwrap();
                        self.solver.get_value(var).unwrap()
                    }
                    _ => self.infer(call.call.as_ref()).unwrap(),
                };

                let v = call
                    .args
                    .iter()
                    .map(|x| self.infer(x).unwrap())
                    .collect::<Vec<_>>();

                let ret = self.solver.add(None);

                self.unify(&t, &Type::Function(Box::new(Type::Unbound(ret)), v))
                    .ok()
                    .or_else(|| {
                        println!("{:?} --- ??", t);
                        None
                    })?;

                self.solver.get_value(ret)
            }
            Expression::Record(rec) => {
                let res = rec
                    .iter()
                    .map_while(|r| self.infer(r.expr.as_ref()).map(|x| (r.id.clone(), x)))
                    .collect::<Vec<_>>();

                if res.len() == rec.len() {
                    Some(Type::Record(res))
                } else {
                    None
                }
            }
            Expression::Alloc(x) => {
                let t = self.infer(x)?;
                Some(Type::Pointer(Box::new(t)))
            }
            Expression::Member(e, m) => {
                let t = self.infer(e)?;

                if let Type::Record(x) = t {
                    let r = x.iter().find(|x| &x.0 == m).or_else(|| {
                        println!("Unknown member");
                        None
                    })?;

                    Some(r.1.clone())
                } else {
                    println!("Trying to access non-record type with '.'");
                    None
                }
            }
            Expression::Null => Some(Type::Unbound(self.solver.add(None))),
        }
    }

    fn unify(&mut self, t: &Type, t1: &Type) -> Result<(), ()> {
        match (t, t1) {
            (Type::Unbound(x), Type::Unbound(x1)) => self.solver.unify_var_var(*x, *x1),
            (t, Type::Unbound(x1)) | (Type::Unbound(x1), t) => {
                // Firstly see if we know smth about unbound type
                if let Some(t) = self.solver.get_value(*x1) {
                    self.unify(&t, t1)
                } else {
                    self.solver.unify_var_val(*x1, Some(t.clone()))
                }
            }
            (Type::Record(x), Type::Record(y)) => {
                for (l, r) in std::iter::zip(x, y) {
                    if l.0 == r.0 {
                        self.unify(&l.1, &r.1)?;
                    } else {
                        return Err(());
                    }
                }

                Ok(())
            }
            (Type::Pointer(x1), Type::Pointer(x2)) => self.unify(x1, x2),
            (Type::Function(ret1, args1), Type::Function(ret2, args2)) => {
                self.unify(ret1, ret2)?;

                if args1.len() == args2.len() {
                    let v = std::iter::zip(args1, args2)
                        .map(|(x, y)| {
                            self.unify(x, y).map_err(|_| {
                                println!("Cannot pass {:?} to {:?}", x, y);
                            })
                        })
                        .collect::<Vec<_>>();
                    let old = v.len();

                    let res = v.into_iter().flatten().collect::<Vec<_>>();
                    if old != res.len() {
                        Err(())
                    } else {
                        Ok(())
                    }
                } else {
                    Err(())
                }
            }
            (t, t1) => {
                if t == t1 {
                    Ok(())
                } else {
                    println!("Wrong types {:?} {:?}", t, t1);
                    Err(())
                }
            }
        }
    }

    fn proccess_stmt(&mut self, s: &Statement) -> AnalisysResult {
        match s {
            Statement::Assign(assign) => {
                let lhs = self.infer(assign.lhs.as_ref()).ok_or(())?;
                let rhs = self.infer(assign.rhs.as_ref()).ok_or(())?;

                self.unify(&lhs, &rhs).map_err(|_| {
                    println!("Cannot assign {:?} to {:?} in '{:?}'", rhs, lhs, s);
                })?;
                Ok(())
            }
            Statement::Compound(x) => {
                for i in x {
                    self.proccess_stmt(i)?;
                }

                Ok(())
            }
            Statement::Output(x) => {
                let t = self.infer(x).ok_or(())?;

                self.unify(&t, &Type::Int).map_err(|_| {
                    println!("Cannot output type {:?} in '{:?}'", t, s);
                })?;

                Ok(())
            }
            Statement::While(wh) => {
                let t = self.infer(wh.guard.as_ref()).ok_or(())?;

                self.unify(&t, &Type::Int).map_err(|_| {
                    println!("Cannot use type {:?} as while guard '{:?}'", t, s);
                })?;

                self.proccess_stmt(wh.body.as_ref())
            }
            Statement::If(iff) => {
                let t = self.infer(iff.guard.as_ref()).ok_or(())?;

                self.unify(&t, &Type::Int).map_err(|_| {
                    println!("Cannot use type {:?} as while guard '{:?}'", t, s);
                })?;

                self.proccess_stmt(iff.then.as_ref())?;

                if let Some(elsee) = iff.elsee.as_ref() {
                    self.proccess_stmt(elsee.as_ref())
                } else {
                    Ok(())
                }
            }
            Statement::Function(_) | Statement::Return(_) => panic!("Wrong call"),
        }
    }

    fn proccess_function(&mut self, f: &mut Function) -> AnalisysResult {
        for i in f.locals() {
            for local in i {
                let key = self.solver.add(None);
                self.env.add_local(local.id().clone(), key);
            }
        }

        let mut v = vec![];
        if let Some(p) = f.params() {
            for i in p {
                let key = self.solver.add(None);
                self.env.add_local(i.id().clone(), key);

                v.push(Type::Unbound(key));
            }
        }

        let ret = self.solver.add(None);

        let function_key = self.solver.add(Some(Type::Function(
            Box::new(Type::Unbound(ret)),
            v.clone(),
        )));

        self.env.add_global(f.name().clone(), function_key);

        if let Some(b) = f.body() {
            self.proccess_stmt(b)?;
        }

        f.type_params(|x| {
            self.solver
                .get_value(self.env.find_local(x).unwrap())
                .unwrap()
        });
        f.type_local(|x| {
            self.solver
                .get_value(self.env.find_local(x).unwrap())
                .unwrap()
        });

        let ret = if f.name() == "main" {
            if let Statement::Return(x) = f.ret_e() {
                let t = self.infer(x).ok_or(())?;
                self.unify(&t, &Type::Int).map_err(|_| {
                    println!("{:?} cannot be return type of 'main'", t);
                })?;

                t
            } else {
                unreachable!()
            }
        } else if let Statement::Return(x) = f.ret_e() {
            let t = self.infer(x).ok_or(())?;

            if let Type::Function(_, y) = self.solver.get_value(function_key).unwrap() {
                self.solver
                    .update_value(function_key, Some(Type::Function(Box::new(t.clone()), y)));
            } else {
                panic!();
            }

            t
        } else {
            unreachable!()
        };

        if let Type::Unbound(x) = ret {
            f.type_retval(self.solver.get_value(x).unwrap());
        } else {
            f.type_retval(ret);
        }

        self.env.clear_local();

        Ok(())
    }
}

impl AstAnalisys for TypeAnalysis {
    fn run(&mut self, f: &mut Ast) -> AnalisysResult {
        for i in f.functions_mut() {
            self.proccess_function(i)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests::for_each_prog_parsed;
    use regex::Regex;

    #[test]
    fn test_error_programs() {
        let r = Regex::new(r"// *TEST-ERROR:.*typing").unwrap();

        for_each_prog_parsed("./typing", &r, |_, ast, path| {
            let mut analisys = TypeAnalysis::new();
            let res = analisys.run(ast);

            if res.is_ok() {
                println!("Program {:?} expected to fail type analisys", path);
                assert!(res.is_err());
            }
        })
    }

    #[test]
    fn test_ok_programs() {
        let r = Regex::new(r"// *TEST-OK:.*typing").unwrap();

        for_each_prog_parsed("./typing", &r, |_, ast, path| {
            let mut analisys = TypeAnalysis::new();
            let res = analisys.run(ast);

            if res.is_err() {
                println!("Program {:?} expected to pass type analisys", path);
                assert!(res.is_ok());
            }
        })
    }
}

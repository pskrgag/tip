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
enum Enviroment {
    Leaf(HashMap<Indentifier, TypeVariable>),
    Frame(HashMap<Indentifier, TypeVariable>, Box<Enviroment>),
}

impl Enviroment {
    pub fn new() -> Self {
        Self::Leaf(HashMap::new())
    }

    pub fn add(&mut self, id: Indentifier, t: TypeVariable) {
        match self {
            Self::Leaf(x) => x.insert(id, t),
            Self::Frame(x, _) => x.insert(id, t),
        };
    }

    pub fn new_frame(&mut self) {
        *self = Self::Frame(HashMap::new(), Box::new(self.clone()));
    }

    pub fn drop_frame(&mut self) {
        match self {
            Self::Leaf(_) => panic!(""),
            Self::Frame(_, x) => *self = *x.clone(),
        };
    }

    pub fn find(&self, id: &Indentifier) -> Option<TypeVariable> {
        match self {
            Self::Leaf(x) => x.get(id).cloned(),
            Self::Frame(cur, x) => cur.get(id).cloned().or_else(|| x.find(id)),
        }
    }
}

pub struct TypeAnalysis<'ast> {
    solver: UnionSolver<TypeVariable>,
    env: Enviroment,
    ast: &'ast Ast,
}

impl<'ast> TypeAnalysis<'ast> {
    pub fn new(ast: &'ast Ast) -> Self {
        Self {
            env: Enviroment::new(),
            solver: UnionSolver::new(),
            ast,
        }
    }

    fn infer(&mut self, s: &'ast Expression) -> Option<Type> {
        match s {
            Expression::Indentifier(x) => {
                let val = self.env.find(x).unwrap();
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
                    let tp = self.env.find(name).unwrap();
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
                        if let Some(t) = self.solver.get_value(x) {
                            if let Type::Pointer(x) = t {
                                Some(x.as_ref().clone())
                            } else {
                                panic!("Should not happen {:?}", tp);
                            }
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
                        let var = self.env.find(x).unwrap();
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
            _ => todo!(),
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
            (Type::Pointer(x1), Type::Pointer(x2)) => self.unify(x1, x2),
            (Type::Function(ret1, args1), Type::Function(ret2, args2)) => {
                self.unify(ret1, ret2)?;

                if args1.len() == args2.len() {
                    let v = std::iter::zip(args1, args2)
                        .map(|(x, y)| {
                            self.unify(x, y).or_else(|_| {
                                println!("Cannot pass {:?} to {:?}", x, y);
                                Err(())
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

    fn proccess_stmt(&mut self, s: &'ast Statement) -> AnalisysResult {
        match s {
            Statement::Assign(assign) => {
                let lhs = self.infer(assign.lhs.as_ref()).ok_or(())?;
                let rhs = self.infer(assign.rhs.as_ref()).ok_or(())?;

                self.unify(&lhs, &rhs).or_else(|_| {
                    println!("Cannot assign {:?} to {:?} in '{:?}'", rhs, lhs, s);
                    Err(())
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

                self.unify(&t, &Type::Int).or_else(|_| {
                    println!("Cannot output type {:?} in '{:?}'", t, s);
                    Err(())
                })?;

                Ok(())
            }
            Statement::While(wh) => {
                let t = self.infer(wh.guard.as_ref()).ok_or(())?;

                self.unify(&t, &Type::Int).or_else(|_| {
                    println!("Cannot use type {:?} as while guard '{:?}'", t, s);
                    Err(())
                })?;

                self.proccess_stmt(wh.body.as_ref())
            }
            Statement::If(iff) => {
                let t = self.infer(iff.guard.as_ref()).ok_or(())?;

                self.unify(&t, &Type::Int).or_else(|_| {
                    println!("Cannot use type {:?} as while guard '{:?}'", t, s);
                    Err(())
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

    fn proccess_function(&mut self, f: &'ast Function) -> AnalisysResult {
        for i in f.locals() {
            for local in i {
                let key = self.solver.add(None);
                self.env.add(local.clone(), key);
            }
        }

        let mut v = vec![];
        if let Some(p) = f.params() {
            for i in p {
                let key = self.solver.add(None);
                self.env.add(i.clone(), key);

                v.push(Type::Unbound(key));
            }
        }

        let ret = self.solver.add(None);

        let function_key = self.solver.add(Some(Type::Function(
            Box::new(Type::Unbound(ret)),
            v.clone(),
        )));

        self.env.add(f.name().clone(), function_key);

        if let Some(b) = f.body() {
            self.proccess_stmt(b)?;
        }

        if f.name() == "main" {
            if let Statement::Return(x) = f.ret_e() {
                let t = self.infer(x).ok_or(())?;

                self.unify(&t, &Type::Int).or_else(|_| {
                    println!("{:?} cannot be return type of 'main'", t);
                    Err(())
                })?;
            }
        } else {
            if let Statement::Return(x) = f.ret_e() {
                let t = self.infer(x).ok_or(())?;

                if let Type::Function(_, y) = self.solver.get_value(function_key).unwrap() {
                    self.solver
                        .update_value(function_key, Some(Type::Function(Box::new(t), y)));
                } else {
                    panic!();
                }
            }
        }

        Ok(())
    }
}

impl<'ast> AstAnalisys for TypeAnalysis<'ast> {
    fn run(&mut self) -> AnalisysResult {
        for i in self.ast.functions() {
            self.proccess_function(i)?;
        }

        Ok(())
    }
}

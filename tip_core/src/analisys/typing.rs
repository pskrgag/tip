use super::error::report_error;
use super::AstAnalisys;
use crate::frontend::*;
use crate::frontend::{
    types::{Type, TypeVariable},
    Ast,
};
use crate::solvers::union::{UnionKey, UnionSolver};
use anyhow::{bail, Result};
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
    cp: Option<UnionSolver<TypeVariable>>,
}

impl TypeAnalysis {
    pub fn new() -> Self {
        Self {
            env: Enviroment::new(),
            solver: UnionSolver::new(),
            cp: None,
        }
    }

    fn format_infer_type(&self, t: &Type) -> String {
        let t = match t {
            Type::Unbound(x) => &self.solver.get_value(*x).unwrap(),
            _ => t,
        };

        format!("{:?}", t)
    }

    fn infer(&mut self, s: &Expression) -> Result<Type> {
        match &s.kind {
            ExpressionKind::Indentifier(x) => {
                let val = crate::report_undefined_opt!(
                    self.env.find_local(x).or(self.env.find_glocal(x)),
                    s,
                    x
                );

                if let Some(x) = self.solver.get_value(val) {
                    Ok(x)
                } else {
                    Ok(Type::Unbound(val))
                }
            }
            ExpressionKind::Number(_) | ExpressionKind::Input => Ok(Type::Int),
            ExpressionKind::Binary(b) => {
                let lhs = self.infer(b.lhs.as_ref())?;
                let rhs = self.infer(b.rhs.as_ref())?;

                crate::failable!(
                    self.unify(&lhs, &Type::Int),
                    s,
                    "{:?} cannot be type lhs of binary expression in '{:?}'",
                    lhs,
                    s
                );

                crate::failable!(
                    self.unify(&rhs, &Type::Int),
                    s,
                    "{:?} cannot be type rhs of binary expression in '{:?}'",
                    rhs,
                    s,
                );

                Ok(Type::Int)
            }
            ExpressionKind::Unary(x) => match x.as_ref() {
                Unary::Addressof(name) => {
                    let tp = crate::report_undefined_opt!(
                        self.env.find_local(name).or(self.env.find_glocal(name)),
                        s,
                        name
                    );
                    let tp = self.solver.get_value(tp).unwrap_or(Type::Unbound(tp));
                    Ok(Type::Pointer(Box::new(tp)))
                }
                Unary::Deref(e) => {
                    let tp = self.infer(e)?;
                    let new_type = self.solver.add(None);

                    crate::failable!(
                        self.unify(&tp, &Type::Pointer(Box::new(Type::Unbound(new_type)))),
                        e,
                        "Cannot derefence type {:?}",
                        tp
                    );

                    if let Type::Pointer(x) = tp {
                        Ok(x.as_ref().clone())
                    } else if let Type::Unbound(x) = tp {
                        let x = self
                            .solver
                            .get_value(x)
                            .unwrap()
                            .as_pointer()
                            .unwrap()
                            .clone();
                        Ok(x.as_ref().clone())
                    } else {
                        panic!("Should not happen {:?}", tp)
                    }
                }
            },
            ExpressionKind::Call(call) => {
                let t = match &call.call.kind {
                    ExpressionKind::Indentifier(x) => {
                        let var = crate::failable_opt!(
                            self.env.find_local(x).or(self.env.find_glocal(x)),
                            call.call,
                            "Cannot find function {:?}",
                            x
                        );

                        self.solver.get_value(var).unwrap_or(Type::Unbound(var))
                    }
                    _ => self.infer(call.call.as_ref())?,
                };

                if let Type::Unbound(t) = t {
                    // Here we don't know type at calling moment. For example this
                    // happens in case of higher-order functions like
                    // apply(f) {
                    //  var a;
                    //   a = f(1234);
                    //   return fun;
                    // }
                    //
                    // So just infer type from args and go futher.
                    let types = call
                        .args
                        .iter()
                        .map(|x| self.infer(x).unwrap())
                        .collect::<Vec<_>>();

                    let ret = self.solver.add(None);
                    self.solver
                        .unify_var_val(t, Some(Type::Function(Box::new(Type::Unbound(ret)), types)))
                        .expect("Cannot unify fresh type variable");

                    return Ok(Type::Unbound(ret));
                }

                if t.is_poly() {
                    self.cp = Some(self.solver.clone());
                }

                crate::failable_match!(
                    t,
                    Type::Function(_, _),
                    s,
                    "Cannot call non-function object"
                );
                let (ret, args) = t.as_function().unwrap();

                if args.len() != call.args.len() {
                    crate::bail_with_error!(
                        s, "Cannot call function with wrong number of arguments. Expected: {}, but got {}",
                        args.len(), call.args.len());
                }

                for (called, expected) in std::iter::zip(call.args.iter(), args) {
                    let t = self.infer(called)?;

                    crate::failable!(
                        self.unify(&t, expected),
                        called,
                        "Cannot pass {:?} to function expecting {:?}",
                        self.format_infer_type(&t),
                        self.format_infer_type(expected),
                    );
                }

                let ret = if t.is_poly() {
                    let r = if let Type::Unbound(x) = ret.as_ref() {
                        self.solver.get_value(*x).unwrap()
                    } else {
                        (**ret).clone()
                    };

                    self.solver = self.cp.take().unwrap();
                    r
                } else {
                    (**ret).clone()
                };

                Ok(ret)
            }
            ExpressionKind::Record(rec) => {
                let res = rec
                    .iter()
                    .map_while(|r| self.infer(r.expr.as_ref()).ok().map(|x| (r.id.clone(), x)))
                    .collect::<Vec<_>>();

                if res.len() == rec.len() {
                    Ok(Type::Record(res))
                } else {
                    bail!("")
                }
            }
            ExpressionKind::Alloc(x) => {
                let t = self.infer(x)?;
                Ok(Type::Pointer(Box::new(t)))
            }
            ExpressionKind::Member(e, m) => {
                let t = self.infer(e)?;

                if let Type::Record(x) = t {
                    let r = crate::failable_opt!(
                        x.iter().find(|x| &x.0 == m),
                        s,
                        "Unknown member {:?}",
                        m
                    );
                    Ok(r.1.clone())
                } else {
                    println!("Trying to access non-record type with '.'");
                    bail!("")
                }
            }
            ExpressionKind::Null => Ok(Type::Unbound(self.solver.add(None))),
        }
    }

    fn unify_options(&mut self, t: Option<Type>, t1: Option<Type>) -> Result<Option<Type>> {
        match (t, t1) {
            (Some(t), Some(t1)) => {
                self.unify(&t, &t1)?;
                Ok(Some(t))
            }
            (Some(t), None) | (None, Some(t)) => Ok(Some(t)),
            _ => Ok(None),
        }
    }

    fn unify(&mut self, t: &Type, t1: &Type) -> Result<()> {
        match (t, t1) {
            (Type::Void, _) | (_, Type::Void) => bail!(""),
            (Type::Unbound(x), Type::Unbound(x1)) => self.solver.unify_var_var(*x, *x1),
            (t, Type::Unbound(x1)) | (Type::Unbound(x1), t) => {
                // Infer unknown type first
                if let Some(t1) = self.solver.get_value(*x1) {
                    self.unify(t, &t1)
                } else {
                    self.solver.unify_var_val(*x1, Some(t.clone()))
                }
            }
            (Type::Record(x), Type::Record(y)) => {
                for (l, r) in std::iter::zip(x, y) {
                    if l.0 == r.0 {
                        self.unify(&l.1, &r.1)?;
                    } else {
                        bail!("");
                    }
                }

                Ok(())
            }
            (Type::Pointer(x1), Type::Pointer(x2)) => self.unify(x1, x2),
            (Type::Function(ret1, args1), Type::Function(ret2, args2)) => {
                self.unify(ret1, ret2)?;

                if args1.len() == args2.len() {
                    for (i, j) in std::iter::zip(args1, args2) {
                        self.unify(i, j)?;
                    }

                    Ok(())
                } else {
                    bail!("")
                }
            }
            (t, t1) => {
                if t == t1 {
                    Ok(())
                } else {
                    bail!("");
                }
            }
        }
    }

    fn proccess_stmt(&mut self, s: &Statement) -> Result<Option<Type>> {
        match &s.kind {
            StatementKind::Expression(x) => {
                self.infer(x)?;
                Ok(None)
            }
            StatementKind::Assign(assign) => {
                let lhs = self.infer(assign.lhs.as_ref())?;
                let rhs = self.infer(assign.rhs.as_ref())?;

                crate::failable!(
                    self.unify(&lhs, &rhs),
                    s,
                    "Cannot assign {:?} to {:?}",
                    rhs,
                    lhs
                );
                Ok(None)
            }
            StatementKind::Compound(x) => {
                let mut t = None;

                for i in x {
                    if let Some(new) = self.proccess_stmt(i.as_ref())? {
                        if let Some(t) = &t {
                            crate::failable!(
                                self.unify(t, &new),
                                i,
                                "Cannot have different return types for function: {:?} {:?}",
                                self.format_infer_type(t),
                                self.format_infer_type(&new),
                            );
                        } else {
                            t = Some(new);
                        }
                    }
                }

                Ok(t)
            }
            StatementKind::Output(x) => {
                let t = self.infer(x.as_ref())?;

                crate::failable!(self.unify(&t, &Type::Int), s, "Cannot output type {:?}", t,);
                Ok(None)
            }
            StatementKind::While(wh) => {
                let t = self.infer(wh.guard.as_ref())?;

                crate::failable!(
                    self.unify(&t, &Type::Int),
                    s,
                    "Cannot use type {:?} as while guard",
                    t
                );

                self.proccess_stmt(wh.body.as_ref())
            }
            StatementKind::If(iff) => {
                let t = self.infer(iff.guard.as_ref())?;

                crate::failable!(
                    self.unify(&t, &Type::Int),
                    s,
                    "Cannot use type {:?} as while guard",
                    t
                );

                let t = self.proccess_stmt(iff.then.as_ref())?;

                if let Some(elsee) = iff.elsee.as_ref() {
                    let u = self.proccess_stmt(elsee.as_ref())?;
                    self.unify_options(t, u)
                } else {
                    Ok(None)
                }
            }
            StatementKind::Return(x) => Ok(Some(self.infer(x)?)),
            StatementKind::Function(_) => panic!("Wrong call"),
        }
    }

    fn proccess_function(&mut self, f: &mut Function) -> Result<()> {
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

        let mut ret_t = Type::Void;

        if let Some(b) = f.body() {
            if let Some(new_ret) = self.proccess_stmt(b)? {
                ret_t = new_ret;
            }
        }

        f.type_params(|x| {
            let unbound = self.env.find_local(x).unwrap();

            self.solver
                .get_value(unbound)
                .unwrap_or(Type::Unbound(unbound))
        });

        f.type_local(|x| {
            let unbound = self.env.find_local(x).unwrap();

            self.solver
                .get_value(unbound)
                .unwrap_or(Type::Unbound(unbound))
        });

        if f.name() == "main" {
            // HACK FOR NOW
            struct Hack {
                pub loc: crate::frontend::source::SourceLoc,
            }
            let h = Hack {
                loc: crate::frontend::source::SourceLoc { start: 0, end: 0 },
            };

            crate::failable!(
                self.unify(&ret_t, &Type::Int),
                h,
                "{:?} cannot be return type of 'main'",
                ret_t
            );
        } else if let Type::Function(_, y) = self.solver.get_value(function_key).unwrap() {
            self.solver.update_value(
                function_key,
                Some(Type::Function(Box::new(ret_t.clone()), y)),
            );
        }

        if let Type::Unbound(x) = ret_t {
            if let Some(x) = self.solver.get_value(x) {
                f.type_retval(x);
            } else {
                f.type_retval(ret_t);
            }
        } else {
            f.type_retval(ret_t);
        }

        self.env.clear_local();

        Ok(())
    }
}

impl AstAnalisys for TypeAnalysis {
    fn run(&mut self, f: &mut Ast) -> Result<()> {
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

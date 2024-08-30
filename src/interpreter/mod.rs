use crate::frontend::error::report_error;
use crate::frontend::*;
use anyhow::{bail, Context, Result};
use enum_as_inner::EnumAsInner;
use std::collections::HashMap;
use std::collections::LinkedList;

type Pointer = usize;

#[derive(Clone, Default, Debug, EnumAsInner)]
enum Value {
    #[default]
    Undefined,

    Number(i64),
    Pointer(Pointer),
    Function(FunctionPoiner),
    Record(HashMap<Indentifier, Pointer>),
    Null,
}

macro_rules! report_invalid_int {
    ($e:expr, $node:expr) => {
        match $e {
            Err(x) => match x {
                MapToIntRes::Uninitialized => { bail_with_error!($node, "Use of undefined variable"); },
                MapToIntRes::NaN => { bail_with_error!($node, "Expected integer"); },
            },
            Ok(x) => x,
        }
    };
}

macro_rules! ret_if_val {
    ($e:expr) => {
        if let ret @ Some(_) = $e? {
            return Ok(ret);
        }
    };
}

macro_rules! bail_on_void {
    ($e:expr, $node:expr) => {
        failable_opt!($e, $node, "Cannot have 'void' value")
    };
}

#[derive(Default, Debug)]
struct Env(LinkedList<HashMap<Indentifier, Pointer>>);

#[derive(Default)]
struct Store(Vec<Value>);

pub struct Interpreter<'a> {
    ast: &'a Ast,
    store: Store,
    env: Env,
}

impl Env {
    pub fn scope_begin(&mut self) {
        self.0.push_back(HashMap::new());
    }

    pub fn scope_end(&mut self) {
        self.0.pop_back();
    }

    fn map_var(&mut self, name: Indentifier, dts: Pointer) -> Result<()> {
        self.0.back_mut().context("")?.insert(name, dts);
        Ok(())
    }

    pub fn maybe_var(&self, name: &Indentifier) -> Option<Pointer> {
        self.0.back().unwrap().get(name).copied()
    }

    pub fn var(&self, name: &Indentifier) -> Result<Pointer> {
        Ok(self.maybe_var(name).context("")?)
    }
}

impl Store {
    fn new_var(&mut self, v: Value) -> Pointer {
        let res = self.0.len();

        self.0.push(v);
        res
    }

    fn read_value(&self, ptr: Pointer) -> Option<&Value> {
        let res = &self.0[ptr];

        if matches!(res, Value::Undefined) {
            None
        } else {
            Some(res)
        }
    }

    fn store_value(&mut self, ptr: Pointer, v: Value) {
        self.0[ptr] = v;
    }
}

#[derive(PartialEq, Eq)]
enum MapToIntRes {
    Uninitialized,
    NaN,
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            store: Store::default(),
            env: Env::default(),
        }
    }

    pub fn run(mut self) -> Result<i64> {
        let main = self.ast.main().expect("No main function found");
        let main_f = self.ast.function_by_index(main).unwrap();
        let val = self.run_func(main, vec![])?;

        if let Some(Value::Number(x)) = val {
            Ok(x)
        } else {
            bail_with_error!(main_f.last_ret().unwrap(), "Main should only return ints");
        }
    }

    fn map_to_int(&self, v: Value) -> Result<i64, MapToIntRes> {
        match v {
            Value::Pointer(x) => {
                let v = self.store.read_value(x).ok_or(MapToIntRes::Uninitialized)?;

                if let Value::Number(x) = v {
                    Ok(*x)
                } else {
                    Err(MapToIntRes::NaN)
                }
            }
            Value::Number(x) => Ok(x),
            _ => Err(MapToIntRes::NaN),
        }
    }

    fn exec_lhs_expr(&mut self, e: &Expression) -> Result<Value> {
        Ok(match &e.kind {
            ExpressionKind::Indentifier(x) => {
                self.env.var(x).unwrap();
                let val = report_undefined!(self.env.var(x), e, x);
                Value::Pointer(val)
            }
            ExpressionKind::Unary(x) => match x.as_ref() {
                Unary::Deref(x) => {
                    let ptr = self.exec_lhs_expr(x)?;

                    failable_match!(ptr, Value::Pointer(_), x, "Expected pointer");
                    let ptr = *ptr.as_pointer().unwrap();

                    report_uninitialized!(self.store.read_value(ptr).clone(), x).clone()
                }
                _ => {
                    bail_with_error!(e, "Unary {:?} cannot be used as lhs", x);
                }
            },
            ExpressionKind::Member(expr, id) => {
                let e = bail_on_void!(self.exec_rhs_expr(expr)?, e);

                failable_match!(e, Value::Record(_), expr, "Expected pointer");
                let x = e.as_record().unwrap();
                Value::Pointer(*x.get(id).unwrap())
            }
            _ => {
                bail_with_error!(e, "{:?} cannot be used as lhs", e);
            }
        })
    }

    fn exec_rhs_expr(&mut self, e: &Expression) -> Result<Option<Value>> {
        Ok(match &e.kind {
            ExpressionKind::Indentifier(x) => {
                if let Some(f) = self.ast.function(x.id()) {
                    Some(Value::Function(f))
                } else {
                    let var = report_undefined!(self.env.var(x), e, x);
                    Some(report_uninitialized!(self.store.read_value(var), e).clone())
                }
            }
            ExpressionKind::Number(x) => Some(Value::Number(*x)),
            ExpressionKind::Binary(b) => {
                let e1 = bail_on_void!(self.exec_rhs_expr(b.lhs.as_ref())?, b.lhs);
                let e2 = bail_on_void!(self.exec_rhs_expr(b.rhs.as_ref())?, b.rhs);

                let e1 = report_invalid_int!(self.map_to_int(e1), b.lhs);
                let e2 = report_invalid_int!(self.map_to_int(e2), b.rhs);

                Some(Value::Number(match b.op {
                    BinaryOp::Mul => e1 * e2,
                    BinaryOp::Plus => e1 + e2,
                    BinaryOp::Div => e1 / e2,
                    BinaryOp::Minus => e1 - e2,
                    BinaryOp::Gt => (e1 > e2) as i64,
                    BinaryOp::Eq => (e1 == e2) as i64,
                }))
            }
            ExpressionKind::Unary(u) => match u.as_ref() {
                Unary::Addressof(x) => {
                    let var = report_undefined!(self.env.var(x), e, x);
                    Some(Value::Pointer(var))
                }
                Unary::Deref(x) => {
                    let ptr = bail_on_void!(self.exec_rhs_expr(x)?, x);

                    failable_match!(ptr, Value::Pointer(_), x, "Expected pointer");
                    let ptr = *ptr.as_pointer().unwrap();

                    Some(report_uninitialized!(self.store.read_value(ptr), x).clone())
                }
            },
            ExpressionKind::Call(call) => {
                let res = bail_on_void!(self.exec_rhs_expr(call.call.as_ref())?, call.call);

                let f = if let Value::Function(f) = res {
                    f
                } else {
                    report_error(
                        call.call.loc,
                        format!("Expected function, but got {:?}", res).as_str(),
                        "",
                    );
                    bail!("")
                };

                let params: Vec<Value> = call.args.iter().try_fold(vec![], |mut acc, x| {
                    acc.push(bail_on_void!(self.exec_rhs_expr(x.as_ref())?, x));
                    Ok::<Vec<Value>, anyhow::Error>(acc)
                })?;

                self.run_func(f, params)?
            }
            ExpressionKind::Alloc(x) => {
                let res = bail_on_void!(self.exec_rhs_expr(x.as_ref())?, x);

                Some(Value::Pointer(self.store.new_var(res)))
            }
            ExpressionKind::Record(x) => Some(Value::Record(
                x.iter()
                    .try_fold(vec![], |mut acc, x| {
                        let val = bail_on_void!(self.exec_rhs_expr(x.expr.as_ref())?, x.expr);
                        let ptr = self.store.new_var(val);

                        acc.push((x.id.clone(), ptr));
                        Ok::<Vec<(Indentifier, usize)>, anyhow::Error>(acc)
                    })?
                    .into_iter()
                    .collect(),
            )),
            ExpressionKind::Input => {
                let mut input_line = String::new();

                std::io::stdin()
                    .read_line(&mut input_line)
                    .expect("Failed to read line");
                Some(Value::Number(
                    input_line.trim().parse().expect("Input not an integer"),
                ))
            }
            ExpressionKind::Null => Some(Value::Null),
            ExpressionKind::Member(mem, id) => {
                let e = bail_on_void!(self.exec_rhs_expr(mem)?, e);
                failable_match!(e, Value::Record(_), mem, "Expected Record type");

                let x = e.as_record().unwrap();
                Some(report_uninitialized!(self.store.read_value(*x.get(id).unwrap()), mem).clone())
            }
        })
    }

    fn val_to_str(&self, v: &Value) -> String {
        match v {
            Value::Number(x) => x.to_string(),
            Value::Pointer(x) => format!("addr{{{:x}}}", x),
            _ => panic!("Cannot print {:?}", v),
        }
    }

    fn exec_statement(&mut self, f: &Statement) -> Result<Option<Value>> {
        match &f.kind {
            StatementKind::Expression(e) => {
                self.exec_rhs_expr(e)?;
                Ok(None)
            }
            StatementKind::Assign(assign) => {
                let e = bail_on_void!(self.exec_rhs_expr(assign.rhs.as_ref())?, assign.rhs);
                let ptr = self.exec_lhs_expr(assign.lhs.as_ref())?;

                failable_match!(ptr, Value::Pointer(_), f, "pointer");

                let ptr = ptr.as_pointer().expect("never happens");
                self.store.store_value(*ptr, e);
                Ok(None)
            }
            StatementKind::Output(e) => match &e.kind {
                ExpressionKind::Indentifier(x) => {
                    let var = report_undefined!(self.env.var(x), e, x);
                    println!(
                        "{:?}",
                        self.val_to_str(report_uninitialized!(self.store.read_value(var), e))
                    );
                    Ok(None)
                }
                _ => {
                    let v = bail_on_void!(self.exec_rhs_expr(e)?, e);
                    println!("{}", self.val_to_str(&v));
                    Ok(None)
                }
            },
            StatementKind::If(iff) => {
                let guard = self.exec_lhs_expr(iff.guard.as_ref())?;

                failable_match!(guard, Value::Number(_), f, "number");

                let x = *guard.as_number().expect("never happens");
                if x == 1 {
                    ret_if_val!(self.exec_statement(iff.then.as_ref()));
                } else if let Some(els) = iff.elsee.as_ref() {
                    ret_if_val!(self.exec_statement(els));
                }
                Ok(None)
            }
            StatementKind::Compound(x) => {
                for i in x {
                    ret_if_val!(self.exec_statement(i));
                }
                Ok(None)
            }
            StatementKind::Function(_) => Ok(None),
            StatementKind::Return(e) => Ok(Some(bail_on_void!(self.exec_rhs_expr(e)?, e))),
            StatementKind::While(wl) => {
                loop {
                    let e = bail_on_void!(self.exec_rhs_expr(&wl.guard)?, wl.guard);

                    failable_match!(e, Value::Number(_), f, "number");

                    let e = *e.as_number().expect("never happens");
                    if e == 1 {
                        ret_if_val!(self.exec_statement(&wl.body));
                    } else {
                        break;
                    }
                }

                Ok(None)
            }
        }
    }

    fn run_func(&mut self, f: FunctionPoiner, args: Vec<Value>) -> Result<Option<Value>> {
        let f = self.ast.function_by_index(f).unwrap();

        self.env.scope_begin();

        if let Some(p) = f.params() {
            for (name, val) in std::iter::zip(p.iter(), args.into_iter()) {
                let var = self.store.new_var(val);

                self.env.map_var(name.id().clone(), var).unwrap()
            }
        }

        for i in f.locals() {
            for i in i {
                let var = self.store.new_var(Value::Undefined);

                self.env.map_var(i.id().clone(), var).unwrap();
            }
        }

        let mut ret = None;

        if let Some(body) = f.body() {
            ret = self.exec_statement(body)?;
        }

        self.env.scope_end();
        Ok(ret)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests::for_each_prog_parsed;
    use lalrpop_util::lalrpop_mod;
    use regex::Regex;

    lalrpop_mod!(pub tip);

    #[test]
    fn test_programs() {
        let r = Regex::new(r"// *TEST-INTERPRET: *(\d+)").unwrap();

        for_each_prog_parsed(".", &r, |caps, ast, path| {
            let num = caps[1].parse::<i64>().unwrap();

            println!("{:?}", path);
            let int = Interpreter::new(ast);
            let res = int.run().unwrap();

            if res != num {
                println!("Program {:?} produced wrong result", path);
                assert_eq!(res, num);
            }
        })
    }
}

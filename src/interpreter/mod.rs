use crate::frontend::*;
use std::collections::HashMap;
use std::collections::LinkedList;

type Pointer = usize;

#[derive(Clone, Default, Debug)]
enum Value {
    #[default]
    Undefined,

    Number(i64),
    Pointer(Pointer),
    Function(FunctionPoiner),
    Record(HashMap<Indentifier, Pointer>),
    Null,
}

#[derive(Default)]
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

    fn map_var(&mut self, name: Indentifier, dts: Pointer) {
        self.0
            .back_mut()
            .unwrap_or_else(|| panic!("use of undeclared variable {:?}", name))
            .insert(name, dts);
    }

    pub fn maybe_var(&self, name: &Indentifier) -> Option<Pointer> {
        self.0.back().unwrap().get(name).copied()
    }

    pub fn var(&self, name: &Indentifier) -> Pointer {
        self.maybe_var(name)
            .unwrap_or_else(|| panic!("Cannot find varible {:?}", name))
    }
}

impl Store {
    fn new_var(&mut self, v: Value) -> Pointer {
        let res = self.0.len();

        self.0.push(v);
        res
    }

    fn read_value(&self, ptr: Pointer) -> &Value {
        let res = &self.0[ptr];

        if matches!(res, Value::Undefined) {
            panic!("Use of uninitialized variable");
        }

        res
    }

    fn store_value(&mut self, ptr: Pointer, v: Value) {
        self.0[ptr] = v;
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            store: Store::default(),
            env: Env::default(),
        }
    }

    pub fn run(mut self) -> i64 {
        let main = self.ast.main().expect("No main function found");
        let val = self.run_func(main, vec![]);

        if let Value::Number(x) = val {
            x
        } else {
            panic!("Main should only return ints!");
        }
    }

    fn map_to_int(&self, v: Value) -> i64 {
        match v {
            Value::Pointer(x) => {
                let v = self.store.read_value(x);

                if let Value::Number(x) = v {
                    *x
                } else {
                    panic!("Expected number");
                }
            }
            Value::Number(x) => x,
            _ => panic!("Unexpected shit"),
        }
    }

    fn exec_lhs_expr(&mut self, e: &Expression) -> Value {
        match &e.kind {
            ExpressionKind::Indentifier(x) => Value::Pointer(self.env.var(x)),
            ExpressionKind::Unary(x) => match x.as_ref() {
                Unary::Deref(x) => {
                    let ptr = self.exec_lhs_expr(x);

                    if let Value::Pointer(ptr) = ptr {
                        self.store.read_value(ptr).clone()
                    } else {
                        panic!()
                    }
                }
                _ => panic!("Unary {:?} cannot be used as lhs", x),
            },
            ExpressionKind::Member(expr, id) => {
                let e = self.exec_rhs_expr(expr);

                if let Value::Record(x) = e {
                    Value::Pointer(*x.get(id).unwrap())
                } else {
                    panic!()
                }
            }
            _ => panic!("{:?} Cannot be an lhs expression", e),
        }
    }

    fn exec_rhs_expr(&mut self, e: &Expression) -> Value {
        match &e.kind {
            ExpressionKind::Indentifier(x) => {
                if let Some(f) = self.ast.function(x.id()) {
                    Value::Function(f)
                } else {
                    self.store.read_value(self.env.var(x)).clone()
                }
            }
            ExpressionKind::Number(x) => Value::Number(*x),
            ExpressionKind::Binary(b) => {
                let e1 = self.exec_rhs_expr(b.lhs.as_ref());
                let e2 = self.exec_rhs_expr(b.rhs.as_ref());

                let e1 = self.map_to_int(e1);
                let e2 = self.map_to_int(e2);

                Value::Number(match b.op {
                    BinaryOp::Mul => e1 * e2,
                    BinaryOp::Plus => e1 + e2,
                    BinaryOp::Div => e1 / e2,
                    BinaryOp::Minus => e1 - e2,
                    BinaryOp::Gt => (e1 > e2) as i64,
                    BinaryOp::Eq => (e1 == e2) as i64,
                })
            }
            ExpressionKind::Unary(u) => match u.as_ref() {
                Unary::Addressof(x) => Value::Pointer(self.env.var(x)),
                Unary::Deref(x) => {
                    let ptr = self.exec_rhs_expr(x);

                    if let Value::Pointer(ptr) = ptr {
                        self.store.read_value(ptr).clone()
                    } else {
                        panic!("Expected pointer, but got {:?}", ptr)
                    }
                }
            },
            ExpressionKind::Call(call) => {
                let res = self.exec_rhs_expr(call.call.as_ref());

                let f = if let Value::Function(f) = res {
                    f
                } else {
                    panic!("Expected fuction, but got {:?}", res);
                };

                let params = call
                    .args
                    .iter()
                    .map(|x| self.exec_rhs_expr(x.as_ref()))
                    .collect::<Vec<_>>();

                self.run_func(f, params)
            }
            ExpressionKind::Alloc(x) => {
                let res = self.exec_rhs_expr(x.as_ref());

                Value::Pointer(self.store.new_var(res))
            }
            ExpressionKind::Record(x) => Value::Record(
                x.iter()
                    .map(|x| {
                        let val = self.exec_rhs_expr(x.expr.as_ref());
                        let ptr = self.store.new_var(val);

                        (x.id.clone(), ptr)
                    })
                    .collect::<HashMap<_, _>>(),
            ),
            ExpressionKind::Input => {
                let mut input_line = String::new();

                std::io::stdin()
                    .read_line(&mut input_line)
                    .expect("Failed to read line");
                Value::Number(input_line.trim().parse().expect("Input not an integer"))
            }
            ExpressionKind::Null => Value::Null,
            ExpressionKind::Member(x, id) => {
                let e = self.exec_rhs_expr(x);

                if let Value::Record(x) = e {
                    self.store.read_value(*x.get(id).unwrap()).clone()
                } else {
                    panic!("Wtf {:?} {:?}", e, x)
                }
            }
        }
    }

    fn val_to_str(&self, v: &Value) -> String {
        match v {
            Value::Number(x) => x.to_string(),
            Value::Pointer(x) => format!("addr{{{:x}}}", x),
            _ => panic!("Cannot print {:?}", v),
        }
    }

    fn exec_statement(&mut self, f: &Statement) {
        match &f.kind {
            StatementKind::Assign(assign) => {
                let e = self.exec_rhs_expr(assign.rhs.as_ref());
                let ptr = self.exec_lhs_expr(assign.lhs.as_ref());

                if let Value::Pointer(ptr) = ptr {
                    self.store.store_value(ptr, e);
                } else {
                    panic!()
                }
            }
            StatementKind::Output(e) => match &e.kind {
                ExpressionKind::Indentifier(x) => {
                    println!(
                        "{:?}",
                        self.val_to_str(self.store.read_value(self.env.var(x)))
                    )
                }
                _ => {
                    let v = self.exec_rhs_expr(e);
                    println!("{}", self.val_to_str(&v));
                }
            },
            StatementKind::If(iff) => {
                let guard = self.exec_lhs_expr(iff.guard.as_ref());

                if let Value::Number(x) = guard {
                    if x == 1 {
                        self.exec_statement(iff.then.as_ref());
                    } else if let Some(els) = iff.elsee.as_ref() {
                        self.exec_statement(els);
                    }
                } else {
                    panic!("");
                }
            }
            StatementKind::Compound(x) => {
                for i in x {
                    self.exec_statement(i);
                }
            }
            StatementKind::Function(_) => {
                panic!("")
            }
            StatementKind::Return(_) => {
                panic!("")
            }
            StatementKind::While(wl) => loop {
                let e = self.exec_rhs_expr(&wl.guard);

                if let Value::Number(e) = e {
                    if e == 1 {
                        self.exec_statement(&wl.body);
                    } else {
                        break;
                    }
                } else {
                    panic!()
                }
            },
        }
    }

    fn run_func(&mut self, f: FunctionPoiner, args: Vec<Value>) -> Value {
        let f = self.ast.function_by_index(f).unwrap();

        self.env.scope_begin();

        if let Some(p) = f.params() {
            for (name, val) in std::iter::zip(p.iter(), args.into_iter()) {
                let var = self.store.new_var(val);

                self.env.map_var(name.id().clone(), var)
            }
        }

        for i in f.locals() {
            for i in i {
                let var = self.store.new_var(Value::Undefined);
                self.env.map_var(i.id().clone(), var);
            }
        }

        if let Some(body) = f.body() {
            self.exec_statement(body);
        }

        if let StatementKind::Return(ref x) = f.ret_e().kind {
            let res = self.exec_rhs_expr(x.as_ref());
            self.env.scope_end();
            res
        } else {
            unreachable!()
        }
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

            let int = Interpreter::new(ast);
            let res = int.run();

            if res != num {
                println!("Program {:?} produced wrong result", path);
                assert_eq!(res, num);
            }
        })
    }
}

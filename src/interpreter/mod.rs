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
    Function(Function),
    Record(HashMap<String, Pointer>),
    Null,
}

#[derive(Default)]
struct Env(LinkedList<HashMap<String, Pointer>>);

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

    fn map_var(&mut self, name: String, dts: Pointer) {
        self.0
            .back_mut()
            .expect(format!("use of undeclared variable {name}").as_str())
            .insert(name, dts);
    }

    pub fn var(&self, name: &String) -> Pointer {
        *self
            .0
            .back()
            .unwrap()
            .get(name)
            .expect(format!("use of undeclared variable {name}").as_str())
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

    pub fn run(mut self) {
        let main = self.ast.main().expect("No main function found");
        self.run_func(main, vec![]);
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

    fn exec_lhs_expr(&mut self, e: &Box<Expression>) -> Value {
        match e.as_ref() {
            Expression::Indentifier(x) => Value::Pointer(self.env.var(x.id())),
            Expression::Deref(x) => {
                let ptr = self.exec_lhs_expr(x);

                if let Value::Pointer(ptr) = ptr {
                    self.store.read_value(ptr).clone()
                } else {
                    panic!()
                }
            }
            Expression::Member(expr, id) => {
                let e = self.exec_rhs_expr(expr);

                if let Value::Record(x) = e {
                    Value::Pointer(*x.get(id.id()).unwrap())
                } else {
                    panic!()
                }
            }
            _ => panic!("Ill formed program {:?}", e),
        }
    }

    fn exec_rhs_expr(&mut self, e: &Box<Expression>) -> Value {
        match e.as_ref() {
            Expression::Indentifier(x) => self.store.read_value(self.env.var(x.id())).clone(),
            Expression::Number(x) => Value::Number(*x),
            Expression::Binary(e1, op, e2) => {
                let e1 = self.exec_rhs_expr(e1);
                let e2 = self.exec_rhs_expr(e2);

                let e1 = self.map_to_int(e1);
                let e2 = self.map_to_int(e2);

                Value::Number(match op {
                    BinaryOp::Mul => e1 * e2,
                    BinaryOp::Plus => e1 + e2,
                    BinaryOp::Div => e1 / e2,
                    BinaryOp::Minus => e1 - e2,
                    BinaryOp::Gt => (e1 > e2) as i64,
                    BinaryOp::Eq => (e1 == e2) as i64,
                })
            }
            Expression::Addresof(x) => Value::Pointer(self.env.var(x.id())),
            Expression::Deref(x) => {
                let ptr = self.exec_rhs_expr(x);

                if let Value::Pointer(ptr) = ptr {
                    self.store.read_value(ptr).clone()
                } else {
                    panic!()
                }
            }
            Expression::Call(name, params) => {
                let f = self
                    .ast
                    .function(name.id())
                    .expect("Use of undefined function");
                let params = params
                    .iter()
                    .map(|x| self.exec_rhs_expr(x))
                    .collect::<Vec<_>>();

                self.run_func(f, params)
            }
            Expression::Alloc(x) => {
                let res = self.exec_rhs_expr(x);

                Value::Pointer(self.store.new_var(res))
            }
            Expression::Record(x) => Value::Record(
                x.iter()
                    .map(|x| {
                        let val = self.exec_rhs_expr(&x.expr);
                        let ptr = self.store.new_var(val);

                        (x.id.id().clone(), ptr)
                    })
                    .collect::<HashMap<_, _>>(),
            ),
            Expression::Input => {
                let mut input_line = String::new();

                std::io::stdin()
                    .read_line(&mut input_line)
                    .expect("Failed to read line");
                Value::Number(input_line.trim().parse().expect("Input not an integer"))
            }
            Expression::Null => Value::Null,
            Expression::Member(x, id) => {
                let e = self.exec_rhs_expr(x);

                if let Value::Record(x) = e {
                    self.store.read_value(*x.get(id.id()).unwrap()).clone()
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

    fn exec_statement(&mut self, f: &Box<Statement>) {
        match f.as_ref() {
            Statement::Assign(var, e) => {
                let e = self.exec_rhs_expr(e);
                let ptr = self.exec_lhs_expr(var);

                if let Value::Pointer(ptr) = ptr {
                    self.store.store_value(ptr, e);
                } else {
                    panic!()
                }
            }
            Statement::Output(e) => match e.as_ref() {
                Expression::Indentifier(x) => {
                    println!(
                        "{:?}",
                        self.val_to_str(self.store.read_value(self.env.var(x.id())))
                    )
                }
                _ => {
                    let v = self.exec_rhs_expr(e);
                    println!("{}", self.val_to_str(&v));
                }
            },
            Statement::If(guard, then, els) => {
                let guard = self.exec_lhs_expr(guard);

                if let Value::Number(x) = guard {
                    if x == 1 {
                        self.exec_statement(then);
                    } else if let Some(els) = els {
                        self.exec_statement(els);
                    }
                } else {
                    panic!("");
                }
            }
            Statement::Compound(x) => {
                for i in x {
                    self.exec_statement(i);
                }
            }
            Statement::While(guard, x) => loop {
                let e = self.exec_rhs_expr(guard);

                if let Value::Number(e) = e {
                    if e == 1 {
                        self.exec_statement(x);
                    } else {
                        break;
                    }
                } else {
                    panic!()
                }
            },
        }
    }

    fn run_func(&mut self, f: &Function, args: Vec<Value>) -> Value {
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

        let res = self.exec_rhs_expr(f.ret_e());
        self.env.scope_end();

        res
    }
}
use super::*;
use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};

const INDENT_STEP: usize = 2;

struct IndentGuard<'a> {
    vis: &'a AstPriner<'a>,
}

pub struct AstPriner<'a> {
    ast: &'a Ast,
    pub(crate) indent: AtomicUsize,
}

impl<'a> IndentGuard<'a> {
    pub fn new(vis: &'a AstPriner<'a>) -> Self {
        vis.indent.fetch_add(1, Ordering::Relaxed);

        Self { vis }
    }
}

impl<'a> Drop for IndentGuard<'a> {
    fn drop(&mut self) {
        self.vis.indent.fetch_sub(1, Ordering::Relaxed);
    }
}

impl<'a> AstPriner<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            indent: 0.into(),
        }
    }

    fn dump_str<W: Write, S: AsRef<str>>(&self, s: S, b: &mut W) {
        writeln!(
            b,
            "{}`- {}",
            " ".repeat(self.indent.load(Ordering::Relaxed) * INDENT_STEP),
            s.as_ref(),
        )
        .unwrap();
    }

    fn new_node<W: Write, S: AsRef<str>>(&self, s: S, b: &mut W) -> IndentGuard {
        self.dump_str(s, b);
        IndentGuard::new(self)
    }

    pub fn dump<W: Write>(&self, buffer: &mut W) {
        for i in self.ast.functions() {
            self.dump_function(i, buffer);
        }
    }

    fn format_function_args(f: &Option<Vec<Indentifier>>) -> String {
        let mut s = String::new();

        if let Some(f) = f {
            for i in f {
                s.push_str(format!("{}", i.id()).as_str());
            }
        }

        s
    }

    fn dump_id<W: Write>(&self, e: &Indentifier, buffer: &mut W) {
        self.dump_str(format!("Indentifier({:?})", e.id()), buffer)
    }

    fn dump_expression<W: Write>(&self, e: &Expression, buffer: &mut W) {
        match e {
            Expression::Binary(binary) => {
                let _guard = self.new_node(format!("BinaryExpression: {}", binary.op), buffer);

                self.dump_expression(binary.lhs.as_ref(), buffer);
                self.dump_expression(binary.rhs.as_ref(), buffer);
            }
            Expression::Unary(unary) => {
                let _guard = self.new_node(
                    format!(
                        "UnaryExpression: {}",
                        match unary.as_ref() {
                            Unary::Deref(_) => "*",
                            Unary::Addressof(_) => "&",
                        }
                    ),
                    buffer,
                );

                if let Unary::Addressof(x) = unary.as_ref() {
                    self.dump_id(x, buffer);
                }

                if let Unary::Deref(x) = unary.as_ref() {
                    self.dump_expression(x, buffer);
                }
            }
            Expression::Number(n) => {
                self.dump_str(format!("Number({n})"), buffer);
            }
            Expression::Indentifier(n) => {
                self.dump_id(n, buffer);
            }
            Expression::Call(call) => {
                let _guard = self.new_node("CallExpression:", buffer);

                self.dump_expression(call.call.as_ref(), buffer);

                for i in call.args.iter() {
                    self.dump_expression(i.as_ref(), buffer);
                }
            }
            Expression::Alloc(alloc) => {
                let _guard = self.new_node("AllocExpression:", buffer);

                self.dump_expression(alloc.as_ref(), buffer);
            }
            Expression::Null => {
                self.new_node("NullExpression", buffer);
            }
            Expression::Input => {
                self.new_node("InputExpression", buffer);
            }
            Expression::Member(e, id) => {
                let _guard = self.new_node("MemberExpression", buffer);

                self.dump_expression(e, buffer);
                self.dump_id(id, buffer);
            }
            Expression::Record(recs) => {
                let _guard = self.new_node("RecordExpression", buffer);

                for i in recs.iter() {
                    self.dump_id(&i.id, buffer);
                    self.dump_expression(&i.expr, buffer);
                }
            }
        }
    }

    fn dump_statement<W: Write>(&self, st: &Statement, buffer: &mut W) {
        match st {
            Statement::If(iff) => {
                let _guard = self.new_node("IfStatement:", buffer);

                self.dump_expression(iff.guard.as_ref(), buffer);
                self.dump_statement(iff.then.as_ref(), buffer);

                if let Some(ref elsee) = iff.elsee {
                    self.dump_statement(elsee.as_ref(), buffer);
                }
            }
            Statement::Compound(stmts) => {
                let _guard = self.new_node("CompoundStatement:", buffer);

                for i in stmts {
                    self.dump_statement(i.as_ref(), buffer);
                }
            }
            Statement::Assign(assign) => {
                let _guard = self.new_node("AssignmentStatement:", buffer);

                self.dump_expression(assign.lhs.as_ref(), buffer);
                self.dump_expression(assign.rhs.as_ref(), buffer);
            }
            Statement::While(cond, compound) => {
                let _guard = self.new_node("WhileStatement:", buffer);

                self.dump_expression(cond.as_ref(), buffer);
                self.dump_statement(compound.as_ref(), buffer);
            }
            Statement::Output(expr) => {
                let _guard = self.new_node("OutputStatement:", buffer);

                self.dump_expression(expr.as_ref(), buffer);
            }
        }
    }

    fn dump_function<W: Write>(&self, f: &Function, buffer: &mut W) {
        let _guard = self.new_node(
            format!(
                "Function {} ({}):",
                f.name.id(),
                Self::format_function_args(f.params())
            ),
            buffer,
        );

        let locals = f.locals();

        for i in locals {
            let _guard = self.new_node("Local definitions:", buffer);

            for local in i {
                self.dump_str(format!("{:?}", local), buffer);
            }
        }

        if let Some(body) = f.body() {
            self.dump_statement(body, buffer);
        }

        let _g = self.new_node("ReturnStatement:", buffer);
        self.dump_expression(f.ret_e(), buffer);
    }
}

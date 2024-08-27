use std::fmt::{Display, Formatter};
use types::Type;

pub mod ast_printer;
pub mod cfg;
pub mod types;

#[macro_use]
pub mod source;

#[derive(Debug)]
pub struct Ast(Vec<Box<Statement>>);

pub fn append<T>(mut data: Vec<T>, t: T) -> Vec<T> {
    data.push(t);
    data
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Indentifier(String);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypedIndentifier(Indentifier, Option<types::Type>);

#[derive(Debug, Clone)]
pub struct Function {
    name: Indentifier,
    params: Option<Vec<TypedIndentifier>>,
    locals: Vec<Vec<TypedIndentifier>>,
    body: Option<Box<Statement>>,
    ret: Box<Statement>,
    ret_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub id: Indentifier,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Gt,
    Eq,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expression>,
    pub op: BinaryOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Unary {
    Deref(Box<Expression>),
    Addressof(Indentifier),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub call: Box<Expression>,
    pub args: Vec<Box<Expression>>,
}

#[derive(Clone)]
pub struct If {
    pub guard: Box<Expression>,
    pub then: Box<Statement>,
    pub elsee: Option<Box<Statement>>,
}

#[derive(Clone)]
pub struct While {
    pub guard: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Binary>),
    Unary(Box<Unary>),
    Number(i64),
    Indentifier(Indentifier),
    Call(Box<Call>),
    Alloc(Box<Expression>),
    Record(Vec<Record>),
    Null,
    Member(Box<Expression>, Indentifier),
    Input,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Compound(Vec<Box<Statement>>),
    Assign(Box<Assign>),
    Output(Box<Expression>),
    If(Box<If>),
    While(While),
    Function(Box<Function>),
    Return(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub loc: source::SourceLoc,
}

impl Binary {
    pub fn new(lhs: Box<Expression>, op: BinaryOp, rhs: Box<Expression>) -> Box<Self> {
        Box::new(Self { lhs, op, rhs })
    }
}

impl Indentifier {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    pub fn id(&self) -> &String {
        &self.0
    }
}

impl TypedIndentifier {
    pub fn new_untyped(s: Indentifier) -> Self {
        Self(s, None)
    }

    pub fn change_type(&mut self, t: types::Type) {
        assert!(self.1.is_none());

        self.1 = Some(t)
    }

    pub fn id(&self) -> &Indentifier {
        &self.0
    }
}

impl Function {
    pub fn new(
        name: Indentifier,
        params: Option<Vec<TypedIndentifier>>,
        locals: Vec<Vec<TypedIndentifier>>,
        body: Option<Box<Statement>>,
        ret: Box<Statement>,
    ) -> Box<Self> {
        Box::new(Self {
            name,
            params,
            locals,
            body,
            ret,
            ret_type: None,
        })
    }

    pub fn type_retval(&mut self, t: Type) {
        self.ret_type = Some(t);
    }

    pub fn type_params<F: Fn(&Indentifier) -> Type>(&mut self, f: F) {
        if let Some(p) = self.params.as_mut() {
            p.iter_mut().for_each(|x| x.change_type(f(x.id())));
        }
    }

    pub fn ret_type(&self) -> Option<&Type> {
        self.ret_type.as_ref()
    }

    pub fn type_local<F: Fn(&Indentifier) -> Type>(&mut self, f: F) {
        for j in &mut self.locals {
            j.iter_mut().for_each(|x| x.change_type(f(x.id())));
        }
    }

    pub fn params(&self) -> &Option<Vec<TypedIndentifier>> {
        &self.params
    }

    pub fn name(&self) -> &Indentifier {
        &self.name
    }

    pub fn body(&self) -> &Option<Box<Statement>> {
        &self.body
    }

    pub fn ret_e(&self) -> &Statement {
        &self.ret
    }

    pub fn locals(&self) -> &Vec<Vec<TypedIndentifier>> {
        &self.locals
    }
}

impl Record {
    pub fn new(id: Indentifier, expr: Box<Expression>) -> Self {
        Self { id, expr }
    }
}

#[derive(Clone, Default, Debug, Copy)]
pub struct FunctionPoiner(pub(crate) usize);

impl Ast {
    pub fn new(funcs: Vec<Box<Statement>>) -> Self {
        funcs
            .iter()
            .for_each(|x| assert!(matches!(x.kind, StatementKind::Function(_))));

        Self(funcs)
    }

    pub fn main(&self) -> Option<FunctionPoiner> {
        self.function("main")
    }

    pub fn function<S: AsRef<str>>(&self, name: S) -> Option<FunctionPoiner> {
        Some(FunctionPoiner(self.0.iter().position(|x| {
            if let StatementKind::Function(x) = &x.kind {
                x.name().id().as_str() == name.as_ref()
            } else {
                unreachable!()
            }
        })?))
    }

    pub fn function_by_index(&self, ptr: FunctionPoiner) -> Option<&Function> {
        let f = self.0.get(ptr.0)?;

        if let StatementKind::Function(f) = &f.kind {
            Some(f)
        } else {
            unreachable!()
        }
    }

    pub fn functions(&self) -> Vec<&Function> {
        self.0
            .iter()
            .map(|x| {
                if let StatementKind::Function(x) = &x.kind {
                    x.as_ref()
                } else {
                    unreachable!()
                }
            })
            .collect()
    }

    pub fn functions_mut(&mut self) -> Vec<&mut Function> {
        self.0
            .iter_mut()
            .map(|x| {
                if let StatementKind::Function(x) = &mut x.kind {
                    x.as_mut()
                } else {
                    unreachable!()
                }
            })
            .collect()
    }
}

impl std::fmt::Debug for Indentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Indentifier({})", self.0)
    }
}

impl std::fmt::Debug for TypedIndentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(t) = self.1.as_ref() {
            write!(f, "{:?}: type {:?}", self.0, t)
        } else {
            write!(f, "{:?}: type 'Untyped'", self.0)
        }
    }
}

impl std::fmt::Debug for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.guard)
    }
}

impl std::fmt::Debug for While {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.guard)
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lex = match self {
            Self::Gt => ">",
            Self::Eq => "==",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Plus => "+",
            Self::Minus => "-",
        };

        write!(f, "{lex}")
    }
}

impl std::cmp::PartialEq<str> for Indentifier {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use lalrpop_util::lalrpop_mod;
    use lalrpop_util::{lexer::Token, ParseError};

    lalrpop_mod!(pub tip);

    fn parse_expr(code: &str) -> Result<Box<Expression>, ParseError<usize, Token<'_>, &str>> {
        tip::ExpressionParser::new().parse(code)
    }

    fn parse_stmt(code: &str) -> Result<Box<Statement>, ParseError<usize, Token<'_>, &str>> {
        tip::StatementParser::new().parse(code)
    }

    fn parse_function(code: &str) -> Result<Box<Statement>, ParseError<usize, Token<'_>, &str>> {
        tip::FunctionParser::new().parse(code)
    }

    fn parse_program(code: &str) -> Result<Ast, ParseError<usize, Token<'_>, &str>> {
        tip::TipParser::new().parse(code)
    }

    #[test]
    fn expr_parsing() {
        assert!(parse_expr("2 + 2").is_ok());
        assert!(parse_expr("2+ 2").is_ok());
        assert!(parse_expr("2+2").is_ok());
        assert!(parse_expr("2+       2").is_ok());
        assert!(parse_expr("2+++       2").is_err());

        assert!(parse_expr("alloc 10").is_ok());
        assert!(parse_expr("&a").is_ok());
        // parse_expr("*&a").unwrap();
        assert!(parse_expr("&10").is_err());

        assert!(parse_expr("{f: 1, g: 2}").is_ok());
        assert!(parse_expr("i - 1").is_ok());
        assert!(parse_expr("i-1").is_ok());
    }

    #[test]
    fn stmt_parsing() {
        assert!(parse_stmt("a = 2 + 2;").is_ok());
        assert!(parse_stmt("a = 2 + 2").is_err());
        assert!(parse_stmt("*a = 10;").is_ok());
        assert!(parse_stmt("a=a-1;").is_ok());
        assert!(parse_stmt("a=-1;").is_ok());

        assert!(parse_stmt("output 2 + 2 + 3;").is_ok());
        assert!(parse_stmt("if (a) { a = 10; }").is_ok());
        assert!(parse_stmt("if (a) { a = 10; } else { a = 20; }").is_ok());
        assert!(parse_stmt("if (a) { a = 10; } else { a = 20; ").is_err());
        assert!(parse_stmt("if (a) { a = 10; } else { a = 20 }").is_err());

        assert!(parse_stmt("x = {f: 1, g: 2};").is_ok());
        assert!(parse_stmt("(*y).g = 4;").is_ok());
    }

    #[test]
    fn function_parsing() {
        assert!(parse_function("twice(f,x) { return 10; }").is_ok());
        assert!(parse_function("inc(x) { return y + 1; }").is_ok());
        assert!(parse_function("inc(x) { return f(x); }").is_ok());
        assert!(parse_function("inc(x) { return f(f(x)); }").is_ok());
    }

    #[test]
    fn program_parsing() {
        assert!(parse_program(
            "
twice(f, x) {
  return f(f(x));
}
inc(y) {
  return y+1;
}
main(z) {
  return twice(inc, z);
}
"
        )
        .is_ok());

        assert!(parse_program(
            "
twice(f, x) {
  return f(f(x));
}
// Some comment
inc(y) {
  return y+1; // and here
}
main(z) { // and maybe here
  return twice(inc, z);
}
"
        )
        .is_ok());
    }
}

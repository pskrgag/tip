use std::fmt::Display;

#[derive(Debug)]
pub struct Ast(Vec<Box<Function>>);

pub fn append<T>(mut data: Vec<T>, t: T) -> Vec<T> {
    data.push(t);
    data
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Indentifier(String);

#[derive(Debug, Clone)]
pub struct Function {
    name: Indentifier,
    params: Option<Vec<Indentifier>>,
    locals: Vec<Vec<Indentifier>>,
    body: Option<Box<Statement>>,
    ret: Box<Expression>,
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
pub enum Expression {
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Number(i64),
    Indentifier(Indentifier),
    Call(Box<Expression>, Vec<Box<Expression>>),
    Deref(Box<Expression>),
    Addresof(Indentifier),
    Alloc(Box<Expression>),
    Record(Vec<Record>),
    Null,
    Member(Box<Expression>, Indentifier),
    Input,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Compound(Vec<Box<Statement>>),
    Assign(Box<Expression>, Box<Expression>),
    Output(Box<Expression>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
}

impl Indentifier {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    pub fn id(&self) -> &String {
        &self.0
    }
}

impl Function {
    pub fn new(
        name: Indentifier,
        params: Option<Vec<Indentifier>>,
        locals: Vec<Vec<Indentifier>>,
        body: Option<Box<Statement>>,
        ret: Box<Expression>,
    ) -> Box<Self> {
        Box::new(Self {
            name,
            params,
            locals,
            body,
            ret,
        })
    }

    pub fn params(&self) -> &Option<Vec<Indentifier>> {
        &self.params
    }

    pub fn name(&self) -> &Indentifier {
        &self.name
    }

    pub fn body(&self) -> &Option<Box<Statement>> {
        &self.body
    }

    pub fn ret_e(&self) -> &Box<Expression> {
        &self.ret
    }

    pub fn locals(&self) -> &Vec<Vec<Indentifier>> {
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
    pub fn new(funcs: Vec<Box<Function>>) -> Self {
        Self(funcs)
    }

    pub fn main(&self) -> Option<FunctionPoiner> {
        self.function(&"main".to_owned())
    }

    pub fn function(&self, name: &String) -> Option<FunctionPoiner> {
        Some(FunctionPoiner(self.0.iter().position(|x| x.name().id() == name)?))
    }

    pub fn function_by_index(&self, ptr: FunctionPoiner) -> Option<&Box<Function>> {
        self.0.get(ptr.0)
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

    fn parse_function(code: &str) -> Result<Box<Function>, ParseError<usize, Token<'_>, &str>> {
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

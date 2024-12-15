use crate::frontend::*;

pub trait AstVisitor {
    type Context;

    fn visit_statement(&self, st: &Statement, ctx: Self::Context) -> Self::Context {
        match &st.kind {
            StatementKind::Assign(assign) => self.visit_assign(assign, ctx),
            StatementKind::Return(expr) => self.visit_return(expr, ctx),
            _ => ctx,
        }
    }

    fn visit_expression(&self, expr: &Expression, ctx: Self::Context) -> Self::Context {
        match &expr.kind {
            ExpressionKind::Indentifier(_) => self.visit_identifier(expr, ctx),
            ExpressionKind::Alloc(_) => self.visit_alloc(expr, ctx),
            ExpressionKind::Call(_) => self.visit_call(expr, ctx),
            ExpressionKind::Binary(_) => self.visit_binary(expr, ctx),
            _ => ctx,
        }
    }

    fn visit_call(&self, exp: &Expression, ctx: Self::Context) -> Self::Context {
        let call = exp.kind.as_call().unwrap();
        let ctx = self.visit_expression(&call.call, ctx);
        let ctx = call
            .args
            .iter()
            .fold(ctx, |ctx, arg| self.visit_expression(arg, ctx));

        ctx
    }

    fn visit_return(&self, exp: &Expression, ctx: Self::Context) -> Self::Context {
        self.visit_expression(exp, ctx)
    }

    fn visit_binary(&self, exp: &Expression, ctx: Self::Context) -> Self::Context {
        let bin = exp.kind.as_binary().unwrap();
        let ctx = self.visit_expression(&bin.rhs, ctx);
        let ctx = self.visit_expression(&bin.lhs, ctx);

        ctx
    }

    fn visit_alloc(&self, exp: &Expression, ctx: Self::Context) -> Self::Context {
        self.visit_expression(exp, ctx)
    }

    fn visit_identifier(&self, _expr: &Expression, ctx: Self::Context) -> Self::Context {
        ctx
    }

    fn visit_assign(&self, assign: &Assign, ctx: Self::Context) -> Self::Context {
        let ctx = self.visit_expression(&assign.lhs, ctx);
        self.visit_expression(&assign.rhs, ctx)
    }
}

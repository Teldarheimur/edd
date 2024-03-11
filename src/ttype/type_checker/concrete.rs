use crate::{parse::span::Span, ttype::{
    ast::{Expr, PlaceExpr, Statement}, Result, Type
}};

pub fn concretise_type(span: Span, t: &mut Type) -> Result<()> {
    match t {
        Type::Unknown(v) => {
            *t = v.clone().concretise().map_err(|e| e.span(span))?;
            Ok(())
        }
        Type::Option(t) => concretise_type(span, t),
        Type::ArrayPointer(t) => concretise_type(span, t),
        Type::Pointer(t) => concretise_type(span, t),
        Type::Slice(t) => concretise_type(span, t),
        Type::Array(t, _) => concretise_type(span, t),
        Type::Function(args, ret) => {
            for arg in &mut **args {
                concretise_type(span, arg)?;
            }
            concretise_type(span, ret)
        }
        Type::Struct(_) => todo!(),
        Type::Opaque
        | Type::Bool
        | Type::Byte
        | Type::U8
        | Type::I8
        | Type::U16
        | Type::I16
        | Type::U32
        | Type::I32
        | Type::CompInteger
        | Type::CompString
        | Type::Float
        | Type::Unit => Ok(()),
    }
}

pub fn concretise_statements(stmnts: &mut [Statement]) -> Result<()> {
    for stmnt in stmnts.iter_mut() {
        concretise_statement(stmnt)?;
    }
    Ok(())
}
pub fn concretise_statement(stmnt: &mut Statement) -> Result<()> {
    match stmnt {
        Statement::Express(span, t, e) => {
            concretise_type(*span, t)?;
            concretise_expr(e)
        }
        Statement::Let(span, _, t, e) => {
            concretise_type(*span, t)?;
            concretise_expr(e)
        }
        Statement::Var(span, _, t, e) => {
            concretise_type(*span, t)?;
            concretise_expr(e)
        }
        Statement::Rebind(_, p, e) => {
            concretise_pexpr(p)?;
            concretise_expr(e)
        }
        Statement::Return(_, e) => concretise_expr(e),
    }
}

fn concretise_pexpr(p: &mut PlaceExpr) -> Result<()> {
    match p {
        PlaceExpr::Ident(_, _) => Ok(()),
        PlaceExpr::Deref(_, e) => concretise_expr(e),
        PlaceExpr::Index(_, e, e2) => {
            concretise_expr(e).and_then(|()| concretise_expr(e2))
        }
        PlaceExpr::FieldAccess(_, e, _) => concretise_expr(e),
    }
}
pub fn concretise_expr(expr: &mut Expr) -> Result<()> {
    match expr {
        Expr::Ident(_, _)
        | Expr::ConstBoolean(_, _)
        | Expr::ConstI8(_, _)
        | Expr::ConstU8(_, _)
        | Expr::ConstI16(_, _)
        | Expr::ConstU16(_, _)
        | Expr::ConstI32(_, _)
        | Expr::ConstU32(_, _)
        | Expr::ConstFloat(_, _)
        | Expr::ConstCompInteger(_, _)
        | Expr::ConstUnit(_, )
        | Expr::ConstString(_, _)
        | Expr::ConstNull(_, ) => Ok(()),
        Expr::Ref(_, Err(e)) |
        Expr::Cast(_, e, _, _) |
        Expr::Not(_, e) |
        Expr::Neg(_, e) |
        Expr::Deref(_, e) => concretise_expr(e),
        Expr::Lambda(sp, args, ret, e) => {
            for (_, at) in args.iter_mut() {
                concretise_type(*sp, at)?;
            }
            concretise_type(*sp, ret)?;
            concretise_expr(e)
        }
        Expr::Ref(_, Ok(pl_e)) => concretise_pexpr(pl_e),
        Expr::Block(_, stmnts) => concretise_statements(stmnts),
        Expr::StructConstructor(_, es) => {
            for (_, e) in es.iter_mut() {
                concretise_expr(e)?;
            }
            Ok(())
        }
        Expr::Array(_, es) |
        Expr::Call(_, _, es) => {
            for e in es.iter_mut() {
                concretise_expr(e)?;
            }
            Ok(())
        }
        Expr::If(_, e, e2, e3) => {
            concretise_expr(e)?;
            concretise_expr(e2)?;
            concretise_expr(e3)
        }
        Expr::Add(_, e1, e2) |
        Expr::Sub(_, e1, e2) |
        Expr::Mul(_, e1, e2) |
        Expr::Div(_, e1, e2) |
        Expr::Concat(_, e1, e2) |
        Expr::Eq(_, e1, e2, _) |
        Expr::Neq(_, e1, e2, _) |
        Expr::Lt(_, e1, e2, _) |
        Expr::Lte(_, e1, e2, _) |
        Expr::Gt(_, e1, e2, _) |
        Expr::Gte(_, e1, e2, _) => {
            concretise_expr(e1)?;
            concretise_expr(e2)
        }
    }
}

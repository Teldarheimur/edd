use crate::ttype::{
    ast::{Expr, PlaceExpr, Statement},
    Type, Result,
};

pub fn concretise_type(t: &mut Type) -> Result<()> {
    match t {
        Type::Unknown(v) => {
            *t = v.clone().concretise()?;
            Ok(())
        }
        Type::Option(t) => concretise_type(t),
        Type::ArrayPointer(t) => concretise_type(t),
        Type::Pointer(t) => concretise_type(t),
        Type::Slice(t) => concretise_type(t),
        Type::Array(t, _) => concretise_type(t),
        Type::Function(args, ret) => {
            for arg in &mut **args {
                concretise_type(arg)?;
            }
            concretise_type(ret)
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
        Statement::Express(t, e) => {
            concretise_type(t)?;
            concretise_expr(e)
        }
        Statement::Let(_, t, e) => {
            concretise_type(t)?;
            concretise_expr(e)
        }
        Statement::Var(_, t, e) => {
            concretise_type(t)?;
            concretise_expr(e)
        }
        Statement::Rebind(p, e) => {
            concretise_pexpr(p)?;
            concretise_expr(e)
        }
        Statement::Return(e) => concretise_expr(e),
    }
}

fn concretise_pexpr(p: &mut PlaceExpr) -> Result<()> {
    match p {
        PlaceExpr::Ident(_) => Ok(()),
        PlaceExpr::Deref(e) => concretise_expr(e),
        PlaceExpr::Index(e, e2) => {
            concretise_expr(e).and_then(|()| concretise_expr(e2))
        }
        PlaceExpr::FieldAccess(e, _) => concretise_expr(e),
    }
}
pub fn concretise_expr(expr: &mut Expr) -> Result<()> {
    match expr {
        Expr::Ident(_)
        | Expr::ConstBoolean(_)
        | Expr::ConstI8(_)
        | Expr::ConstU8(_)
        | Expr::ConstI16(_)
        | Expr::ConstU16(_)
        | Expr::ConstI32(_)
        | Expr::ConstU32(_)
        | Expr::ConstFloat(_)
        | Expr::ConstCompInteger(_)
        | Expr::ConstUnit
        | Expr::ConstString(_)
        | Expr::ConstNull
        | Expr::Raise(_)
        | Expr::Var(_) => Ok(()),
        Expr::Ref(e) |
        Expr::Cast(e, _, _) |
        Expr::Not(e) |
        Expr::Neg(e) |
        Expr::Deref(e) => concretise_expr(e),
        Expr::Lambda(args, ret, e) => {
            for (_, at) in args.iter_mut() {
                concretise_type(at)?;
            }
            concretise_type(ret)?;
            concretise_expr(e)
        }
        Expr::Block(stmnts) => concretise_statements(stmnts),
        Expr::StructConstructor(es) => {
            for (_, e) in es.iter_mut() {
                concretise_expr(e)?;
            }
            Ok(())
        }
        Expr::Array(es) |
        Expr::Call(_, es) => {
            for e in es.iter_mut() {
                concretise_expr(e)?;
            }
            Ok(())
        }
        Expr::If(e, e2, e3) => {
            concretise_expr(e)?;
            concretise_expr(e2)?;
            concretise_expr(e3)
        }
        Expr::Add(e1, e2) |
        Expr::Sub(e1, e2) |
        Expr::Mul(e1, e2) |
        Expr::Div(e1, e2) |
        Expr::Concat(e1, e2) |
        Expr::Eq(e1, e2, _) |
        Expr::Neq(e1, e2, _) |
        Expr::Lt(e1, e2, _) |
        Expr::Lte(e1, e2, _) |
        Expr::Gt(e1, e2, _) |
        Expr::Gte(e1, e2, _) => {
            concretise_expr(e1)?;
            concretise_expr(e2)
        }
    }
}

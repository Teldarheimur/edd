use crate::ttype::{
    ast::{Decl, Expr, Index, PlaceExpr, Program, Statement}, Result, StorageClass, Type,
};

// TODO: check if a variable is ever addrof'd and make sure its storage class is stack

pub fn check(mut program: Program) -> Result<Program> {
    for (_, decl) in &mut program.0 {
        match decl {
            Decl::Static(_, e) => check_expr(&mut e.1),
            Decl::Const(_, e) => check_expr(&mut e.1),
            Decl::Fn(_, _, e) => check_expr(&mut e.1),
            Decl::ExternStatic(_, _) => (),
            Decl::ExternFn(_, _, _) => (),
        }
    }

    Ok(program)
}

fn check_expr(e: &mut Expr) {
    match e {
        Expr::Ident(_, _) |
        Expr::ConstBoolean(_, _) |
        Expr::ConstI8(_, _) |
        Expr::ConstU8(_, _) |
        Expr::ConstI16(_, _) |
        Expr::ConstU16(_, _) |
        Expr::ConstI32(_, _) |
        Expr::ConstU32(_, _) |
        Expr::ConstFloat(_, _) |
        Expr::ConstCompInteger(_, _) |
        Expr::ConstUnit(_) |
        Expr::ConstString(_, _) |
        Expr::ConstNull(_) => (),
        Expr::Ref(_, place_expr) => match place_expr {
            Ok(place_expr) => check_place_expr(place_expr),
            Err(e) => check_expr(e),
        }
        Expr::Array(_, _, exprs) => for expr in exprs {
            check_expr(expr);
        }
        Expr::StructConstructor(_, items) => {
            for item in items {
                check_expr(&mut item.1);
            }
        }
        Expr::Eq(_, e1, e2, _) |
        Expr::Neq(_, e1, e2, _) |
        Expr::Lt(_, e1, e2, _) |
        Expr::Lte(_, e1, e2, _) |
        Expr::Gt(_, e1, e2, _) |
        Expr::Gte(_, e1, e2, _) |
        Expr::Add(_, e1, e2) |
        Expr::Sub(_, e1, e2) |
        Expr::Mul(_, e1, e2) |
        Expr::Div(_, e1, e2) |
        Expr::Concat(_, e1, e2) => {
            check_expr(e1);
            check_expr(e2);
        }
        Expr::Lambda(_, _, _, e) |
        Expr::Not(_, e) |
        Expr::Cast(_, e, _, _) |
        Expr::Neg(_, e) |
        Expr::FieldAccess(_, e, _) |
        Expr::Deref(_, e) => {
            check_expr(e);
        }
        Expr::Index(_, e, index) => {
            check_expr(e);
            match &mut **index {
                Index::Full => (),
                Index::Index(e) |
                Index::RangeFrom(e) |
                Index::RangeToExcl(e) |
                Index::RangeToIncl(e) => {
                    check_expr(e);
                }
                Index::RangeExcl(e1, e2) |
                Index::RangeIncl(e1, e2) => {
                    check_expr(e1);
                    check_expr(e2);
                }
            }
        }
        Expr::Block(_, statements) => for statement in statements {
            match statement {
                Statement::Return(_, expr) |
                Statement::Express(_, _, expr) => check_expr(expr),
                Statement::Let(_, cell, _, t, expr) |
                Statement::Var(_, cell, _, t, expr) => {
                    match &**t {
                        Type::Unknown(_) |
                        Type::Opaque => todo!(),
                        // TODO: let structs be in registers (optimisation)
                        Type::Struct(_) |
                        Type::Function(_, _) |
                        Type::Array(_, _) => {
                            cell.set(StorageClass::Stack);
                        }
                        Type::Bool |
                        Type::Byte |
                        Type::U8 |
                        Type::I8 |
                        Type::U16 |
                        Type::I16 |
                        Type::U32 |
                        Type::I32 |
                        Type::Float |
                        Type::Unit |
                        Type::Option(_) |
                        Type::Pointer(_) |
                        Type::ArrayPointer(_) |
                        Type::Slice(_) => (),
                    }
                    check_expr(expr);
                }
                Statement::Assign(_, place_expr, expr) => {
                    check_place_expr(place_expr);
                    check_expr(expr);
                }
            }
        }
        Expr::Call(_, _, es) => {
            for e in es {
                check_expr(e);
            }
        }
        Expr::If(_, e1, e2, e3) => {
            check_expr(e1);
            check_expr(e2);
            check_expr(e3);
        }
    }
}

fn check_place_expr(place_expr: &mut PlaceExpr) {
    match place_expr {
        PlaceExpr::Index(_, e1, _, e2) => {
            check_expr(e1);
            check_expr(e2);
        }
        PlaceExpr::Ident(_, _) => {},
        PlaceExpr::FieldAccess(_, e, _) |
        PlaceExpr::Deref(_, e, _) => check_expr(e),
    }
}

use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;
use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest_derive::Parser;

use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;

pub mod ast;
pub mod span;

use self::ast::{Expr, Literal, PlaceExpr, Statement};
use crate::get_only_one;
use crate::ttype::Type;

lazy_static! {
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrattParser::new()
            .op(Op::infix(eq, Left)
                | Op::infix(neq, Left)
                | Op::infix(lt, Left)
                | Op::infix(lte, Left)
                | Op::infix(gt, Left)
                | Op::infix(gte, Left))
            .op(Op::infix(concat, Left))
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::prefix(not) | Op::prefix(r#ref) | Op::prefix(neg) | Op::prefix(deref))
            .op(Op::infix(cast_as, Left))
    };
}

pub fn parse(line: &str) -> Result<Box<[Statement]>> {
    let pairs = EddParser::parse(Rule::program, line)?;

    Ok(EddParser::parse_program(pairs))
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct EddParser;

impl EddParser {
    fn parse_literal(lit: Pairs<Rule>) -> Literal {
        let pair = get_only_one(lit);
        match pair.as_rule() {
            Rule::num => {
                let s = pair.as_str();
                s.parse()
                    .map(Literal::Integer)
                    .or_else(|_| s.parse().map(Literal::Float))
                    .unwrap()
            }
            Rule::boolean => match pair.as_str() {
                "true" => Literal::Boolean(true),
                "false" => Literal::Boolean(false),
                _ => unreachable!(),
            },
            Rule::string => {
                let mut buf = String::new();
                for part in pair.into_inner() {
                    match part.as_rule() {
                        Rule::string_part => {
                            buf.push_str(part.as_str());
                        },
                        Rule::escape_c => match part.as_str() {
                            "n" => buf.push('\n'),
                            "r" => buf.push('\r'),
                            "0" => buf.push('\0'),
                            "t" => buf.push('\t'),
                            "\\" => buf.push('\\'),
                            "\'" => buf.push('\''),
                            "\"" => buf.push('\"'),
                            x if x.starts_with('x') => match u8::from_str_radix(&x[1..], 16) {
                                Ok(c @ 0..=0x7f) => buf.push(c as char),
                                _ => unreachable!(),
                            }
                            _ => todo!("return invalid escape sequence error"),
                        },
                        _ => unreachable!(),
                    }
                }
                Literal::String(buf.into())
            },
            r => unreachable!("{r:?}"),
        }
    }
    fn parse_type(mut pairs_t: Pairs<Rule>) -> Option<Type> {
        let t = pairs_t.next();
        assert!(pairs_t.next().is_none());
        let t = t?;
        assert_eq!(t.as_rule(), Rule::r#type);
        let t = get_only_one(t.into_inner());
        Some(match t.as_rule() {
            Rule::primitive => match get_only_one(t.into_inner()).as_rule() {
                Rule::bool_t => Type::Bool,
                Rule::byte_t => Type::Byte,
                Rule::u8_t => Type::U8,
                Rule::i8_t => Type::I8,
                Rule::i16_t => Type::I16,
                Rule::u16_t => Type::U16,
                Rule::i32_t => Type::I32,
                Rule::u32_t => Type::U32,
                Rule::float_t => Type::I8,
                Rule::unit_t => Type::Unit,
                _ => unreachable!(),
            }
            Rule::opt => Type::Option(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::ptr => Type::Pointer(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::slice => Type::Slice(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::arrptr => Type::ArrayPointer(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::array => {
                let mut ps = t.into_inner();
                let size = ps.next().unwrap().as_str().parse().unwrap();
                let t = get_only_one(ps).into_inner();
                Type::Array(Box::new(Self::parse_type(t).unwrap()), size)
            }
            Rule::fntype => {
                let mut ft = t.into_inner();
                let args = ft.next().unwrap().into_inner();
                let args = args
                    .map(|t| Self::parse_type(Pairs::single(t)).unwrap())
                    .collect();
                let ret = Self::parse_type(ft).unwrap();
                Type::Function(args, Box::new(ret))
            }
            Rule::r#type => Self::parse_type(t.into_inner()).unwrap(),
            _ => unreachable!(),
        })
    }
    fn parse_typed_ident(mut pairs: Pairs<Rule>) -> (Rc<str>, Option<Type>) {
        let ident = pairs.next().unwrap().as_str();
        let annot = get_only_one(pairs);
        (ident.into(), Self::parse_type(annot.into_inner()))
    }
    fn parse_expr(expr: Pairs<Rule>) -> Expr {
        EXPR_PARSER
            .map_primary(|p| match p.as_rule() {
                Rule::literal => Expr::Const(p.as_span().into(), Self::parse_literal(p.into_inner())),
                Rule::ident => Expr::Ident(p.as_span().into(), p.as_str().into()),
                Rule::expr => Self::parse_expr(p.into_inner()),
                Rule::r#if => {
                    let span = p.as_span().into();
                    let mut pairs = p.into_inner();
                    let c = Self::parse_expr(pairs.next().unwrap().into_inner());
                    let t = Self::parse_expr(pairs.next().unwrap().into_inner());
                    let e = Self::parse_expr(get_only_one(pairs).into_inner());
                    Expr::If(span, Box::new(c), Box::new(t), Box::new(e))
                }
                Rule::lambda => {
                    let span = p.as_span().into();
                    let mut pairs = p.into_inner();
                    let idents = pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|p| Self::parse_typed_ident(p.into_inner()))
                        .collect();
                    let ret = Self::parse_type(pairs.next().unwrap().into_inner());
                    let body = Self::parse_expr(get_only_one(pairs).into_inner());

                    Expr::Lambda(span, idents, ret, Box::new(body))
                }
                Rule::call => {
                    let span = p.as_span().into();
                    let mut pairs = p.into_inner();
                    let name = pairs.next().unwrap().as_str().into();
                    let exprs = get_only_one(pairs)
                        .into_inner()
                        .map(|p| Self::parse_expr(p.into_inner()))
                        .collect();

                    Expr::Call(span, name, exprs)
                }
                Rule::block => {
                    let span = p.as_span().into();
                    let stmnts = p
                        .into_inner()
                        .map(|p| Self::parse_statement(p.into_inner()))
                        .collect();

                    Expr::Block(span, stmnts)
                }
                r => unreachable!("{r:?}"),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => Expr::Add(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::subtract => Expr::Sub(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::multiply => Expr::Mul(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::divide => Expr::Div(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::eq => Expr::Eq(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::neq => Expr::Neq(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::lt => Expr::Lt(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::lte => Expr::Lte(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::gt => Expr::Gt(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::gte => Expr::Gte(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::concat => Expr::Concat(op.as_span().into(), Box::new(lhs), Box::new(rhs)),
                Rule::cast_as => Expr::Cast(op.as_span().into(), Box::new(lhs), todo!("{}", rhs)),
                _ => unreachable!(),
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::not => Expr::Not(op.as_span().into(), Box::new(rhs)),
                Rule::r#ref => Expr::Ref(op.as_span().into(), Box::new(rhs)),
                Rule::neg => Expr::Neg(op.as_span().into(), Box::new(rhs)),
                Rule::deref => Expr::Deref(op.as_span().into(), Box::new(rhs)),
                _ => unreachable!(),
            })
            .parse(expr)
    }
    fn parse_pl_expr(pairs_t: Pairs<Rule>) -> PlaceExpr {
        let pl_expr = get_only_one(pairs_t);
        let span = pl_expr.as_span().into();
        match pl_expr.as_rule() {
            Rule::ident => PlaceExpr::Ident(span, pl_expr.as_str().into()),
            Rule::deref_expr => PlaceExpr::Deref(span, Self::parse_expr(pl_expr.into_inner())),
            Rule::array_index => {
                let mut pairs = pl_expr.into_inner();
                let e = Pairs::single(pairs.next().unwrap());
                let i = Pairs::single(pairs.next().unwrap());

                PlaceExpr::Index(span, Self::parse_expr(e), Self::parse_expr(i))
            }
            Rule::field_access => {
                let mut pairs = pl_expr.into_inner();
                let e = Pairs::single(pairs.next().unwrap());
                let i = Pairs::single(pairs.next().unwrap());

                PlaceExpr::FieldAccess(span, Self::parse_expr(e), i.as_str().into())
            }
            _ => unreachable!(),
        }
    }
    fn parse_statement(mut stmnt: Pairs<Rule>) -> Statement {
        let Some(stmnt) = stmnt.next() else {
            // TODO: FIXME
            return Statement::Express(Default::default(), Expr::Const(Default::default(), Literal::Unit));
        };
        let span = stmnt.as_span().into();
        match stmnt.as_rule() {
            Rule::expr => Statement::Express(span, Self::parse_expr(stmnt.into_inner())),
            Rule::let_decl => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let t_annotation = Self::parse_type(binding.next().unwrap().into_inner());
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Let(span, id, t_annotation, expr)
            }
            Rule::var_decl => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let t_annotation = Self::parse_type(binding.next().unwrap().into_inner());
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Var(span, id, t_annotation, expr)
            }
            Rule::rebind => {
                let mut binding = stmnt.into_inner();
                let id = Self::parse_pl_expr(binding.next().unwrap().into_inner());
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Rebind(span, id, expr)
            }
            Rule::r#return => {
                let expr = get_only_one(stmnt.into_inner());
                Statement::Express(span, Self::parse_expr(expr.into_inner()))
            }
            Rule::fn_decl => {
                let mut decl = stmnt.into_inner();
                let id = decl.next().unwrap().as_str().into();
                let typed_idents = decl.next()
                    .unwrap()
                    .into_inner()
                    .map(|ps| Self::parse_typed_ident(ps.into_inner()))
                    .collect();
                let ret = Self::parse_type(decl.next().unwrap().into_inner()).unwrap();
                let body = Self::parse_expr(Pairs::single(decl.next().unwrap()));
                Statement::Let(span, id, None, Expr::Lambda(span, typed_idents, Some(ret), Box::new(body)))
            }
            e => unreachable!("{e:?}"),
        }
    }
    fn parse_program(mut program: Pairs<Rule>) -> Box<[Statement]> {
        let stmnts = program.next().unwrap()
            .into_inner()
            .map(|p| Self::parse_statement(p.into_inner()))
            .collect();

        assert_eq!(program.next().unwrap().as_rule(), Rule::EOI);

        stmnts
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError(Box<pest::error::Error<Rule>>);

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        ParseError(Box::new(e))
    }
}

impl From<ParseError> for pest::error::Error<Rule> {
    fn from(value: ParseError) -> Self {
        *value.0
    }
}

impl Display for ParseError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.0)
    }
}

pub type Result<T> = StdResult<T, ParseError>;

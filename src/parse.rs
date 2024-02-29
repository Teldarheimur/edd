use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest_derive::Parser;

use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;

use crate::ast::{Expr, Literal, Statement};
use crate::get_only_one;

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
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::prefix(not) | Op::prefix(r#ref) | Op::prefix(neg) | Op::prefix(deref))
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
            r => unreachable!("{r:?}"),
        }
    }
    fn parse_expr(expr: Pairs<Rule>) -> Expr {
        EXPR_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::literal => Expr::Val(Self::parse_literal(primary.into_inner())),
                Rule::ident => Expr::Ident(primary.as_str().into()),
                Rule::expr => Self::parse_expr(primary.into_inner()),
                Rule::r#if => {
                    let mut pairs = primary.into_inner();
                    let c = Self::parse_expr(pairs.next().unwrap().into_inner());
                    let t = Self::parse_expr(pairs.next().unwrap().into_inner());
                    let e = Self::parse_expr(get_only_one(pairs).into_inner());
                    Expr::If(Box::new(c), Box::new(t), Box::new(e))
                }
                Rule::lambda => {
                    let mut pairs = primary.into_inner();
                    let idents = pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .into_iter()
                        .map(|p| p.as_str().into())
                        .collect();
                    let body = Self::parse_expr(get_only_one(pairs).into_inner());

                    Expr::Lambda(idents, Box::new(body))
                }
                Rule::call => {
                    let mut pairs = primary.into_inner();
                    let name = pairs.next().unwrap().as_str().into();
                    let exprs = get_only_one(pairs)
                        .into_inner()
                        .into_iter()
                        .map(|p| Self::parse_expr(p.into_inner()))
                        .collect();

                    Expr::Call(name, exprs)
                }
                r => unreachable!("{r:?}"),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Rule::subtract => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                Rule::multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Rule::divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                Rule::eq => Expr::Eq(Box::new(lhs), Box::new(rhs)),
                Rule::neq => Expr::Neq(Box::new(lhs), Box::new(rhs)),
                Rule::lt => Expr::Lt(Box::new(lhs), Box::new(rhs)),
                Rule::lte => Expr::Lte(Box::new(lhs), Box::new(rhs)),
                Rule::gt => Expr::Gt(Box::new(lhs), Box::new(rhs)),
                Rule::gte => Expr::Gte(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::not => Expr::Not(Box::new(rhs)),
                Rule::r#ref => Expr::Ref(Box::new(rhs)),
                Rule::neg => Expr::Neg(Box::new(rhs)),
                Rule::deref => Expr::Deref(Box::new(rhs)),
                _ => unreachable!(),
            })
            .parse(expr)
    }
    fn parse_statement(mut stmnt: Pairs<Rule>) -> Statement {
        let Some(stmnt) = stmnt.next() else {
            return Statement::Express(Expr::Val(Literal::Empty));
        };
        match stmnt.as_rule() {
            Rule::expr => Statement::Express(Self::parse_expr(stmnt.into_inner())),
            Rule::let_decl => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Let(id, expr)
            }
            Rule::var_decl => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Var(id, expr)
            }
            Rule::rebind => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Statement::Rebind(id, expr)
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

pub type Result<T> = StdResult<T, pest::error::Error<Rule>>;

use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest_derive::Parser;

use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;

use crate::ast::{Expr, Literal, Query};
use crate::get_only_one;

lazy_static! {
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrattParser::new()
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::infix(power, Right))
            .op(Op::infix(eq, Left)
                | Op::infix(neq, Left)
                | Op::infix(lt, Left)
                | Op::infix(lte, Left)
                | Op::infix(gt, Left)
                | Op::infix(gte, Left))
    };
}

pub fn parse(line: &str) -> Result<Query> {
    let pairs = EddParser::parse(Rule::query, line)?;

    Ok(EddParser::parse_query(pairs))
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
                Rule::ident => Expr::Ident(primary.as_str().to_owned()),
                Rule::expr => Self::parse_expr(primary.into_inner()),
                r => unreachable!("{r:?}"),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Rule::subtract => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                Rule::multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Rule::divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                Rule::power => Expr::Pow(Box::new(lhs), Box::new(rhs)),
                Rule::eq => Expr::Eq(Box::new(lhs), Box::new(rhs)),
                Rule::neq => Expr::Neq(Box::new(lhs), Box::new(rhs)),
                Rule::lt => Expr::Lt(Box::new(lhs), Box::new(rhs)),
                Rule::lte => Expr::Lte(Box::new(lhs), Box::new(rhs)),
                Rule::gt => Expr::Gt(Box::new(lhs), Box::new(rhs)),
                Rule::gte => Expr::Gte(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            })
            .parse(expr)
    }
    fn parse_query(mut query: Pairs<Rule>) -> Query {
        let query = query.next().unwrap();
        match query.as_rule() {
            Rule::expr => Query::Inquire(Self::parse_expr(query.into_inner())),
            Rule::let_binding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Let(id, expr)
            }
            Rule::var_binding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Var(id, expr)
            }
            Rule::rebinding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Rebind(id, expr)
            }
            e => unreachable!("{e:?}"),
        }
    }
}

pub type Result<T> = StdResult<T, pest::error::Error<Rule>>;

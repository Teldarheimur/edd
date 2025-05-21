use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::rc::Rc;
use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest_derive::Parser;

use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;

pub mod ast;
pub mod location;

use self::ast::{Expr, ExprOrType, Literal, PlaceExpr, Program, Index, Statement};
use self::location::Location;
use crate::get_only_one;
use crate::parse::ast::Decl;
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

pub fn parse_file(path: &Path) -> Result<Program> {
    let source = {
        let mut file = File::open(path)?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        buf
    };
    let pairs = EddParser::parse(Rule::program, &source)
        .map_err(|e| {
            let e = e.with_path(&path.to_string_lossy());

            match &e.variant {
                pest::error::ErrorVariant::ParsingError { positives, negatives } => {
                    eprintln!("pos: {positives:?}");
                    eprintln!("neg: {negatives:?}");
                }
                pest::error::ErrorVariant::CustomError { .. } => unreachable!(),
            }

            e
        })
        ?;

    EddParser::parse_program(pairs, &path.into())
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct EddParser;

impl EddParser {
    fn parse_string(s: Pairs<Rule>) -> String {
        let mut buf = String::new();
        for part in s {
            match part.as_rule() {
                Rule::string_part => {
                    buf.push_str(part.as_str());
                }
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
                    },
                    _ => todo!("return invalid escape sequence error"),
                },
                r => unreachable!("{r:?} {:?}", part.as_span().start_pos().line_col()),
            }
        }
        buf
    }
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
            Rule::string => Literal::String(Self::parse_string(pair.into_inner()).into()),
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
            },
            Rule::opt => Type::Option(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::ptr => Type::Pointer(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::slice => Type::Slice(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::arrptr => Type::ArrayPointer(Box::new(Self::parse_type(t.into_inner()).unwrap())),
            Rule::array => {
                let mut ps = t.into_inner();
                let size = ps.next().unwrap().as_str().parse().unwrap();
                Type::Array(Box::new(Self::parse_type(ps).unwrap()), size)
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
    fn parse_expr(expr: Pairs<Rule>, sf: &Rc<Path>) -> Expr {
        EXPR_PARSER
            .map_primary::<_, ExprOrType>(|p| match p.as_rule() {
                Rule::literal => Expr::Const(
                    Location::from_span(sf, p.as_span()),
                    Self::parse_literal(p.into_inner()),
                ),
                Rule::ident => Expr::Ident(Location::from_span(sf, p.as_span()), p.as_str().into()),
                Rule::expr => Self::parse_expr(p.into_inner(), sf),
                Rule::r#if => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut pairs = p.into_inner();
                    let c = Self::parse_expr(pairs.next().unwrap().into_inner(), sf);
                    let t = Self::parse_expr(pairs.next().unwrap().into_inner(), sf);
                    let e = Self::parse_expr(get_only_one(pairs).into_inner(), sf);
                    Expr::If(loc, Box::new(c), Box::new(t), Box::new(e))
                }
                Rule::lambda => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut pairs = p.into_inner();
                    let idents = pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|p| Self::parse_typed_ident(p.into_inner()))
                        .collect();
                    let ret = Self::parse_type(pairs.next().unwrap().into_inner());
                    let body = Self::parse_expr(get_only_one(pairs).into_inner(), sf);

                    Expr::Lambda(loc, idents, ret, Box::new(body))
                }
                Rule::call => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut pairs = p.into_inner();
                    let name = pairs.next().unwrap().as_str().into();
                    let exprs = get_only_one(pairs)
                        .into_inner()
                        .map(|p| Self::parse_expr(p.into_inner(), sf))
                        .collect();

                    Expr::Call(loc, name, exprs)
                }
                Rule::block => {
                    let loc = Location::from_span(sf, p.as_span());
                    let stmnts = p
                        .into_inner()
                        .map(|p| Self::parse_statement(p.into_inner(), sf))
                        .collect();

                    Expr::Block(loc, stmnts)
                }
                Rule::indexed => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let indexable = Self::parse_expr(Pairs::single(ps.next().unwrap()), sf);
                    let p = get_only_one(ps);
                    let rule = p.as_rule();
                    let mut exprs = p
                        .into_inner()
                        .map(|e| Box::new(Self::parse_expr(Pairs::single(e), sf)));

                    Expr::Index(loc, Box::new(indexable), match rule {
                        Rule::full_range => Index::Full,
                        Rule::range_from => Index::RangeFrom(
                            get_only_one(exprs),
                        ),
                        Rule::range_to_excl => Index::RangeToExcl(
                            get_only_one(exprs),
                        ),
                        Rule::range_to_incl => Index::RangeToIncl(
                            get_only_one(exprs),
                        ),
                        Rule::range_excl => Index::RangeExcl(
                            exprs.next().unwrap(),
                            get_only_one(exprs),
                        ),
                        Rule::range_incl => Index::RangeIncl(
                            exprs.next().unwrap(),
                            get_only_one(exprs),
                        ),
                        Rule::expr => Index::Index(get_only_one(exprs)),
                        r => unreachable!("{r:?}"),
                    })
                },
                Rule::array_lit => {
                    let loc = Location::from_span(sf, p.as_span());
                    let exprs = p
                        .into_inner()
                        .map(|p| Self::parse_expr(p.into_inner(), sf))
                        .collect();
                    Expr::Array(loc, exprs)
                }
                Rule::membered => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let membered = Self::parse_expr(Pairs::single(ps.next().unwrap()), sf);
                    let p = get_only_one(ps);
                    let field = p.as_str().into();

                    Expr::FieldAccess(loc, Box::new(membered), field)
                }
                r => unreachable!("{r:?}"),
            }.into())
            .map_infix(|lhs, op, rhs| {
                let lhs = lhs.into_expr();

                if let Rule::cast_as = op.as_rule() {
                    return Expr::Cast(
                        Location::from_span(sf, op.as_span()),
                        Box::new(lhs),
                        rhs.into_type()
                    ).into();
                }

                let rhs = rhs.into_expr();

                match op.as_rule() {
                Rule::add => Expr::Add(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::subtract => Expr::Sub(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::multiply => Expr::Mul(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::divide => Expr::Div(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::eq => Expr::Eq(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::neq => Expr::Neq(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::lt => Expr::Lt(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::lte => Expr::Lte(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::gt => Expr::Gt(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::gte => Expr::Gte(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                Rule::concat => Expr::Concat(
                    Location::from_span(sf, op.as_span()),
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                _ => unreachable!(),
            }}.into())
            .map_prefix(|op, rhs| {
                let rhs = rhs.into_expr();
                match op.as_rule() {
                Rule::not => Expr::Not(Location::from_span(sf, op.as_span()), Box::new(rhs)),
                Rule::r#ref => Expr::Ref(Location::from_span(sf, op.as_span()), Box::new(PlaceExpr::try_from(rhs))),
                Rule::neg => Expr::Neg(Location::from_span(sf, op.as_span()), Box::new(rhs)),
                Rule::deref => Expr::Deref(Location::from_span(sf, op.as_span()), Box::new(rhs)),
                _ => unreachable!(),
            }}.into())
            .parse(expr).into_expr()
    }
    fn parse_statement(mut stmnt: Pairs<Rule>, sf: &Rc<Path>) -> Statement {
        let Some(stmnt) = stmnt.next() else {
            // TODO: FIXME
            let loc = Location::new(sf.clone());
            return Statement::Express(loc.clone(), Expr::Const(loc, Literal::Unit));
        };
        let loc = Location::from_span(sf, stmnt.as_span());
        match stmnt.as_rule() {
            Rule::expr => Statement::Express(loc, Self::parse_expr(stmnt.into_inner(), sf)),
            Rule::let_bind => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let t_annotation = Self::parse_type(binding.next().unwrap().into_inner());
                let expr = Self::parse_expr(binding.next().unwrap().into_inner(), sf);
                Statement::Let(loc, id, t_annotation, expr)
            }
            Rule::var_bind => {
                let mut binding = stmnt.into_inner();
                let id = binding.next().unwrap().as_str().into();
                let t_annotation = Self::parse_type(binding.next().unwrap().into_inner());
                let expr = Self::parse_expr(binding.next().unwrap().into_inner(), sf);
                Statement::Var(loc, id, t_annotation, expr)
            }
            Rule::assign => {
                let mut binding = stmnt.into_inner();
                let place = Self::parse_expr(binding.next().unwrap().into_inner(), sf);
                let expr = Self::parse_expr(binding.next().unwrap().into_inner(), sf);
                Statement::Assign(loc, PlaceExpr::try_from(place).expect("TODO: RETURN ERROR INSTEAD"), expr)
            }
            Rule::r#return => {
                let expr = get_only_one(stmnt.into_inner());
                Statement::Return(loc, Self::parse_expr(expr.into_inner(), sf))
            }
            e => unreachable!("{e:?}"),
        }
    }
    /// Returns whether to export, this will probably change later
    fn parse_decorator(ps: Pairs<Rule>) -> bool {
        let mut export = false;
        for p in ps {
            match p.as_rule() {
                Rule::export => export = true,
                _ => unreachable!("{p}"),
            }
        }
        export
    }
    fn parse_program(mut ps: Pairs<Rule>, sf: &Rc<Path>) -> Result<Program> {
        let mut decls = Vec::new();
        loop {
            let p = ps.next().unwrap();
            match p.as_rule() {
                Rule::static_decl => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let (n, t) = Self::parse_typed_ident(ps.next().unwrap().into_inner());
                    let expr = Self::parse_expr(get_only_one(ps).into_inner(), sf);

                    decls.push((n, Decl::Static(loc, Box::new((t.unwrap(), expr)))));
                }
                Rule::const_decl => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps: Pairs<'_, Rule> = p.into_inner();
                    let (n, t) = Self::parse_typed_ident(ps.next().unwrap().into_inner());
                    let expr = Self::parse_expr(get_only_one(ps).into_inner(), sf);

                    decls.push((n, Decl::Const(loc, Box::new((t.unwrap(), expr)))));
                }
                Rule::fn_decl => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let export = Self::parse_decorator(ps.next().unwrap().into_inner());
                    let n = ps.next().unwrap().as_str().into();
                    let typed_idents = ps
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|ps| Self::parse_typed_ident(ps.into_inner()))
                        .map(|(n, t)| (n, t.unwrap()))
                        .collect();
                    let ret = Self::parse_type(ps.next().unwrap().into_inner()).unwrap();
                    let body = Self::parse_expr(Pairs::single(ps.next().unwrap()), sf);

                    if export {
                        decls.push((n, Decl::ExportFn(loc, typed_idents, Box::new((ret, body)))));
                    } else {
                        decls.push((n, Decl::LocalFn(loc, typed_idents, Box::new((ret, body)))));
                    }
                }
                Rule::extern_fn_decl => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let n = ps.next().unwrap().as_str().into();
                    let typed_idents = ps
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|ps| Self::parse_typed_ident(ps.into_inner()))
                        .map(|(n, t)| (n, t.unwrap()))
                        .collect();
                    let ret = Self::parse_type(ps.next().unwrap().into_inner()).unwrap();

                    decls.push((n, Decl::ExternFn(loc, typed_idents, Box::new(ret))));
                }
                Rule::extern_decl => {
                    let loc = Location::from_span(sf, p.as_span());
                    let mut ps = p.into_inner();
                    let (n, t) = Self::parse_typed_ident(ps.next().unwrap().into_inner());

                    decls.push((n, Decl::ExternStatic(loc, Box::new(t.unwrap()))));
                }
                Rule::include => {
                    let path = Self::parse_string(get_only_one(p.into_inner()).into_inner());
                    let path = sf.parent().unwrap().join(path);
                    let mut inner_program = parse_file(&path)?;
                    decls.append(&mut inner_program.0);
                }
                Rule::EOI => break,
                _ => unreachable!(),
            }
        }

        Ok(Program(decls))
    }
}

#[derive(Debug)]
pub enum ParseError {
    Pest(Box<pest::error::Error<Rule>>),
    IoError(io::Error),
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        ParseError::Pest(Box::new(e))
    }
}
impl From<io::Error> for ParseError {
    fn from(e: io::Error) -> Self {
        ParseError::IoError(e)
    }
}

impl Display for ParseError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Pest(e) => e.fmt(f),
            ParseError::IoError(e) => e.fmt(f),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseError::Pest(e) => Some(e),
            ParseError::IoError(e) => Some(e),
        }
    }
}

pub type Result<T> = StdResult<T, ParseError>;

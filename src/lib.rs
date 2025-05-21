use std::{fmt::Display, path::Path};

use flat::{flatten, Program};
use ttype::type_checker::check;

use self::parse::parse_file;

pub mod flat;
pub mod parse;
pub mod rt;
pub mod ttype;
pub mod telda;
pub mod small_set;
pub mod regalloc;

#[track_caller]
fn get_only_one<R, I: Iterator<Item = R>>(mut iter: I) -> R {
    let one = iter.next().unwrap();
    assert!(iter.next().is_none());
    one
}

#[derive(Debug, Default)]
pub struct CompileOptions {
    parsed_hook: Option<fn(&self::parse::ast::Program)>,
    checked_hook: Option<fn(&self::ttype::ast::Program)>,
}

pub fn compile(path: &Path, options: CompileOptions) -> Result<Program, Box<dyn Display>> {
    let program = parse_file(path).map_err(|e| -> Box<dyn Display> { Box::new(e) })?;
    if let Some(hook) = options.parsed_hook {
        hook(&program);
    }
    let program = check(program).map_err(|e| -> Box<dyn Display> { Box::new(e) })?;
    if let Some(hook) = options.checked_hook {
        hook(&program);
    }
    let program = flatten(program);

    Ok(program)
}

impl CompileOptions {
    pub fn hook_parsed(self, hook: fn(&self::parse::ast::Program)) -> Self {
        Self {
            parsed_hook: Some(hook),
            ..self
        }
    }
    pub fn hook_type_checked(self, hook: fn(&self::ttype::ast::Program)) -> Self {
        Self {
            checked_hook: Some(hook),
            ..self
        }
    }
}

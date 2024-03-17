use super::Program;

mod const_prop;
mod dead_removal;
mod dead_path;

pub type Pass = fn(Program) -> Program;

pub use self::{
    const_prop::const_prop_pass,
    dead_removal::dead_removal_pass,
    dead_path::dead_path_removal_pass,
};

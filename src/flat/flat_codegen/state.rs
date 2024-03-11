use std::{collections::HashMap, rc::Rc};

use crate::flat::{ticker::Ticker, Function, Global, Ident, Label, Line, StaticDecl, Temp};

pub struct FlattenState<'a> {
    fn_name: &'a str,
    symtab: HashMap<Rc<str>, Temp>,
    lines: &'a mut Vec<Line>,
    pub statics: &'a mut Vec<StaticDecl>,
    pub fns: &'a mut HashMap<Global, Function>,
    local_names: Vec<Box<str>>,
    label_ticker: Ticker,
    global_ticker: Ticker,
}
impl<'a> FlattenState<'a> {
    pub fn new<I: IntoIterator<Item=Rc<str>>>(
        fn_name: &'a str,
        args: I,
        lines: &'a mut Vec<Line>,
        statics: &'a mut Vec<StaticDecl>,
        fns: &'a mut HashMap<Global, Function>,
    ) -> Self {
        let mut ret = Self {
            fn_name,
            symtab: HashMap::new(),
            lines,
            statics,
            fns,
            local_names: vec!["_".into()],
            label_ticker: Ticker::new(),
            global_ticker: Ticker::new(),
        };
        for name in args {
            ret.new_temp_from_identifier(name);
        }
        ret
    }
    pub fn add_code(&mut self, line: Line) {
        self.lines.push(line);
    }
    pub fn ident_from_identifier(&self, identifier: Rc<str>) -> Ident {
        if let Some(local) = self.symtab.get(&identifier) {
            return local.clone().into();
        }

        Global(identifier).into()
    }
    pub fn new_temp_from_identifier(&mut self, identifier: Rc<str>) -> Temp {
        let temp = self.new_temp(&identifier);
        self.symtab.insert(identifier, temp.clone());
        temp
    }
    pub fn new_temp(&mut self, name: &str) -> Temp {
        let index = self.local_names.len();
        self.local_names.push(format!("__{name}").into_boxed_str());

        Temp(index)
    }
    pub fn new_label(&mut self) -> Label {
        Label(self.label_ticker.tick())
    }
    pub fn new_global(&mut self, purpose: &str) -> Global {
        let n = self.global_ticker.tick();
        Global(format!("#g@{}${purpose}{n}", self.fn_name).into())
    }
    pub fn into_local_names(self) -> Vec<Box<str>> {
        self.local_names
    }
}

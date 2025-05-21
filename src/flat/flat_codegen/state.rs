use std::{collections::HashMap, iter, rc::Rc};

use crate::flat::{
    ticker::Ticker, FlatType, Function, Global, Label, Line, StackVar, StaticDecl, Temp
};

#[derive(Debug)]
pub enum Local {
    Register(Temp),
    Stack(StackVar),
}
#[derive(Debug)]
pub enum Named {
    Temp(Temp),
    Stack(StackVar),
    Global(Global)
}
pub struct FlattenState<'a> {
    fn_name: &'a str,
    local_symtab: HashMap<Rc<str>, Local>,
    pub statics: &'a mut Vec<StaticDecl>,
    pub fns: &'a mut HashMap<Global, Function>,
    temp_types: Vec<Option<FlatType>>,
    stack_types: Vec<FlatType>,
    label_ticker: Ticker,
    global_ticker: Ticker,
    function: &'a mut Function,
    epilogue: Vec<Line>,
}
impl<'a> FlattenState<'a> {
    pub fn new(
        fn_name: &'a str,
        function: &'a mut Function,
        statics: &'a mut Vec<StaticDecl>,
        fns: &'a mut HashMap<Global, Function>,
    ) -> Self {
        Self {
            temp_types: iter::once(None)
                .chain(function.arg_types.iter().map(|t| Some(t.clone())))
                .collect(),
            local_symtab: function
                .reg_names
                .iter()
                .skip(1)
                .zip(1..)
                .map(|(n, i)| ((**n).into(), Local::Register(Temp(i))))
                .collect(),
            stack_types: Vec::new(),
            fn_name,
            function,
            statics,
            fns,
            label_ticker: Ticker::new(),
            global_ticker: Ticker::new(),
            epilogue: Vec::new(),
        }
    }
    pub fn add_code(&mut self, line: Line) {
        self.function.lines.push(line);
    }
    pub fn lookup(&self, identifier: Rc<str>) -> (Named, FlatType) {
        match self.local_symtab.get(&identifier) {
            Some(Local::Register(temp)) => (
                Named::Temp(temp.clone()),
                self.temp_types.get(temp.inner()).cloned().flatten().expect("temp has no type"),
            ),
            Some(Local::Stack(sv)) => (
                Named::Stack(sv.clone()),
                self.stack_types.get(sv.inner()).cloned().expect("stack var has no type"),
            ),
            None => {
                let g = Global(identifier);
                (Named::Global(g.clone()), self.get_global_type(g).expect("global has no type"))
            }
        }
    }
    pub fn new_temp_from_identifier(&mut self, identifier: Rc<str>, t: FlatType) -> Temp {
        let temp = self.new_temp(&identifier, t);
        self.local_symtab.insert(identifier, Local::Register(temp.clone()));
        temp
    }
    pub fn new_sv_from_identifier(&mut self, identifier: Rc<str>, t: FlatType) -> StackVar {
        let sv = self.new_sv(&identifier, t);
        self.local_symtab.insert(identifier, Local::Stack(sv.clone()));
        sv
    }
    pub const fn temp_hole(&self) -> Temp {
        Temp(0)
    }
    pub fn new_temp(&mut self, name: &str, t: FlatType) -> Temp {
        let index = self.function.reg_names.len();
        self.function
            .reg_names
            .push(format!("__{name}").into_boxed_str());

        let temp = Temp(index);
        self.set_temp_type(temp.clone(), t);
        temp
    }
    pub fn new_sv(&mut self, name: &str, t: FlatType) -> StackVar {
        let index = self.function.stack_names.len();
        self.function
            .stack_names
            .push(format!("__{name}").into_boxed_str());

        let sv = StackVar(index);
        _ = t;
        self.set_stack_type(sv.clone(), t);
        sv
    }
    pub fn new_label(&mut self) -> Label {
        Label(self.label_ticker.tick())
    }
    pub fn new_global(&mut self, purpose: &str) -> Global {
        let n = self.global_ticker.tick();
        Global(format!("#g@{}${purpose}{n}", self.fn_name).into())
    }
    pub fn set_temp_type(&mut self, temp: Temp, t: FlatType) {
        if self.temp_types.len() <= temp.inner() {
            self.temp_types.resize(temp.inner() + 1, None);
        }
        self.temp_types[temp.inner()] = Some(t);
    }
    pub fn set_stack_type(&mut self, sv: StackVar, t: FlatType) {
        debug_assert!(sv.inner() == self.stack_types.len());
        self.stack_types.push(t);
    }
    pub fn get_global_type(&self, g: Global) -> Option<FlatType> {
        if let Some(f) = self.fns.get(&g) {
            return Some(FlatType::FnPtr(
                f.arg_types.clone(),
                Box::new(f.ret_type.clone()),
            ));
        } else if &**g.inner() == self.fn_name {
            return Some(FlatType::FnPtr(
                self.function.arg_types.clone(),
                Box::new(self.function.ret_type.clone()),
            ));
        }
        for decl in self.statics.iter() {
            match decl {
                StaticDecl::SetConst(n, t, _)
                | StaticDecl::SetAlias(n, t, _)
                | StaticDecl::SetArray(n, t, _)
                | StaticDecl::SetString(n, t, _)
                | StaticDecl::External(n, t)
                | StaticDecl::SetPtr(n, t, _) => {
                    if n == &g {
                        return Some(t.clone());
                    }
                }
            }
        }
        None
    }
    
    pub fn epilogue_marker(&self) -> EpilogueMarker {
        EpilogueMarker(self.epilogue.len())
    }
    pub fn add_epilogue_code(&mut self, line: Line) {
        self.epilogue.push(line);
    }
    pub fn epilogue(&mut self, marker: EpilogueMarker) {
        let start = marker.0;
        while self.epilogue.len() > start {
            let line = self.epilogue.pop().unwrap();
            self.add_code(line);
        }
    }
}

pub struct EpilogueMarker(usize);

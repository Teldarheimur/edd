use std::{collections::HashMap, iter, rc::Rc};

use crate::flat::{
    ticker::Ticker, FlatType, Function, Global, Ident, Label, Line, StackOffset, StaticDecl, Temp
};

pub struct FlattenState<'a> {
    fn_name: &'a str,
    register_symtab: HashMap<Rc<str>, Temp>,
    stackvar_symtab: HashMap<Rc<str>, StackOffset>,
    pub statics: &'a mut Vec<StaticDecl>,
    pub fns: &'a mut HashMap<Global, Function>,
    local_types: Vec<Option<FlatType>>,
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
            local_types: iter::once(None)
                .chain(function.arg_types.iter().map(|t| Some(t.clone())))
                .collect(),
            register_symtab: function
                .reg_names
                .iter()
                .skip(1)
                .zip(1..)
                .map(|(n, i)| ((**n).into(), Temp(i)))
                .collect(),
            stackvar_symtab: HashMap::new(),
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
    pub fn ident_from_identifier(&self, identifier: Rc<str>) -> Ident {
        if let Some(local) = self.register_symtab.get(&identifier) {
            return local.clone().into();
        }
        if let Some(so) = self.stackvar_symtab.get(&identifier) {
            return so.clone().into();
        }

        Global(identifier).into()
    }
    pub fn new_temp_from_identifier(&mut self, identifier: Rc<str>, t: FlatType) -> Temp {
        let temp = self.new_temp(&identifier, t);
        self.register_symtab.insert(identifier, temp.clone());
        temp
    }
    pub fn new_so_from_identifier(&mut self, identifier: Rc<str>, t: FlatType) -> StackOffset {
        let so = self.new_so(&identifier, t);
        self.stackvar_symtab.insert(identifier, so.clone());
        so
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
        self.set_type(temp.clone(), t);
        temp
    }
    pub fn new_so(&mut self, name: &str, t: FlatType) -> StackOffset {
        let index = self.function.stack_names.len();
        self.function
            .stack_names
            .push(format!("__{name}").into_boxed_str());

        let so = StackOffset(index);
        _ = t;
        // self.set_type(temp.clone(), t);
        so
    }
    pub fn new_label(&mut self) -> Label {
        Label(self.label_ticker.tick())
    }
    pub fn new_global(&mut self, purpose: &str) -> Global {
        let n = self.global_ticker.tick();
        Global(format!("#g@{}${purpose}{n}", self.fn_name).into())
    }
    pub fn set_type(&mut self, temp: Temp, t: FlatType) {
        if self.local_types.len() <= temp.inner() {
            self.local_types.resize(temp.inner() + 1, None);
        }
        self.local_types[temp.inner()] = Some(t);
    }
    pub fn get_type<I: Into<Ident>>(&self, var: I) -> Option<FlatType> {
        match var.into() {
            Ident::Reg(temp) => self.local_types.get(temp.inner()).cloned().flatten(),
            Ident::Stack(_so) => todo!(),
            Ident::Global(g) => {
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
        }
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

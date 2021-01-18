#[derive(Debug, PartialEq)]
pub enum Typ {
    Int,
    Bool,
    Arr(Box<Typ>, Box<Typ>),
    Metavar(u32),
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(i32),
    Bool(bool)
}

pub type Id = String;

#[derive(Debug, PartialEq)]
pub enum Exp {
    Lit(Lit),
    Var(Id),
    Fun(Id, Typ, Box<Exp>),
    App(Box<Exp>, Box<Exp>)
}


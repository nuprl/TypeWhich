#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Int,
    Bool,
    Arr(Box<Typ>, Box<Typ>),
    Metavar(u32),
}

impl Typ {
    pub fn expect_metavar(&self) -> u32 {
        match self {
            Typ::Metavar(n) => *n,
            _ => panic!("expected a Typ::Metavar"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(i32),
    Bool(bool),
}

impl Lit {
    pub fn typ(&self) -> Typ {
        match self {
            Lit::Int(_) => Typ::Int,
            Lit::Bool(_) => Typ::Bool,
        }
    }
}

pub type Id = String;

#[derive(Debug, PartialEq)]
pub enum Exp {
    Lit(Lit),
    Var(Id),
    Fun(Id, Typ, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Add(Box<Exp>, Box<Exp>),
}

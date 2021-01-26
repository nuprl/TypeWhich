#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Int,
    Bool,
    Str,
    Arr(Box<Typ>, Box<Typ>),
    List(Box<Typ>),
    Pair(Box<Typ>, Box<Typ>),
    Any,
    Metavar(u32),
}

impl Typ {
    pub fn is_arr(&self) -> bool {
        match self {
            Typ::Arr(..) => true,
            _ => false,
        }
    }
    pub fn is_atom(&self) -> bool {
        match self {
            Typ::Int | Typ::Bool | Typ::Str | Typ::Any | Typ::Metavar(..) => false,
            Typ::Arr(..) | Typ::List(..) | Typ::Pair(..) => true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Int(i32),
    Bool(bool),
    Str(String),
}

impl Lit {
    pub fn typ(&self) -> Typ {
        match self {
            Lit::Int(_) => Typ::Int,
            Lit::Bool(_) => Typ::Bool,
            Lit::Str(_) => Typ::Str,
        }
    }
}

pub type Id = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Exp {
    Lit(Lit),
    Var(Id),
    Fun(Id, Typ, Box<Exp>),
    Fix(Id, Typ, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Let(Id, Typ, Box<Exp>, Box<Exp>),
    AddOverload(Box<Exp>, Box<Exp>),
    Add(Box<Exp>, Box<Exp>),
    Mul(Box<Exp>, Box<Exp>),
    IntEq(Box<Exp>, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Pair(Box<Exp>, Box<Exp>),
    Cons(Box<Exp>, Box<Exp>),
    Empty,
    IsEmpty(Box<Exp>),
    Head(Box<Exp>),
    Tail(Box<Exp>),
    /// Type tests
    IsBool(Box<Exp>),
    IsInt(Box<Exp>),
    IsString(Box<Exp>),
    IsList(Box<Exp>),
    IsFun(Box<Exp>),
    /// This `u32` is the index of a boolean metavariable. When `true`, the
    /// cast is needed. When `false`, it can be safely removed.
    MaybeToAny(u32, Box<Exp>),
    MaybeFromAny(u32, Box<Exp>),
    ToAny(Box<Exp>),
    FromAny(Box<Exp>),
}

impl Exp {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Exp::Lit(Lit::Int(0)))
    }

    pub fn is_app_like(&self) -> bool {
        match self {
            Exp::MaybeFromAny(_, e) | Exp::MaybeToAny(_, e) => e.is_app_like(),
            Exp::App(..)
            | Exp::Cons(..)
            | Exp::Head(..)
            | Exp::Tail(..)
            | Exp::IsBool(..)
            | Exp::IsInt(..)
            | Exp::IsString(..)
            | Exp::IsFun(..)
            | Exp::ToAny(..)
            | Exp::FromAny(..) => true,
            _ => false,
        }
    }
    pub fn is_fun_exp(&self) -> bool {
        match self {
            Exp::MaybeFromAny(_, e) | Exp::MaybeToAny(_, e) => e.is_fun_exp(),
            Exp::Fun(..) | Exp::Fix(..) | Exp::If(..) | Exp::Let(..) | Exp::Cons(..) => true,
            _ => false,
        }
    }
    pub fn is_add_or_looser(&self) -> bool {
        match self {
            Exp::MaybeFromAny(_, e) | Exp::MaybeToAny(_, e) => e.is_add_or_looser(),
            Exp::Add(..) => true,
            _ => self.is_fun_exp(),
        }
    }
    pub fn is_mul_or_looser(&self) -> bool {
        match self {
            Exp::MaybeFromAny(_, e) | Exp::MaybeToAny(_, e) => e.is_mul_or_looser(),
            Exp::Mul(..) => true,
            _ => self.is_add_or_looser(),
        }
    }

    pub fn is_atom(&self) -> bool {
        match self {
            Exp::MaybeFromAny(_, e) | Exp::MaybeToAny(_, e) => e.is_atom(),
            Exp::Lit(..) | Exp::Var(_) | Exp::Empty => true,
            _ => false,
        }
    }
}

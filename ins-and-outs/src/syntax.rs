#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Typ {
    Null,
    Arr(Box<Typ>, Box<Typ>),
    Any,
    Metavar(u32),
    /// if X is Metavar(n) then X? is MetavarArg(Metavar(n))
    /// if X is Metavar(n) then X?? is MetavarArg(MetavarArg(Metavar(n)))
    MetavarArg(Box<Typ>),
    /// if X is Metavar(n) then X! is MetavarRet(Metavar(n))
    MetavarRet(Box<Typ>),
}

impl Typ {
    pub fn is_arr(&self) -> bool {
        match self {
            Typ::Arr(..) => true,
            _ => false,
        }
    }
    pub fn is_atom(&self) -> bool {
        !self.is_arr()
    }
    pub fn is_metavar(&self) -> bool {
        match self {
            Typ::Metavar(..) | Typ::MetavarArg(..) | Typ::MetavarRet(..) => true,
            _ => false,
        }
    }
    /// t1 -> t2 --> t1
    /// X --> X?
    /// (as defined in the paper)
    pub fn get_arg(&self) -> Typ {
        match self {
            Typ::Arr(t1, _) => *t1.clone(),
            _ if self.is_metavar() => Typ::MetavarArg(Box::new(self.clone())),
            _ => panic!("cannot make arg type variable of non-arrow, non-metavar"),
        }
    }
    /// t1 -> t2 --> t2
    /// X --> X!
    /// (as defined in the paper)
    pub fn get_ret(&self) -> Typ {
        match self {
            Typ::Arr(_, t2) => *t2.clone(),
            _ if self.is_metavar() => Typ::MetavarRet(Box::new(self.clone())),
            _ => panic!("cannot make arg type variable of non-arrow, non-metavar"),
        }
    }
    /// self ≼ t
    pub fn dyn_consistent(&self, t: &Typ) -> bool {
        match (self, t) {
            (Typ::Null, _) | (Typ::Any, _) | (_, Typ::Any) | (Typ::Arr(..), Typ::Arr(..)) => true,
            _ => false,
        }
    }
    /// least upper bound of kinds. self and k must be kinds (which means not
    /// metavars, i think(?))
    pub fn least_upper_bound(&self, k: &Typ) -> Typ {
        assert!(!self.is_metavar());
        assert!(!k.is_metavar());
        match (self, k) {
            (Typ::Null, k) | (k, Typ::Null) => k.clone(),
            (Typ::Any, _) | (_, Typ::Any) => Typ::Any,
            (Typ::Arr(x_arg, x_ret), Typ::Arr(x_arg2, x_ret2))
                if x_arg == x_arg2 && x_ret == x_ret2 =>
            {
                self.clone()
            }
            _ => panic!(
                "somehow the paper thinks this covers our bases. tried to LUB:\n{} |_| {}",
                self, k
            ),
        }
    }
    /// ||T||x where T = self
    pub fn kind_of_typ_var(&self, x: &Typ) -> Typ {
        assert!(matches!(Typ::Metavar, x));
        match self {
            Typ::Null => Typ::Null,
            Typ::Any => Typ::Any,
            Typ::Arr(..) => Typ::Arr(Box::new(x.get_arg()), Box::new(x.get_ret())),
            Typ::Metavar(..) | Typ::MetavarArg(..) | Typ::MetavarRet(..) => {
                panic!("t must not be a metavar")
            }
        }
    }
}

pub type Id = String;

#[derive(PartialEq, Clone)]
pub enum Exp {
    Null,
    Var(Id),
    Assign(Id, Box<Exp>),
    Fun(Id, Typ, Box<Exp>, Typ),
    App(Box<Exp>, Box<Exp>),
    Coerce(Typ, Typ, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    // TODO(luna): objects
}

impl Exp {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Exp::Null)
    }

    pub fn is_app_like(&self) -> bool {
        if let Exp::App(..) = self {
            true
        } else {
            false
        }
    }
    pub fn is_fun_exp(&self) -> bool {
        match self {
            Exp::Fun(..) | Exp::If(..) => true,
            _ => false,
        }
    }
    pub fn is_atom(&self) -> bool {
        match self {
            Exp::Null | Exp::Var(..) => true,
            _ => false,
        }
    }
}

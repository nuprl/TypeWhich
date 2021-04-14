#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Typ {
    Null,
    Int,
    Bool,
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
        matches!(self, Typ::Arr(..))
    }
    #[allow(dead_code)]
    pub fn is_atom(&self) -> bool {
        !self.is_arr()
    }
    pub fn is_metavar(&self) -> bool {
        matches!(
            self,
            Typ::Metavar(..) | Typ::MetavarArg(..) | Typ::MetavarRet(..)
        )
    }
    /// Rastogi et al define:
    ///
    /// > Kinds for a type variable X, ranged over by K^X, are types of the
    /// > form null, X?->X!, [object stuff], or any
    ///
    /// Let's focus on X?->X!, which I consider a bit ambiguous. The definition
    /// says kinds *for a type variable X*, which should mean that X in this
    /// notation isn't an arbitrary notation, but rather refers specifically to a
    /// type variable described by X. So that would mean when we see K^X in the
    /// figures, it doesn't mean any non-metavar, it means non-metavars with a
    /// very particular property on arrows. Earlier versions of my
    /// implementation used the former. The introduction of the below function,
    /// which represents the stricter "with respect to X" sense, works to the same
    /// extent. Since it's stricter and we're dealing with infinite loop issues,
    /// I'll stick with this until it breaks something
    pub fn is_kind(&self, x: &Typ) -> bool {
        assert!(x.is_metavar());
        match self {
            _ if self.is_metavar() => false,
            Typ::Arr(t1, t2) if **t1 == x.get_arg() && **t2 == x.get_ret() => true,
            Typ::Arr(..) => false,
            _ => true,
        }
    }
    pub fn is_base(&self) -> bool {
        matches!(self, Typ::Any | Typ::Bool | Typ::Int | Typ::Null)
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
        matches!((self, t),
            (Typ::Null, _) | (Typ::Any, _) | (_, Typ::Any) | (Typ::Arr(..), Typ::Arr(..)))
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
            // the paper avoids having to do this by having only two
            // incompatible types: objects and arrows. their specified behavior is
            // arrow |_| obj = any. we generalize this to t1 |_| t2 where t1 !=
            // t2 = any
            _ if self == k => self.clone(),
            _ => Typ::Any,
        }
    }
    /// ||T||x where T = self
    pub fn kind_of_typ_var(&self, x: &Typ) -> Typ {
        assert!(x.is_metavar());
        match self {
            Typ::Null | Typ::Int | Typ::Bool => self.clone(),
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
pub enum Lit {
    Null,
    Int(i32),
    Bool(bool),
}
impl Lit {
    pub fn typ(&self) -> Typ {
        match self {
            Lit::Null => Typ::Null,
            Lit::Int(_) => Typ::Int,
            Lit::Bool(_) => Typ::Bool,
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Exp {
    Lit(Lit),
    Var(Id),
    #[allow(dead_code)]
    Assign(Id, Box<Exp>),
    Fun(Id, Typ, Box<Exp>, Typ),
    App(Box<Exp>, Box<Exp>),
    Coerce(Typ, Typ, Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Add(Box<Exp>, Box<Exp>),
    #[allow(dead_code)]
    Seq(Box<Exp>, Box<Exp>),
    // TODO(luna): objects
}

impl Exp {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Exp::Lit(Lit::Null))
    }

    pub fn is_app_like(&self) -> bool {
        matches!(self, Exp::App(..))
    }
    pub fn is_fun_exp(&self) -> bool {
        matches!(self, Exp::Fun(..) | Exp::If(..))
    }
    pub fn is_atom(&self) -> bool {
        matches!(self, Exp::Lit(..) | Exp::Var(..))
    }
}

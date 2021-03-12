#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Unit,
    Int,
    Float,
    Bool,
    Str,
    Arr(Box<Typ>, Box<Typ>),
    List(Box<Typ>),
    Pair(Box<Typ>, Box<Typ>),
    Box(Box<Typ>),
    Vect(Box<Typ>),
    Any,
    Metavar(u32),
}

impl Typ {
    pub fn arrs(typs: Vec<Typ>) -> Self {
        assert!(!typs.is_empty());

        if typs.len() == 1 {
            Typ::Arr(
                Box::new(Typ::Unit),
                Box::new(typs.into_iter().next().unwrap()),
            )
        } else {
            let mut typs = typs.into_iter().rev();
            let mut arr = typs.next().unwrap();
            for typ in typs {
                arr = Typ::Arr(Box::new(typ), Box::new(arr));
            }
            arr
        }
    }

    pub fn is_arr(&self) -> bool {
        matches!(self, Typ::Arr(..))
    }
    pub fn is_atom(&self) -> bool {
        match self {
            Typ::Unit
            | Typ::Int
            | Typ::Float
            | Typ::Bool
            | Typ::Str
            | Typ::Any
            | Typ::Metavar(..) => false,
            Typ::Arr(..) | Typ::List(..) | Typ::Pair(..) | Typ::Box(..) | Typ::Vect(..) => true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Int(i32),
    Bool(bool),
    Str(String),
    Unit,
}

impl Lit {
    pub fn typ(&self) -> Typ {
        match self {
            Lit::Int(_) => Typ::Int,
            Lit::Bool(_) => Typ::Bool,
            Lit::Str(_) => Typ::Str,
            Lit::Unit => Typ::Unit,
        }
    }
}

pub type Id = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Toplevel {
    Define(Id, Typ, Exp),
    Exp(Exp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Exp {
    Lit(Lit),
    Var(Id),
    Fun(Id, Typ, Box<Exp>),
    Fix(Id, Typ, Box<Exp>),
    App(Box<Exp>, Box<Exp>),
    Let(Id, Box<Exp>, Box<Exp>),
    LetRec(Vec<(Id, Typ, Exp)>, Box<Exp>),
    Ann(Box<Exp>, Typ),
    AddOverload(Box<Exp>, Box<Exp>),
    Add(Box<Exp>, Box<Exp>),
    Mul(Box<Exp>, Box<Exp>),
    IntEq(Box<Exp>, Box<Exp>),
    Not(Box<Exp>),
    If(Box<Exp>, Box<Exp>, Box<Exp>),
    Pair(Box<Exp>, Box<Exp>),
    Cons(Box<Exp>, Box<Exp>),
    // Γ ⊢ empty: T : List(T)
    Empty(Typ),
    IsEmpty(Box<Exp>),
    Head(Box<Exp>),
    Tail(Box<Exp>),
    Box(Box<Exp>),
    Unbox(Box<Exp>),
    BoxSet(Box<Exp>, Box<Exp>),
    /// Type tests
    IsBool(Box<Exp>),
    IsInt(Box<Exp>),
    IsString(Box<Exp>),
    IsList(Box<Exp>),
    IsFun(Box<Exp>),
    Coerce(Typ, Typ, Box<Exp>),
}

impl Exp {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Exp::Lit(Lit::Int(0)))
    }

    pub fn begin(exps: Vec<Exp>) -> Self {
        let num_exps = exps.len();

        if num_exps == 0 {
            Exp::Lit(Lit::Unit)
        } else if num_exps == 1 {
            exps.into_iter().next().unwrap()
        } else {
            let mut exps = exps.into_iter().rev();
            let mut res = exps.next().unwrap();
            let mut ctr = num_exps;
            for exp in exps {
                res = Exp::Let(
                    format!("__begin{}", ctr),
                    Box::new(exp),
                    Box::new(res),
                );
                ctr = ctr - 1;
            }
            res
        }
    }

    pub fn lets(bindings: Vec<(String, Option<Typ>, Exp)>, body: Exp) -> Self {
        let mut res = body;
        for (x,t,e) in bindings.into_iter().rev() {
            let e = match t {
                Some(t) => Exp::Ann(Box::new(e), t),
                None => e,
            };

            res = Exp::Let(x, Box::new(e), Box::new(res));
        }
        res
    }

    pub fn apps(exps: Vec<Exp>) -> Self {
        assert!(!exps.is_empty());

        if exps.len() == 1 {
            Exp::App(
                Box::new(exps.into_iter().next().unwrap()),
                Box::new(Exp::Lit(Lit::Unit)),
            )
        } else {
            let mut exps = exps.into_iter();
            let mut app = exps.next().unwrap();
            for arg in exps {
                app = Exp::App(Box::new(app), Box::new(arg));
            }
            app
        }
    }

    pub fn funs(args: Vec<(String, Typ)>, body: Self) -> Self
    {
        let mut fun = body;
        for (x, t) in args.into_iter().rev() {
            fun = Exp::Fun(x, t, Box::new(fun));
        }
        fun
    }

    pub fn is_app_like(&self) -> bool {
        matches!(
            self,
            Exp::App(..)
                | Exp::Cons(..)
                | Exp::Head(..)
                | Exp::Tail(..)
                | Exp::IsBool(..)
                | Exp::IsInt(..)
                | Exp::IsString(..)
                | Exp::IsFun(..)
        )
    }
    pub fn is_fun_exp(&self) -> bool {
        matches!(
            self,
            Exp::Fun(..) | Exp::Fix(..) | Exp::If(..) | Exp::Let(..) | Exp::Cons(..)
        )
    }
    pub fn is_add_or_looser(&self) -> bool {
        match self {
            Exp::Add(..) => true,
            _ => self.is_fun_exp(),
        }
    }
    pub fn is_mul_or_looser(&self) -> bool {
        match self {
            Exp::Mul(..) => true,
            _ => self.is_add_or_looser(),
        }
    }

    pub fn is_atom(&self) -> bool {
        matches!(self, Exp::Lit(..) | Exp::Var(..) | Exp::Empty(..))
    }
}

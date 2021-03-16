use crate::parser::next_metavar;

#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Unit,
    Int,
    Float,
    Bool,
    Str,
    Char,
    Arr(Box<Typ>, Box<Typ>),
    List(Box<Typ>),
    Pair(Box<Typ>, Box<Typ>),
    Box(Box<Typ>),
    Vect(Box<Typ>),
    Any,
    Metavar(u32),
}

impl Typ {
    /// Generates a right-associated function type
    ///
    /// `typs` must not be empty
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

    /// Generates a right-associated, unit-terminated tuple type
    pub fn tuples(typs: Vec<Typ>) -> Self {
        if typs.len() == 0 {
            Typ::Unit
        } else if typs.len() == 1 {
            Typ::Pair(
                Box::new(typs.into_iter().next().unwrap()),
                Box::new(Typ::Unit),
            )
        } else {
            let mut tup = Typ::Unit;
            for fst in typs.into_iter().rev() {
                tup = Typ::Pair(Box::new(fst), Box::new(tup));
            }
            tup
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
            | Typ::Char
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
    Float(f64),
    Char(char),
    Unit,
}

impl Lit {
    pub fn typ(&self) -> Typ {
        match self {
            Lit::Int(_) => Typ::Int,
            Lit::Bool(_) => Typ::Bool,
            Lit::Str(_) => Typ::Str,
            Lit::Float(_) => Typ::Float,
            Lit::Char(_) => Typ::Char,
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
    // pairs
    Pair(Box<Exp>, Box<Exp>),
    Fst(Box<Exp>),
    Snd(Box<Exp>),
    // lists
    Cons(Box<Exp>, Box<Exp>),
    // Γ ⊢ empty: T : List(T)
    Empty(Typ),
    IsEmpty(Box<Exp>),
    Head(Box<Exp>),
    Tail(Box<Exp>),
    // boxes
    Box(Box<Exp>),
    Unbox(Box<Exp>),
    BoxSet(Box<Exp>, Box<Exp>),
    // vectors
    /// size, initial value
    Vector(Box<Exp>, Box<Exp>),
    /// vector, index
    VectorRef(Box<Exp>, Box<Exp>),
    /// vector, index, value
    VectorSet(Box<Exp>, Box<Exp>, Box<Exp>),
    /// vector
    VectorLen(Box<Exp>), // built-in because we need the polymorphic type
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
            let mut ctr = num_exps - 1;
            for exp in exps {
                res = Exp::Let(format!("__begin{}", ctr), Box::new(exp), Box::new(res));
                ctr = ctr - 1;
            }
            res
        }
    }

    pub fn lets(bindings: Vec<(String, Option<Typ>, Exp)>, body: Exp) -> Self {
        let mut res = body;
        for (x, t, e) in bindings.into_iter().rev() {
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

    /// Generates a right-associated, unit-terminated pair (cf. `Typ::tuples`)
    ///
    /// Returns unit or the sole type itself when given 0 or 1 `typs`
    pub fn pairs(exps: Vec<Exp>) -> Self {
        if exps.len() == 0 {
            Exp::Lit(Lit::Unit)
        } else if exps.len() == 1 {
            Exp::Pair(
                Box::new(exps.into_iter().next().unwrap()),
                Box::new(Exp::Lit(Lit::Unit)),
            )
        } else {
            let mut tup = Exp::Lit(Lit::Unit);
            for fst in exps.into_iter().rev() {
                tup = Exp::Pair(Box::new(fst), Box::new(tup));
            }
            tup
        }
    }

    /// Generates a function that gets the `n`th element out of a right-associated, unit-terminated tuple
    pub fn proj(self, n: u32) -> Self {
        let mut n = n;
        let mut proj = self;
        while n > 0 {
            proj = Exp::Snd(Box::new(proj));
            n = n - 1;
        }
        Exp::Fst(Box::new(proj))
    }

    pub fn funs(args: Vec<(String, Typ)>, body: Self) -> Self {
        let mut fun = body;
        for (x, t) in args.into_iter().rev() {
            fun = Exp::Fun(x, t, Box::new(fun));
        }
        fun
    }

    pub fn switch(scrutinee: Exp, cases: Vec<(Vec<i32>, Exp)>, default: Exp) -> Self {
        let name = "__scrutinee".to_string(); // TODO(mmg): ensure freshness
        let x = Exp::Var(name.clone());

        let mut e = default;

        for (vals, then) in cases.into_iter().rev() {
            e = Exp::If(
                Box::new(Exp::one_of_ints(&x, vals)),
                Box::new(then),
                Box::new(e),
            );
        }

        Exp::Let(name, Box::new(scrutinee), Box::new(e))
    }

    fn one_of_ints(val: &Exp, ints: Vec<i32>) -> Self {
        assert!(ints.len() >= 1);

        let mut ints = ints.into_iter();
        let mut e = Exp::IntEq(
            Box::new(val.clone()),
            Box::new(Exp::Lit(Lit::Int(ints.next().unwrap()))),
        );
        for i in ints {
            e = Exp::or(
                e,
                Exp::IntEq(Box::new(val.clone()), Box::new(Exp::Lit(Lit::Int(i)))),
            );
        }

        e
    }

    fn or(l: Exp, r: Exp) -> Self {
        Exp::If(
            Box::new(l),
            Box::new(Exp::Lit(Lit::Bool(true))),
            Box::new(r),
        )
    }

    pub fn cond(cases: Vec<(Exp, Exp)>, default: Exp) -> Exp {
        let mut e = default;

        for (condition, branch) in cases.into_iter().rev() {
            e = Exp::If(
                Box::new(condition),
                Box::new(branch),
                Box::new(e),
            );
        }

        e
    }

    pub fn repeat(
        var: Id,
        lo: Exp,
        hi: Exp,
        acc: Id,
        acc_typ: Typ,
        acc_init: Exp,
        body: Exp,
    ) -> Exp {
        let loop_fun = format!("__loop_{}_{}", var, acc); // TODO(mmg): ensure freshness
        let loop_hi = format!("__loop_{}_{}_hi", var, acc);
        let index = Box::new(Exp::Var(var.clone()));

        let loop_body = Exp::If(
            Box::new(Exp::IntEq(
                index.clone(),
                Box::new(Exp::Var(loop_hi.clone())),
            )),
            // last loop
            Box::new(body.clone()),
            // body
            Box::new(Exp::apps(vec![
                Exp::Var(loop_fun.clone()),
                Exp::Add(index.clone(), Box::new(Exp::Lit(Lit::Int(1)))),
                body.clone(),
            ])),
        );

        Exp::LetRec(
            vec![
                (loop_hi, next_metavar(), hi),
                (
                    loop_fun.clone(),
                    next_metavar(),
                    Exp::funs(vec![(var, next_metavar()), (acc, acc_typ)], loop_body),
                ),
            ],
            Box::new(Exp::apps(vec![Exp::Var(loop_fun), lo, acc_init])),
        )
    }

    /// Replaces all type annotations with metavariables
    ///
    /// Removes `Exp::Ann` and `Exp::Coerce` nodes (but leaves in `Exp::Ann(e, Typ::Any))`)
    pub fn fresh_types(&mut self) {
        match self {
            Exp::Ann(e, Typ::Any) => {
                e.fresh_types();
            }
            Exp::Ann(e, _) | Exp::Coerce(_, _, e) => {
                e.fresh_types();
                *self = e.take();
            }
            Exp::Lit(_) | Exp::Var(_) => (),
            Exp::Empty(t) => *t = next_metavar(),
            Exp::Fun(_, t, e) | Exp::Fix(_, t, e) => {
                *t = next_metavar();
                e.fresh_types();
            }
            Exp::LetRec(bindings, e) => {
                for (_, ti, ei) in bindings.iter_mut() {
                    *ti = next_metavar();
                    ei.fresh_types();
                }
                e.fresh_types();
            }
            Exp::Not(e)
            | Exp::Fst(e)
            | Exp::Snd(e)
            | Exp::IsEmpty(e)
            | Exp::Head(e)
            | Exp::Tail(e)
            | Exp::Box(e)
            | Exp::Unbox(e)
            | Exp::IsBool(e)
            | Exp::IsInt(e)
            | Exp::IsString(e)
            | Exp::IsList(e)
            | Exp::IsFun(e) 
            | Exp::VectorLen(e) => e.fresh_types(),
            Exp::App(e1, e2)
            | Exp::Let(_, e1, e2)
            | Exp::AddOverload(e1, e2)
            | Exp::Add(e1, e2)
            | Exp::Mul(e1, e2)
            | Exp::IntEq(e1, e2)
            | Exp::Pair(e1, e2)
            | Exp::Cons(e1, e2)
            | Exp::BoxSet(e1, e2) 
            | Exp::Vector(e1, e2)
            | Exp::VectorRef(e1, e2) => {
                e1.fresh_types();
                e2.fresh_types();
            }
            Exp::If(e1, e2, e3) 
            | Exp::VectorSet(e1, e2, e3) => {
                e1.fresh_types();
                e2.fresh_types();
                e3.fresh_types();
            }
        };
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

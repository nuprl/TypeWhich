use crate::syntax::Typ;
use ast::{Bool, Dynamic};
use z3::*;

pub struct Z3State<'a> {
    pub cxt: &'a Context,
    pub typ: &'a DatatypeSort<'a>,
    pub typ_sort: &'a Sort<'a>,
    pub int_z3: Dynamic<'a>,
    pub bool_z3: Dynamic<'a>,
    pub str_z3: Dynamic<'a>,
    pub arr_ctor: &'a FuncDecl<'a>,
    pub list_ctor: &'a FuncDecl<'a>,
    pub pair_ctor: &'a FuncDecl<'a>,
    pub any_z3: Dynamic<'a>,
}

impl<'a> Z3State<'a> {
    pub fn new(cxt: &'a Context, typ: &'a DatatypeSort<'a>) -> Self {
        Z3State {
            cxt,
            int_z3: typ.variants[0].constructor.apply(&[]),
            bool_z3: typ.variants[1].constructor.apply(&[]),
            str_z3: typ.variants[2].constructor.apply(&[]),
            arr_ctor: &typ.variants[3].constructor,
            list_ctor: &typ.variants[4].constructor,
            pair_ctor: &typ.variants[5].constructor,
            any_z3: typ.variants[6].constructor.apply(&[]),
            typ_sort: &typ.sort,
            typ,
        }
    }
    pub fn typ(cxt: &'a Context) -> DatatypeSort<'a> {
        DatatypeBuilder::new(&cxt, "Typ")
            .variant("Int", vec![])
            .variant("Bool", vec![])
            .variant("Str", vec![])
            .variant(
                "Arr",
                vec![
                    ("arg", DatatypeAccessor::Datatype("Typ".into())),
                    ("ret", DatatypeAccessor::Datatype("Typ".into())),
                ],
            )
            .variant(
                "List",
                vec![("t", DatatypeAccessor::Datatype("Typ".into()))],
            )
            .variant(
                "Pair",
                vec![
                    ("t1", DatatypeAccessor::Datatype("Typ".into())),
                    ("t2", DatatypeAccessor::Datatype("Typ".into())),
                ],
            )
            .variant("Any", vec![])
            .finish()
    }
    pub fn true_z3(&self) -> Bool<'a> {
        Bool::from_bool(self.cxt, true)
    }
    pub fn z3_to_typ(&self, model: &'a Model, e: Dynamic) -> Typ {
        if self.is_int(model, &e) {
            Typ::Int
        } else if self.is_bool(model, &e) {
            Typ::Bool
        } else if self.is_str(model, &e) {
            Typ::Str
        } else if self.is_arr(model, &e) {
            let arg = self.arr_arg(&e);
            let arg = model.eval(&arg).unwrap();
            let ret = self.arr_ret(&e);
            let ret = model.eval(&ret).unwrap();
            let t1 = self.z3_to_typ(model, arg);
            let t2 = self.z3_to_typ(model, ret);
            Typ::Arr(Box::new(t1), Box::new(t2))
        } else if self.is_list(model, &e) {
            let t = self.list_typ(&e);
            let t = model.eval(&t).unwrap();
            let t = self.z3_to_typ(model, t);
            Typ::List(Box::new(t))
        } else if self.is_pair(model, &e) {
            let t1 = model.eval(&self.pair1(&e)).unwrap();
            let t2 = model.eval(&self.pair2(&e)).unwrap();
            let t1 = self.z3_to_typ(model, t1);
            let t2 = self.z3_to_typ(model, t2);
            Typ::Pair(Box::new(t1), Box::new(t2))
        } else if self.is_any(model, &e) {
            Typ::Any
        } else {
            panic!("missing case in z3_to_typ");
        }
    }

    fn is_variant(&self, i: usize, model: &Model, e: &Dynamic) -> bool {
        model
            .eval(&self.typ.variants[i].tester.apply(&[&e]).as_bool().unwrap())
            .unwrap()
            .as_bool()
            .unwrap()
    }
    pub fn z3_is_arr(&self, e: Dynamic<'a>) -> Bool<'a> {
        self.typ.variants[3].tester.apply(&[&e]).as_bool().unwrap()
    }
    pub fn z3_is_list(&self, e: Dynamic<'a>) -> Bool<'a> {
        self.typ.variants[4].tester.apply(&[&e]).as_bool().unwrap()
    }
    fn is_int(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(0, model, e)
    }
    fn is_bool(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(1, model, e)
    }
    fn is_str(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(2, model, e)
    }
    fn is_arr(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(3, model, e)
    }
    fn is_list(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(4, model, e)
    }
    fn is_pair(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(5, model, e)
    }
    fn is_any(&self, model: &Model, e: &Dynamic) -> bool {
        self.is_variant(6, model, e)
    }
    fn arr_arg(&self, e: &'a Dynamic) -> Dynamic {
        self.typ.variants[3].accessors[0].apply(&[e])
    }
    fn arr_ret(&self, e: &'a Dynamic) -> Dynamic {
        self.typ.variants[3].accessors[1].apply(&[e])
    }
    fn list_typ(&self, e: &'a Dynamic) -> Dynamic {
        self.typ.variants[4].accessors[0].apply(&[e])
    }
    fn pair1(&self, e: &'a Dynamic) -> Dynamic {
        self.typ.variants[5].accessors[0].apply(&[e])
    }
    fn pair2(&self, e: &'a Dynamic) -> Dynamic {
        self.typ.variants[5].accessors[1].apply(&[e])
    }
}

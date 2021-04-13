use super::syntax::*;
use super::Closure;
use crate::pretty::Pretty;

// Copied from jankscripten
#[macro_export]
macro_rules! impl_Display_Pretty2 {
    ($T:ty) => {
        impl std::fmt::Display for $T {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let pp = pretty::BoxAllocator;
                let doc = self.pretty::<_, ()>(&pp);
                doc.1.render_fmt($crate::pretty::DEFAULT_WIDTH, f)
            }
        }
    };
}

////////////////////////////////////////////////////////////////////////////////

const PRINT_COERCIONS: bool = false;

fn parens_if<'b, D, A, T>(pp: &'b D, d: &'b T, b: bool) -> pretty::DocBuilder<'b, D, A>
where
    T: Pretty,
    D: pretty::DocAllocator<'b, A>,
    A: std::clone::Clone,
    <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
{
    if b {
        pp.concat(vec![pp.text("("), d.pretty(pp), pp.text(")")])
    } else {
        d.pretty(pp)
    }
}

impl Pretty for Typ {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Typ::Null => pp.text("null"),
            Typ::Int => pp.text("int"),
            Typ::Bool => pp.text("bool"),
            Typ::Arr(t1, t2) => pretty_arr(t1, t2, pp),
            Typ::Any => pp.text("any"),
            Typ::Metavar(i) => pp.text(greek(*i)),
            Typ::MetavarArg(t) => t.pretty(pp).append(pp.text("?")),
            Typ::MetavarRet(t) => t.pretty(pp).append(pp.text("!")),
        }
    }
}

fn pretty_arr<'b, D, A>(t1: &'b Typ, t2: &'b Typ, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    A: std::clone::Clone,
    <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
{
    pp.concat(vec![
        parens_if(pp, &*t1, t1.is_arr()),
        pp.space(),
        pp.text("->"),
        pp.space(),
        t2.pretty(pp),
    ])
}

/// produces lowercase greek letters in alphabetic order, then produced <i>
/// where i begins at 1 after the greek characters
fn greek(i: u32) -> String {
    let num_greek_chars = 78;
    if i <= num_greek_chars {
        // SAFETY:
        // - a char is a u32 if the u32 is a valid Unicode codepoint
        // - all characters between 0x03b1 and 0x03ff are greek characters
        // - if i is 78, total is 0x03ff, if i is 0, it is 0x03b1
        // - so all integers produced by this path are Unicode code points
        // 0x03b1 is α
        std::char::from_u32(0x03b1 + i).unwrap().to_string()
    } else {
        format!("⦉{}⦊", i - num_greek_chars)
    }
}

impl Pretty for Exp {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Exp::Lit(x) => x.pretty(pp),
            Exp::Var(x) => pp.text(x),
            Exp::Assign(x, v) => pp.concat(vec![pp.text(x), pp.text(": "), v.pretty(pp)]),
            Exp::Fun(x, t1, e, t2) if super::DEBUG => pp.concat(vec![
                t2.pretty(pp).parens(),
                pp.space(),
                pp.text("fun "),
                pp.text(x),
                pp.text(":"),
                t1.pretty(pp),
                pp.text("."),
                pp.softline(),
                e.pretty(pp).nest(2),
            ]),
            Exp::Fun(x, t1, e, _) => pp.concat(vec![
                pp.text("fun "),
                pp.text(x),
                pp.text(":"),
                t1.pretty(pp),
                pp.text("."),
                pp.softline(),
                e.pretty(pp).nest(2),
            ]),
            Exp::App(e1, e2) => pp.concat(vec![
                parens_if(pp, &**e1, e1.is_fun_exp()),
                pp.space(),
                parens_if(pp, &**e2, !e2.is_atom()),
            ]),
            Exp::If(e1, e2, e3) => pp.concat(vec![
                pp.text("if"),
                pp.space(),
                e1.pretty(pp).nest(2),
                pp.softline(),
                pp.concat(vec![pp.text("then"), pp.softline(), e2.pretty(pp)])
                    .nest(2),
                pp.softline(),
                pp.concat(vec![pp.text("else"), pp.softline(), e3.pretty(pp)])
                    .nest(2),
            ]),
            Exp::Add(e1, e2) => pp.concat(vec![
                parens_if(pp, &**e1, !e1.is_atom()),
                pp.text(" + "),
                parens_if(pp, &**e2, !e2.is_atom()),
            ]),
            Exp::Coerce(t1, t2, e) if PRINT_COERCIONS => pp.concat(vec![
                pp.text("⟨"),
                pretty_coercion(t1, t2, pp),
                pp.text("⟩"),
                pp.space(),
                parens_if(pp, &**e, e.is_app_like()),
            ]),
            Exp::Coerce(_, _, e) => e.pretty(pp),
            Exp::Seq(e1, e2) => pp.concat(vec![
                e1.pretty(pp),
                pp.text(";"),
                pp.softline(),
                e2.pretty(pp),
            ]),
        }
    }
}

pub fn pretty_coercion<'b, D, A>(
    t1: &'b Typ,
    t2: &'b Typ,
    pp: &'b D,
) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    A: std::clone::Clone,
    <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
{
    pp.concat(vec![
        t1.pretty(pp),
        pp.space(),
        pp.text("▷"),
        pp.space(),
        t2.pretty(pp),
    ])
}

impl Pretty for Lit {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Lit::Null => pp.text("null"),
            Lit::Int(n) => pp.as_string(n),
            Lit::Bool(true) => pp.text("true"),
            Lit::Bool(false) => pp.text("false"),
        }
    }
}

impl Pretty for (Typ, Typ) {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            self.0.pretty(pp),
            pp.space(),
            pp.text("▷"),
            pp.space(),
            self.1.pretty(pp),
        ])
    }
}

impl Pretty for Closure {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.intersperse(
            self.iter().map(|(t1, t2)| pretty_coercion(t1, t2, pp)),
            pp.text(",").append(pp.softline()),
        )
    }
}

pub struct DisplayClosure<'a>(pub &'a Closure);
impl Pretty for DisplayClosure<'_> {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        self.0.pretty(pp)
    }
}

impl_Display_Pretty2!(Typ);
impl_Display_Pretty2!(Exp);
impl_Display_Pretty2!(DisplayClosure<'_>);

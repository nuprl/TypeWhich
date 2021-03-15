%start program
%%

program -> Vec<Toplevel> :
    tl { let mut v = Vec::new(); v.push($1); v }
  | program tl { let mut v = $1; v.push($2); v }
;

tl -> Toplevel :
    exp { Toplevel::Exp($1) }
  | '(' 'define' id         exp ')' { Toplevel::Define($3, next_metavar(), $4) }  
  | '(' 'define' id ':' typ exp ')' { Toplevel::Define($3, $5, $6) }  
  | '(' 'define' '(' id         ')'         exps ')' { Toplevel::Define($4, next_metavar(), Exp::Fun("__ignored".to_string(), Typ::Unit, Box::new(Exp::begin($6)))) }
  | '(' 'define' '(' id formals ')'         exps ')' { Toplevel::Define($4, next_metavar(), Exp::funs($5, Exp::begin($7))) } 
  | '(' 'define' '(' id formals ')' ':' typ exps ')' { Toplevel::Define($4, next_metavar(), Exp::funs($5, Exp::Ann(Box::new(Exp::begin($9)), $8))) } 
;

exps -> Vec<Exp> :
      exps exp { let mut v = $1; v.push($2); v }
    | exp      { let mut v = Vec::new(); v.push($1); v }
;

exp -> Exp :
      lit { Exp::Lit($1) }
    | id  { Exp::Var($1) }

    | '(' ':' exp typ ')' { Exp::Ann(Box::new($3), $4) }

    | '(' 'let'    '(' bindings ')' exps ')' { Exp::lets($4, Exp::begin($6)) }
    | '(' 'letrec' '(' bindings ')' exps ')' { 
      Exp::LetRec(
        $4.into_iter().map(|(x,to,e)| (x, to.unwrap_or_else(|| next_metavar()), e)).collect(), 
        Box::new(Exp::begin($6)),
      )
    }

    | '(' 'lambda' '(' formals ')' ':' typ exps ')' { Exp::funs($4, Exp::Ann(Box::new(Exp::begin($8)), $7)) }
    | '(' 'lambda' '(' formals ')'         exps ')' { Exp::funs($4, Exp::begin($6)) }

    | '(' 'repeat' '(' id exp exp ')' '(' id ':' typ exp ')' exp ')' { unimplemented!("repeat") }
//    | '(' 'repeat' '(' id exp exp ')' '(' id         exp ')' exp ')' { unimplemented!("repeat") } // grr shift/reduce conflict
    | '(' 'repeat' '(' id exp exp ')' exp ')' { unimplemented!("repeat") }

    | '(' 'if' exp exp exp ')' { Exp::If(Box::new($3), Box::new($4), Box::new($5)) }

    | '(' 'begin' exps ')' { Exp::begin($3) }

    | '(' '+' exp exp ')' { Exp::Add(Box::new($3), Box::new($4)) }
    | '(' '*' exp exp ')' { Exp::Mul(Box::new($3), Box::new($4)) }
    | '(' '=' exp exp ')' { Exp::IntEq(Box::new($3), Box::new($4)) }

    | '(' 'make-tuple' exps ')'    { Exp::pairs($3) }
    | '(' 'tuple-proj' exp pos ')' { let e = $3; e.proj($4) }

    | '(' 'box'   exp ')'      { Exp::Box(Box::new($3)) }
    | '(' 'unbox' exp ')'      { Exp::Unbox(Box::new($3)) }
    | '(' 'boxset' exp exp ')' { Exp::BoxSet(Box::new($3), Box::new($4)) }

    | '(' exps ')' { Exp::apps($2) }
;

bindings -> Vec<(String, Option<Typ>, Exp)> :
      bindings binding { let mut v = $1; v.push($2); v }
    | binding          { let mut v = Vec::new(); v.push($1); v }
;

binding -> (String, Option<Typ>, Exp) :
      '(' id ':' typ exp ')' { ($2, Some($4), $5) }
    | '(' id         exp ')' { ($2, None, $3) }
;

formals -> Vec<(String, Typ)> :
     formals formal { let mut v = $1; v.push($2); v } 
   |         formal { let mut v = Vec::new(); v.push($1); v }
;

formal -> (String, Typ) :
        id             { ($1, next_metavar()) }
  | '(' id ':' typ ')' { ($2, $4) }
;

typs -> Vec<Typ> :
    typs typ  { let mut v = $1; v.push($2); v }
  | typ       { let mut v = Vec::new(); v.push($1); v }
  ;

typ -> Typ :
    '(' '->' typs ')'    { Typ::arrs($3) } 
  | '(' typ '->' typ ')' { Typ::Arr(Box::new($2), Box::new($4)) } 
  | '(' 'List' typ ')'   { Typ::List(Box::new($3)) }
  | '(' 'Ref' typ ')'    { Typ::Box(Box::new($3)) }
  | '(' 'Vect' typ ')'   { Typ::Vect(Box::new($3)) }
  | '(' 'Tuple' typs ')' { Typ::tuples($3) } 
  | 'Dyn'                { Typ::Any }
  | 'Int'                { Typ::Int }
  | 'Float'              { Typ::Float }
  | 'Bool'               { Typ::Bool }
  | id                   { unimplemented!("type variable") }
  | '(' 'Rec' id typ ')' { unimplemented!("recursive types") }
  ;

lit -> Lit :
    i32  { Lit::Int($1) }
  | f64  { Lit::Float($1) }
  | bool { Lit::Bool($1) }
  | str  { Lit::Str($1) }
  | '()' { Lit::Unit }
  ;

f64 -> f64 :
    'FLOscheme' { 
      let span = $1.unwrap().span();
      let str = $lexer.span_str(lrpar::Span::new(span.start() + 2, span.end()));
      str.parse::<f64>().unwrap()
    }
  | 'FLO' { $lexer.span_str($1.unwrap().span()).parse::<f64>().unwrap() }
  ;

i32 -> i32 :
    'NUM' { $lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() }
    ;

// WEIRD error trying to use the lexer to just not allow `-` at the front...
// Err(Lexeme { start: 14, len: 4294967295, tok_id: 32 })
pos -> u32 :
    'NUM' { $lexer.span_str($1.unwrap().span()).parse::<u32>().unwrap_or_else(|err| panic!("tuple indices must be non-negative: {:?}", err)) }
    ;

bool -> bool :
    'true'  { true }
  | 'false' { false }
  ;

str -> String :
    'STR' { let mut s = $lexer.span_str($1.unwrap().span()).to_string(); s.remove(0); s.pop(); s }
  ;

id -> String :
    'ID' { $lexer.span_str($1.unwrap().span()).to_string() }
  ;

%%

use crate::syntax::{Toplevel, Exp, Lit, Typ};
use crate::parser::next_metavar;
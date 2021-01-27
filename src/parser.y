%start exp
%%

i32 -> i32 :
    'INT' { $lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() }
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

lit -> Lit :
    i32  { Lit::Int($1) }
  | bool { Lit::Bool($1) }
  | str { Lit::Str($1) }
  ;

typ_atom -> Typ :
    'any'       { Typ::Any }
  | 'int_typ'   { Typ::Int }
  | 'bool'      { Typ::Bool }
  | '(' typ ')' { $2 }
  ;

typ -> Typ :
    typ_atom '->' typ { Typ::Arr(Box::new($1), Box::new($3)) }
  | typ_atom          { $1 }
  ;

atom -> Exp :
    '(' exp ')' { $2 }
  | lit         { Exp::Lit($1) }
  | id          { Exp::Var($1) }
  | 'empty'     { Exp::Empty   }
  ;

funExp -> Exp :
    funExp atom { app_($1, $2) }
  | 'head' atom { Exp::Head(maybe_from_any_($2)) }
  | 'tail' atom { Exp::Tail(maybe_from_any_($2)) }
  | 'is_empty' atom { Exp::IsEmpty(maybe_from_any_($2)) }
  | 'is_bool' atom { Exp::IsBool(maybe_to_any_($2)) }
  | 'is_int' atom { Exp::IsInt(maybe_to_any_($2)) }
  | 'is_string' atom { Exp::IsString(maybe_to_any_($2)) }
  | 'is_list' atom { Exp::IsList(maybe_to_any_($2)) }
  | 'is_fun' atom { Exp::IsFun(maybe_to_any_($2)) }
  | atom        { $1 }
  ;

mul -> Exp :
    mul '*' funExp { Exp::Mul(maybe_from_any_($1), maybe_from_any_($3)) }
  | 'not' funExp   { Exp::Not(maybe_from_any_($2)) }
  | funExp         { $1 }
  ;

add -> Exp :
    add '+' mul  { Exp::Add(maybe_from_any_($1), maybe_from_any_($3)) }
  | add '+?' mul { Exp::AddOverload(maybe_to_any_($1), maybe_to_any_($3)) }
  | mul          { $1 }
  ;

pair -> Exp :
    pair ',' add { Exp::Pair(Box::new($1), Box::new($3)) }
  | add          { $1 }
  ;

exp -> Exp :
    'fun' id '.' exp { Exp::Fun($2, next_metavar_typ(), Box::new($4)) }
  | 'fun' id ':' typ '.' exp { Exp::Fun($2, $4, Box::new($6)) }
  | 'fix' id '.' exp { Exp::Fix($2, next_metavar_typ(), Box::new($4)) }
  | pair             { $1 }
  | 'if' exp 'then' exp 'else' exp {
        Exp::If(maybe_from_any_($2), maybe_to_any_($4), maybe_to_any_($6))
    }
  | 'let' id '=' exp 'in' exp {
      Exp::Let($2, maybe_from_any_($4), Box::new($6))
    }
  | pair '::' exp    { Exp::Cons(maybe_to_any_($1), maybe_from_any_($3)) }
  ;

%%

fn app_(e1: Exp, e2: Exp) -> Exp {
    Exp::App(maybe_from_any_(e1), maybe_to_any_(e2))
}
fn maybe_to_any_(e: Exp) -> Box<Exp> {
    Box::new(Exp::MaybeToAny(next_metavar(), Box::new(e)))
}
fn maybe_from_any_(e: Exp) -> Box<Exp> {
    Box::new(Exp::MaybeFromAny(next_metavar(), Box::new(e)))
}

use super::syntax::{Exp, Lit, Typ};
use super::parser::{next_metavar, next_metavar_typ};

%start exp
%%

i32 -> i32 :
    'INT' { $lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() }
    ;

bool -> bool :
    'true'  { true }
  | 'false' { false }
  ;

id -> String :
    'ID' { $lexer.span_str($1.unwrap().span()).to_string() }
  ;

typ_atom -> Typ :
    'any'       { Typ::Any }
  | 'null'      { Typ::Null }
  | 'int_typ'   { Typ::Int }
  | 'bool_typ'  { Typ::Bool }
  | '(' typ ')' { $2 }
  ;

typ -> Typ :
    typ_atom '->' typ { Typ::Arr(Box::new($1), Box::new($3)) }
  | typ_atom          { $1 }
  ;

lit -> Lit :
    'null' { Lit::Null }
  | i32    { Lit::Int($1) }
  | bool   { Lit::Bool($1) }
  ;

atom -> Exp :
    '(' exp ')' { $2 }
  | lit         { Exp::Lit($1) }
  | id          { Exp::Var($1) }
  ;

funExp -> Exp :
    funExp atom   { app_($1, $2) }
  | atom          { $1 }
  ;

plus -> Exp :
    plus '+' funExp { Exp::Add(Box::new($1), Box::new($3)) }
  | funExp          { $1 }
  ;

assignExp -> Exp :
    id '=' assignExp { Exp::Assign($1, Box::new($3)) }
  | plus             { $1 }
  ;

exp -> Exp :
    'fun' id '.' exp { Exp::Fun($2, next_metavar_typ(), Box::new($4), next_metavar_typ()) }
  | 'fun' id ':' typ '.' exp { Exp::Fun($2, $4, Box::new($6), next_metavar_typ()) }
  | 'if' exp 'then' exp 'else' exp {
        Exp::If(Box::new($2), Box::new($4), Box::new($6))
    }
  | 'let' id '=' exp 'in' exp {
      app_(Exp::Fun($2, next_metavar_typ(), Box::new($6), next_metavar_typ()), $4)
    }
  | assignExp { $1 }
  | assignExp ';' exp { Exp::Seq(Box::new($1), Box::new($3)) }
  ;

%%

fn app_(e1: Exp, e2: Exp) -> Exp {
    Exp::App(Box::new(e1), Box::new(e2))
}

use super::syntax::*;
use super::parser::next_metavar_typ;

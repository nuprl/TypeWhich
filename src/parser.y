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

lit -> Lit :
    i32  { Lit::Int($1) }
  | bool { Lit::Bool($1) }
  ;

atom -> Exp :
    '(' exp ')' { $2 }
  | lit         { Exp::Lit($1) }
  | id          { Exp::Var($1) }
  ;

add -> Exp :
    add '+' atom { Exp::Add(Box::new($1), Box::new($3)) }
  | atom         { $1 }
  ;

funExp -> Exp :
    funExp add { Exp::App(Box::new($1), Box::new($2)) }
  | add        { $1 }
  ;

exp -> Exp :
    'fun' id '.' exp { Exp::Fun($2, next_metavar(), Box::new($4)) }
  | funExp          { $1 }
  ;

%%

use super::syntax::{Exp,Lit};
use super::parser::next_metavar;
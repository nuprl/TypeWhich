%start program
%%

program -> Vec<Toplevel> :
    tl { let mut v = Vec::new(); v.push($1); v }
  | program tl { let mut v = $1; v.push($2); v }
;

tl -> Toplevel :
    exp { Toplevel::Exp($1) }
  | '(' 'define' id exp ')' { Toplevel::Define($3, next_metavar(), $4) }  
  | '(' 'define' '(' id ')' exp ')' { Toplevel::Define($4, next_metavar(), Exp::Fun("__ignored".to_string(), Typ::Unit, Box::new($6))) }
  | '(' 'define' '(' id formals ')' exp ')' { let mut e = $7; e.into_fun_with_args($5); Toplevel::Define($4, next_metavar(), e) } 
  | '(' 'define' '(' id formals ')' '[' ':' typ ']' exp ')' { let mut e = $11; e.into_fun_with_args($5); Toplevel::Define($4, next_metavar(), Exp::Ann(Box::new(e), $9)) } 
;

exp -> Exp :
      lit { Exp::Lit($1) }
    | id  { Exp::Var($1) }
;

formals -> Vec<(String, Typ)> :
     formals formal { let mut v = $1; v.push($2); v } 
   |         formal { let mut v = Vec::new(); v.push($1); v }
;

formal -> (String, Typ) :
    id                    { ($1, next_metavar()) }
  | '[' id ':' typ ']'    { ($2, $4) }
;

typ_list -> Vec<Typ> :
    typ_list typ { let mut v = $1; v.push($2); v }
  | typ          { let mut v = Vec::new(); v.push($1); v }
  ;

typ -> Typ :
    '(' '->' typ_list ')'     { Typ::arrs($3) } 
  | '(' 'List' typ ')'        { Typ::List(Box::new($3)) }
  | '(' 'Ref' typ ')'         { Typ::Box(Box::new($3)) }
  | '(' 'Vect' typ ')'        { Typ::Vect(Box::new($3)) }
  | '(' 'Tuple' typ_list ')'  { unimplemented!() } 
  | 'Dyn'       { Typ::Any }
  | 'Int'       { Typ::Int }
  | 'Float'     { Typ::Float }
  | 'Bool'      { Typ::Bool }
  ;

lit -> Lit :
    i32  { Lit::Int($1) }
  | bool { Lit::Bool($1) }
  | str { Lit::Str($1) }
  ;

i32 -> i32 :
    'NUM' { $lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() }
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

use super::syntax::{Toplevel, Exp, Lit, Typ};
use super::parser::next_metavar;
%start tl
%%

tl -> Exp :
    exp { $1 }
  | '(' 'define' id exp ')' tl { unimplemented!() }  
  | '(' 'define' '(' id ')' exp ')' tl { unimplemented!() }
//  | '(' 'define' '(' id formals ')' exp ')' tl { unimplemented!() /* TODO(mmg): reverse formals */ } 
//  | '(' 'define' '(' id formals ')' '[' ':' typ ']' exp ')' tl { unimplemented!() /* TODO(mmg): reverse formals */ } 
;

exp -> Exp :
      lit { Exp::Lit($1) }
    | id  { Exp::Var($1) }
;

formals -> Vec<(String, Typ)> :
   formal formals {let mut v = $2; v.push($1); // NB done in reverse, flipped
    v
  } | formal { let mut v = Vec::new(); v.push($1); v }
;

formal -> (String, Typ) :
    id                    { ($1, next_metavar()) }
  | '[' id ':' typ ']'    { ($2, $4) }
;

typ_list -> Vec<Typ> :
    typ typ_list { let mut v = $2; v.push($1); v }
  | typ          { let mut v = Vec::new(); v.push($1); v }
  ;

typ -> Typ :
    '(' '->' typ typ_list ')' { unimplemented!() } // TODO(mmg): reverse typ_list
  | '(' 'List' typ ')'        { unimplemented!() }
  | '(' 'Ref' typ ')'         { unimplemented!() }
  | '(' 'Vect' typ ')'        { unimplemented!() }
  | '(' 'Tuple' typ_list ')'  { unimplemented!() } // TODO(mmg): reverse typ_list
  | 'Dyn'       { Typ::Any }
  | 'Typ_Int'   { Typ::Int }
  | 'Float'     { unimplemented!() }
  | 'Bool'      { Typ::Bool }
  ;

lit -> Lit :
    i32  { Lit::Int($1) }
  | bool { Lit::Bool($1) }
  | str { Lit::Str($1) }
  ;

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

%%

use super::syntax::{Exp, Lit, Typ};
use super::parser::next_metavar;
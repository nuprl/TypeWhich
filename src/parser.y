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
  | '(' ')' { Lit::Unit }
  ;

typ_atom -> Typ :
    'any'       { Typ::Any }
  | 'int_typ'   { Typ::Int }
  | 'bool'      { Typ::Bool }
  | '(' typ ')' { $2 }
  ;

typ_list -> Typ :
    'list' typ_list   { Typ::List(Box::new($2)) }
  | typ_atom          { $1 }
  ;

typ -> Typ :
    typ_list '->' typ { Typ::Arr(Box::new($1), Box::new($3)) }
  | typ_list          { $1 }
  ;

atom -> Exp :
    '(' exp ')'     { $2 }
  | lit             { Exp::Lit($1) }
  | id              { Exp::Var($1) }
  | 'empty'         { Exp::Empty(next_metavar()) }
//  | 'empty' ':' typ { Exp::Empty($3) }
  ;

funExp -> Exp :
    funExp atom { Exp::App(Box::new($1), Box::new($2)) }
  | 'head' atom { Exp::Head(Box::new($2)) }
  | 'tail' atom { Exp::Tail(Box::new($2)) }
  | 'is_empty' atom { Exp::IsEmpty(Box::new($2)) }
  | 'is_bool' atom { Exp::IsBool(Box::new($2)) }
  | 'is_int' atom { Exp::IsInt(Box::new($2)) }
  | 'is_string' atom { Exp::IsString(Box::new($2)) }
  | 'is_list' atom { Exp::IsList(Box::new($2)) }
  | 'is_fun' atom { Exp::IsFun(Box::new($2)) }
  | 'to_any' atom { Exp::Coerce(next_metavar(), Typ::Any, Box::new($2)) }
  | 'from_any' ':' typ atom { Exp::Coerce(Typ::Any, $3, Box::new($4)) }
  | 'from_any' atom { Exp::Coerce(Typ::Any, next_metavar(), Box::new($2)) }
  | atom        { $1 }
  ;

mul -> Exp :
    mul '*' funExp { Exp::Mul(Box::new($1), Box::new($3)) }
  | 'not' funExp   { Exp::Not(Box::new($2)) }
  | funExp         { $1 }
  ;

add -> Exp :
    add '+' mul  { Exp::Add(Box::new($1), Box::new($3)) }
  | add '+?' mul { Exp::AddOverload(Box::new($1), Box::new($3)) }
  | mul          { $1 }
  ;

pair -> Exp :
    pair ',' add { Exp::Pair(Box::new($1), Box::new($3)) }
  | pair '=' add { Exp::IntEq(Box::new($1), Box::new($3)) }
  | add          { $1 }
  ;

exp -> Exp :
    'fun' id '.' exp { Exp::Fun($2, Typ::Any, Box::new($4)) }
  | 'fun' id ':' typ '.' exp { Exp::Fun($2, $4, Box::new($6)) }
  | 'fix' id '.' exp { Exp::Fix($2, next_metavar(), Box::new($4)) }
  | pair             { $1 }
  | 'if' exp 'then' exp 'else' exp {
        Exp::If(Box::new($2), Box::new($4), Box::new($6))
    }
  | 'let' id '=' exp 'in' exp { Exp::Let($2, Box::new($4), Box::new($6)) }
  | 'let' 'rec' bindings 'in' exp { Exp::LetRec($3, Box::new($5)) }
  | pair '::' exp { Exp::Cons(Box::new($1), Box::new($3)) }
  | pair ':' typ  { Exp::Ann(Box::new($1), $3) }
  ;

bindings -> Vec<(String, Typ, Exp)> :
    bindings 'and' binding { let mut v = $1; v.push($3); v }
  |                binding { let mut v = Vec::new(); v.push($1); v }
;

binding -> (String, Typ, Exp) :
    id ':' typ '=' exp    { ($1, $3, $5) }
  | id '=' exp            { ($1, next_metavar(), $3) }
;
%%

use crate::syntax::{Exp, Lit, Typ};
use crate::parser::next_metavar;

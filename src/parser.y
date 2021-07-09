%start exp
%%

i32 -> R<i32> :
    'INT' { Ok($lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap()) }
    ;

bool -> R<bool> :
    'true'  { Ok(true) }
  | 'false' { Ok(false) }
  ;

str -> R<String> :
    'STR' { let mut s = $lexer.span_str($1.unwrap().span()).to_string(); s.remove(0); s.pop(); Ok(s) }
  ;

id -> R<String> :
    'ID' { Ok($lexer.span_str($1.unwrap().span()).to_string()) }
  ;

lit -> R<Lit> :
    i32  { Ok(Lit::Int($1?)) }
  | bool { Ok(Lit::Bool($1?)) }
  | str { Ok(Lit::Str($1?)) }
  | '(' ')' { Ok(Lit::Unit) }
  ;

typ_atom -> R<Typ> :
    'any'       { Ok(Typ::Any) }
  | 'int_typ'   { Ok(Typ::Int) }
  | ID          { Ok(Typ::Any) }
  | 'bool'      { Ok(Typ::Bool) }
  | 'vect' typ_atom { Ok(Typ::Vect(Box::new($2?))) }
  | '(' typ ')' { $2 }
  ;

typ_list -> R<Typ> :
    'list' typ_list   { Ok(Typ::List(Box::new($2?))) }
  | typ_atom          { $1 }
  ;

typ -> R<Typ> :
    typ_list '->' typ { Ok(Typ::Arr(Box::new($1?), Box::new($3?))) }
  | typ_list          { $1 }
  ;

atom -> R<Exp> :
    '(' exp ')'     { $2 }
  | lit             { Ok(Exp::Lit($1?)) }
  | id              { Ok(Exp::Var($1?)) }
  | 'empty'         { Ok(Exp::Empty(next_metavar())) }
//  | 'empty' ':' typ { Exp::Empty($3) }
  ;

funExp -> R<Exp> :
    funExp atom { Ok(Exp::App(Box::new($1?), Box::new($2?))) }
  | 'head' atom { Ok(Exp::Head(Box::new($2?))) }
  | 'tail' atom { Ok(Exp::Tail(Box::new($2?))) }
  | 'is_empty' atom { Ok(Exp::IsEmpty(Box::new($2?))) }
  | 'is_bool' atom { Ok(Exp::IsBool(Box::new($2?))) }
  | 'is_int' atom { Ok(Exp::IsInt(Box::new($2?))) }
  | 'is_string' atom { Ok(Exp::IsString(Box::new($2?))) }
  | 'is_list' atom { Ok(Exp::IsList(Box::new($2?))) }
  | 'is_fun' atom { Ok(Exp::IsFun(Box::new($2?))) }
  | 'to_any' atom { Ok(Exp::Coerce(next_metavar(), Typ::Any, Box::new($2?))) }
  | 'from_any' ':' typ atom { Ok(Exp::Coerce(Typ::Any, $3?, Box::new($4?))) }
  | 'from_any' atom { Ok(Exp::Coerce(Typ::Any, next_metavar(), Box::new($2?))) }
  | atom        { $1 }
  ;

mul -> R<Exp> :
    mul '*' funExp { Ok(Exp::BinaryOp(BinOp::IntMul, Box::new($1?), Box::new($3?))) }
  | 'not' funExp   { Ok(Exp::UnaryOp(UnOp::Not, Box::new($2?))) }
  | funExp         { $1 }
  ;

add -> R<Exp> :
    add '+' mul  { Ok(Exp::BinaryOp(BinOp::IntAdd, Box::new($1?), Box::new($3?))) }
  | add '+?' mul { Ok(Exp::AddOverload(Box::new($1?), Box::new($3?))) }
  | mul          { $1 }
  ;

pair -> R<Exp> :
    pair ',' add { Ok(Exp::Pair(Box::new($1?), Box::new($3?))) }
  | pair '=' add { Ok(Exp::BinaryOp(BinOp::IntEq, Box::new($1?), Box::new($3?))) }
  | add          { $1 }
  ;

exp -> R<Exp> :
    'fun' id '.' exp { Ok(Exp::Fun($2?, Typ::Any, Box::new($4?))) }
  | 'fun' id ':' typ '.' exp { Ok(Exp::Fun($2?, $4?, Box::new($6?))) }
  | 'fix' id '.' exp { Ok(Exp::Fix($2?, next_metavar(), Box::new($4?))) }
  | pair             { $1 }
  | 'if' exp 'then' exp 'else' exp {
        Ok(Exp::If(Box::new($2?), Box::new($4?), Box::new($6?)))
    }
  | 'let' id '=' exp 'in' exp { Ok(Exp::Let($2?, Box::new($4?), Box::new($6?))) }
  | 'let' 'rec' bindings 'in' exp { Ok(Exp::LetRec($3?, Box::new($5?))) }
  | pair '::' exp { Ok(Exp::Cons(Box::new($1?), Box::new($3?))) }
  | pair ':' typ  { Ok(Exp::Ann(Box::new($1?), $3?)) }
  ;

bindings -> R<Vec<(String, Typ, Exp)>> :
    bindings 'and' binding { let mut v = $1?; v.push($3?); Ok(v) }
  |                binding { let mut v = Vec::new(); v.push($1?); Ok(v) }
;

binding -> R<(String, Typ, Exp)> :
    id ':' typ '=' exp    { Ok(($1?, $3?, $5?)) }
  | id '=' exp            { Ok(($1?, next_metavar(), $3?)) }
;
%%

use crate::syntax::{Exp, Lit, Typ, BinOp, UnOp};
use crate::parser::next_metavar;

type R<T> = Result<T, ()>;

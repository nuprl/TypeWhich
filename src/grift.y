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
  | '(' 'define' '(' id                  ')'         exps ')' { Toplevel::Define($4, next_metavar(), Exp::Fun("__ignored".to_string(), Typ::Unit, Box::new(Exp::begin($6)))) }
  | '(' 'define' '(' id                  ')' ':' typ exps ')' { Toplevel::Define($4, next_metavar(), Exp::Fun("__ignored".to_string(), Typ::Unit, Box::new(Exp::Ann(Box::new(Exp::begin($8)), $7)))) }
  | '(' 'define' '(' id nonempty_formals ')'         exps ')' { Toplevel::Define($4, next_metavar(), Exp::funs($5, Exp::begin($7))) } 
  | '(' 'define' '(' id nonempty_formals ')' ':' typ exps ')' { Toplevel::Define($4, next_metavar(), Exp::funs($5, Exp::Ann(Box::new(Exp::begin($9)), $8))) } 
;

exps -> Vec<Exp> :
      exps exp { let mut v = $1; v.push($2); v }
    | exp      { let mut v = Vec::new(); v.push($1); v }
;

exp -> Exp :
      lit { Exp::Lit($1) }
    | id  { Exp::Var($1) }

    | '(' ':' exp typ ')'     { Exp::Ann(Box::new($3), $4) }
    | '(' 'ann' exp typ ')'   { Exp::Ann(Box::new($3), $4) }
    | '(' ':' exp typ str ')' { Exp::Ann(Box::new($3), $4) } // TODO(mmg): store blame label somewhere?

    | '(' 'let'    bindings exps ')' { Exp::lets($3, Exp::begin($4)) }
    | '(' 'letrec' bindings exps ')' { 
      Exp::LetRec(
        $3.into_iter().map(|(x,to,e)| (x, to.unwrap_or_else(|| next_metavar()), e)).collect(), 
        Box::new(Exp::begin($4)),
      )
    }

    | '(' 'lambda' formals ':' typ exps ')' { Exp::funs($3, Exp::Ann(Box::new(Exp::begin($6)), $5)) }
    | '(' 'lambda' formals         exps ')' { Exp::funs($3, Exp::begin($4)) }

    | '(' 'repeat' '(' id exp exp ')' exp exp ')' { 
      match $8 {
          Exp::App(e_id, e_rest) => {
            match *e_id {
              Exp::Var(id) => Exp::repeat($4, $5, $6, id, next_metavar(), *e_rest, $9) ,
              _ => panic!("repeat accumulator should be of the form '(id [: typ] exp)', found {} instead of id", e_id),
            }
          },
          _ => panic!("repeat accumulator should be of the form '(id [: typ] exp)'"),
        }
      }
    | '(' 'repeat' '(' id exp exp ')' '(' id ':' typ exp ')' exp ')'   { Exp::repeat($4, $5, $6, $9, $11, $12, $14) }
    | '(' 'repeat' '(' id exp exp ')' exp ')'                          { Exp::repeat($4, $5, $6, "_".to_string(), next_metavar(), Exp::Lit(Lit::Unit), $8) }

    | '(' 'switch' exp switch_cases '(' 'else' exp ')' ')' { Exp::switch($3, $4, $7) }
    | '(' 'switch' exp              '(' 'else' exp ')' ')' { Exp::switch($3, Vec::new(), $6) }

    | '(' 'if' exp exp exp ')' { Exp::If(Box::new($3), Box::new($4), Box::new($5)) }

    | '(' 'cond' cond_cases ')' { let (cases,default) = $3; Exp::cond(cases, default) }

    | '(' 'begin' exps ')' { Exp::begin($3) }

    | '(' '+' exp exp ')' { Exp::Add(Box::new($3), Box::new($4)) }
    | '(' '*' exp exp ')' { Exp::Mul(Box::new($3), Box::new($4)) }
    | '(' '=' exp exp ')' { Exp::IntEq(Box::new($3), Box::new($4)) }

    | '(' 'make-tuple' exps ')'    { Exp::pairs($3) }
    | '(' 'tuple-proj' exp pos ')' { let e = $3; e.proj($4) }

    | '(' 'box'   exp ')'      { Exp::Box(Box::new($3)) }
    | '(' 'unbox' exp ')'      { Exp::Unbox(Box::new($3)) }
    | '(' 'boxset' exp exp ')' { Exp::BoxSet(Box::new($3), Box::new($4)) }

    | '(' 'gbox'   exp ')'      { Exp::Box(Box::new($3)) }
    | '(' 'gunbox' exp ')'      { Exp::Unbox(Box::new($3)) }
    | '(' 'gboxset' exp exp ')' { Exp::BoxSet(Box::new($3), Box::new($4)) }

    | '(' 'mbox'   exp ')'      { Exp::Box(Box::new($3)) }
    | '(' 'munbox' exp ')'      { Exp::Unbox(Box::new($3)) }
    | '(' 'mboxset' exp exp ')' { Exp::BoxSet(Box::new($3), Box::new($4)) }

    | '(' 'vector'   exp exp ')'      { Exp::Vector(Box::new($3), Box::new($4)) }
    | '(' 'vectorref' exp exp ')'     { Exp::VectorRef(Box::new($3), Box::new($4)) }
    | '(' 'vectorset' exp exp exp ')' { Exp::VectorSet(Box::new($3), Box::new($4), Box::new($5)) }
    | '(' 'vectorlen' exp ')'         { Exp::VectorLen(Box::new($3)) }

    | '(' 'gvector'   exp exp ')'      { Exp::Vector(Box::new($3), Box::new($4)) }
    | '(' 'gvectorref' exp exp ')'     { Exp::VectorRef(Box::new($3), Box::new($4)) }
    | '(' 'gvectorset' exp exp exp ')' { Exp::VectorSet(Box::new($3), Box::new($4), Box::new($5)) }
    | '(' 'gvectorlen' exp ')'         { Exp::VectorLen(Box::new($3)) }

    | '(' 'mvector'   exp exp ')'      { Exp::Vector(Box::new($3), Box::new($4)) }
    | '(' 'mvectorref' exp exp ')'     { Exp::VectorRef(Box::new($3), Box::new($4)) }
    | '(' 'mvectorset' exp exp exp ')' { Exp::VectorSet(Box::new($3), Box::new($4), Box::new($5)) }
    | '(' 'mvectorlen' exp ')'         { Exp::VectorLen(Box::new($3)) }

    | '(' exps ')' { Exp::apps($2) }
;

bindings -> Vec<(String, Option<Typ>, Exp)> :
      '(' ')'                   { Vec::new() }
    | '(' nonempty_bindings ')' { $2 }
    ;

nonempty_bindings -> Vec<(String, Option<Typ>, Exp)> :
      nonempty_bindings binding { let mut v = $1; v.push($2); v }
    | binding                   { let mut v = Vec::new(); v.push($1); v }
;

binding -> (String, Option<Typ>, Exp) :
      '(' id ':' typ exp ')' { ($2, Some($4), $5) }
    | '(' id         exp ')' { ($2, None, $3) }
;

formals -> Vec<(String, Typ)> :
      '(' ')'                  { vec![("_".to_string(), next_metavar())] }
    | '(' nonempty_formals ')' { $2 }
;

nonempty_formals -> Vec<(String, Typ)> :
     nonempty_formals formal { let mut v = $1; v.push($2); v } 
   |                  formal { let mut v = Vec::new(); v.push($1); v }
;

formal -> (String, Typ) :
        id             { ($1, next_metavar()) }
  | '(' id ':' typ ')' { ($2, $4) }
;

repeat_acc -> (Typ, Exp) :
    ':' typ exp { ($2, $3) }
  | exp         { (next_metavar(), $1) }
;

switch_cases -> Vec<(Vec<i32>, Exp)> :
    switch_cases switch_case { let mut v = $1; v.push($2); v }
  |              switch_case { let mut v = Vec::new(); v.push($1); v }
;

switch_case -> (Vec<i32>, Exp) :
  '(' '(' ints ')' exp ')' { ($3, $5) }
;

cond_cases -> (Vec<(Exp,Exp)>, Exp) :
    cond_case cond_cases  { let (mut v, e) = $2; v.insert(0, $1); (v,e) }
  | '(' 'else' exp ')'    { (Vec::new(), $3) }
;

cond_case -> (Exp, Exp) :
  '(' exp exp ')' { ($2, $3) }
;

typs -> Vec<Typ> :
    typs typ  { let mut v = $1; v.push($2); v }
  | typ       { let mut v = Vec::new(); v.push($1); v }
  ;

typ -> Typ :
    '(' '->' typs ')'     { Typ::arrs($3) } 
  | '(' typs '->' typ ')' { let mut args = $2; args.push($4); Typ::arrs(args) } 
  | '(' 'List' typ ')'    { Typ::List(Box::new($3)) }
  | '(' 'Ref' typ ')'     { Typ::Box(Box::new($3)) }
  | '(' 'GRef' typ ')'    { Typ::Box(Box::new($3)) }
  | '(' 'MRef' typ ')'    { Typ::Box(Box::new($3)) }
  | '(' 'Vect' typ ')'    { Typ::Vect(Box::new($3)) }
  | '(' 'GVect' typ ')'   { Typ::Vect(Box::new($3)) }
  | '(' 'MVect' typ ')'   { Typ::Vect(Box::new($3)) }
  | '(' 'Tuple' typs ')'  { Typ::tuples($3) } 
  | 'Dyn'                 { Typ::Any }
  | 'Int'                 { Typ::Int }
  | 'Float'               { Typ::Float }
  | 'Bool'                { Typ::Bool }
  | 'Char'                { Typ::Char }
  | '(' ')'               { Typ::Unit }
  | 'Unit'                { Typ::Unit }
  | id                    { parser_warning(format!("Treating type variable {} as Dyn.", $1)); Typ::Any }
  | '(' 'Rec' id typ ')'  { $4 }
  ;

lit -> Lit :
    i32     { Lit::Int($1) }
  | f64     { Lit::Float($1) }
  | bool    { Lit::Bool($1) }
  | str     { Lit::Str($1) }
  | char    { Lit::Char($1) }
  | '(' ')' { Lit::Unit }
  ;

f64 -> f64 :
    'FLOscheme' { 
      let span = $1.unwrap().span();
      let str = $lexer.span_str(lrpar::Span::new(span.start() + 2, span.end()));
      str.parse::<f64>().unwrap()
    }
  | 'FLO' { $lexer.span_str($1.unwrap().span()).parse::<f64>().unwrap() }
  ;

ints -> Vec<i32> :
    ints i32 { let mut v = $1; v.push($2); v }
  | i32      { let mut v = Vec::new(); v.push($1); v }
;

i32 -> i32 :
    'NUM' { $lexer.span_str($1.unwrap().span()).parse::<i32>().unwrap() }
    ;

// WEIRD error trying to use the lexer to just not allow `-` at the front...
// Err(Lexeme { start: 14, len: 4294967295, tok_id: 32 })
pos -> u32 :
    'NUM' { $lexer.span_str($1.unwrap().span()).parse::<u32>().unwrap_or_else(|err| panic!("tuple indices must be non-negative: {:?}", err)) }
    ;

char -> char :
    'CHR'    { let span = $1.unwrap().span(); 
                let str = $lexer.span_str(lrpar::Span::new(span.start() + 2, span.end()));
                str.chars().next().unwrap()
              }
  | 'CHRnul'     { '\0' }
  | 'CHRnewline' { '\n' }
  | 'CHRspace'   { ' ' }
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
use crate::parser::{next_metavar, parser_warning};

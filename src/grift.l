%%
[\t\r\n ]+ ;
;[^\n]* ;
\#f "false"
\#t "true"
[\(\[] "("
[\)\]] ")"
\(\) "()"
define "define"
lambda "lambda"
repeat "repeat"
begin "begin"
ann "ann"
if "if"
let "let"
letrec "letrec"
\+ "+"
\* "*"
= "="
box "box"
unbox "unbox"
box-set! "boxset"
: ":"
-> "->"
Dyn "Dyn"
Int "Int"
Float "Float"
Bool "Bool"
List "List"
Ref  "Ref"
Vect "Vect"
Tuple "Tuple"
Rec "Rec"
-?[0-9]+ "NUM"
"[^"]*" "STR"
[A-Za-z_\$][A-Za-z_\$0-9]* "ID"
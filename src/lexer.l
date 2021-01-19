%%
[\t\r\n ]+ ;
//[^\n]* ;
fun "fun"
false "false"
true "true"
\+ "+"
\. "."
\( "("
\) ")"
-?[0-9]+ "INT"
"[^"]*" "STR"
[A-Za-z_\$][A-Za-z_\$0-9]* "ID"

structure T = Tokens
type lexresult = T.t

(* val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n") *)
val error = fn x => raise (Fail x)
val eof = fn () => T.EOF

fun d2n(a, []) = 0
  | d2n(a, x::xs) = a*(ord(x) - ord(#"0")) + d2n(10*a, xs)

%%

%structure ArithLex

ws=[ \n\t] ;
digit=[0-9] ;

%%

{ws}+           => (lex()) ;
";"             => (T.EOS) ;
"~"             => (T.Neg) ;
"\+"             => (T.Plus) ;
"\*"             => (T.Times) ;
"("             => (T.LParen) ;
")"             => (T.RParen) ;
{digit}+        => (T.Number(d2n (1, rev(explode yytext)))) ;
"."             => (error ("Illegal character: " ^ yytext)) ;

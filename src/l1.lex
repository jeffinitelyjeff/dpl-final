structure T = Tokens
type lexresult = T.t

structure Substring = Substring

val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val eof = fn () => T.EOF

(* d2n xs is the number described by xs interpreted as a base-10
 * numeral in reverse.  Pre-condition:  each element of xs
 * is in the range "0"-"9".
 *)
fun d2n xs =
let
    fun d2n'(a, []) = 0
      | d2n'(a, x::xs) = a*(ord(x) - ord(#"0")) + d2n'(10*a, xs)
in
    d2n'(1, xs)
end

(* trim s left right is the result of deleting the first left (right)
 * characters along with all following (preceding) whitespace from
 * the beginning (end) of s.
 *)
fun trim s left right =
let
    fun isws c = (c = #"\n" orelse c = #" " orelse c = #"\t")
    val sl = Substring.triml left (Substring.full s)
    val sr = Substring.trimr right sl
    val sl' = Substring.dropl isws sr
    val sr' = Substring.dropr isws sl'
in
    Substring.string sr'
end

%%

%structure L1Lex

alpha=[A-Za-z] ;
digit=[0-9] ;
ws=[\n\ \t] ;

%%

{ws}+                           => (lex()) ;
"~"                             => (T.Unop(Ast.NEG)) ;
"+"                             => (T.Binop(Ast.PLUS)) ;
"-"                             => (T.Binop(Ast.SUB)) ;
"*"                             => (T.Binop(Ast.TIMES)) ;
"/"                             => (T.Binop(Ast.DIV)) ;
"andalso"                       => (T.Binop(Ast.AND)) ;
"orelse"                        => (T.Binop(Ast.OR)) ;
"not"                             => (T.Unop(Ast.NOT)) ;
"<"                             => (T.Binop(Ast.LT)) ;
"<="                            => (T.Binop(Ast.LE)) ;
">"                             => (T.Binop(Ast.GT)) ;
">="                            => (T.Binop(Ast.GE)) ;
"="                             => (T.Binop(Ast.EQ)) ;
"!="                            => (T.Binop(Ast.NE)) ;
"fn"{ws}+{alpha}{ws}*"=>"       => (T.Lambda (trim yytext 2 2)) ;
"("                             => (T.LParen) ;
")"                             => (T.RParen) ;
";"                             => (T.EOS) ;
"if"                            => (T.If) ;
"then"                          => (T.Then) ;
"else"                          => (T.Else) ;
"fi"                            => (T.Endif) ;
"true"                          => (T.True) ;
"false"                         => (T.False) ;
"[]"                            => (T.Nil) ;
"nil"                           => (T.Nil) ;
"::"                            => (T.Binop(Ast.CONS)) ;
"hd"                            => (T.Unop(Ast.HEAD)) ;
"tl"                            => (T.Unop(Ast.TAIL)) ;
"val"{ws}+{alpha}+{ws}*"="      => (T.Assign (trim yytext 3 1)) ;
{alpha}+                        => (T.Ident(yytext)) ;
{digit}+                        => (T.Num(d2n (rev(explode yytext)))) ;


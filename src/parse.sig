(* Signature for parsers for language L1.
*  
*  N. Danner
*  COMP 321.
*)
signature PARSE =
sig

    (* Raised when an error occurs during parsing. *)
	exception parse_error of string ;

    (* parse_expression lexer is the AST for the expression defined
    *  by the tokens yielded by lexer up to the first Tokens.EOS token.
    *)
	val parse_expression : (unit -> Tokens.t) -> Ast.expr

    (* parse_program lexer is the Ast.pgm for the program defined
    *  by the tokens yielded by lexer up to the Tokens.EOF token.
    *)
	val parse_program : (unit -> Tokens.t) -> Ast.pgm

end

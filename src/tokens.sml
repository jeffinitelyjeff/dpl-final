(* Tokens for language L1.
*  
*  N. Danner
*  COMP 321
*)

structure Tokens =
struct

  (* The type of identifiers. *)
  type ident = string

  (* The various tokens.  Rather than have a separate token for each
  *  operation symbol, we have Unop and Binop tokens that take as data
  *  the actual operation (cf. Ast.UnOp, Ast.BinOp).
  *)
  datatype t =
      Ident of string |
      Num of int |
      Unop of Ast.unop |
      Binop of Ast.binop |
      True | False |
      Nil | Cons | 
      If | Then | Else | Endif |
      Lambda of ident |
      LParen | RParen |
      Assign of ident |
      EOS | EOF 

  (* tok2str tok is a string representation of the token tok. *)
  fun tok2str(Ident(x)) = "IDENT(" ^ x ^ ")"
    | tok2str(Num(n)) = Int.toString(n)
    | tok2str(Unop(rator)) = Ast.unop2str(rator)
    | tok2str(Binop(rator)) = Ast.binop2str(rator)
    | tok2str(True) = "true"
    | tok2str(False) = "false"
    | tok2str(Nil) = "[]"
    | tok2str(Cons) = "cons"
    | tok2str(If) = "if"
    | tok2str(Then) = "then"
    | tok2str(Else) = "else"
    | tok2str(Endif) = "fi"
    | tok2str(Lambda(x)) = "fn " ^ x ^ " => "
    | tok2str(LParen) = "("
    | tok2str(RParen) = ")"
    | tok2str(Assign(x)) = x ^ " := "
    | tok2str(EOS) = "<EOS>"
    | tok2str(EOF) = "<EOF>"

end

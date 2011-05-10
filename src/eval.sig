(* Signature for evaluators for language L1.
*  
*  N. Danner
*  COMP 321
*)

signature EVAL =
sig

  (* The type of the value returned by the evaluation functions. *)
  type value

  (* eval_expr e is the value to which the expression e evaluates. *)
  val eval_expr : Ast.expr -> value

  (* eval_pgm p is the value to which the program p evaluates. *)
  val eval_pgm : Ast.pgm -> value

  (* values2ast v is the AST corresponding to the value v. *)
  val value2ast : value -> Ast.expr

end

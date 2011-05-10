(* Abstract syntax trees for language L1.
*  
*  COMP 321
*  N. Danner
*)
structure Ast =
struct

  (* Rather than have an AST constructor for each operation, we will have
  *  UnOp and BinOp constructors, which take a unop or binop argument
  *  to identify the operation, and then ASTs for the arguments.
  *)
  datatype unop = NEG | NOT | HEAD | TAIL
  datatype binop = PLUS | SUB | TIMES | DIV | 
                   LT | LE | GT | GE | EQ | NE | AND | OR |
                   CONS ;

  type ident = string
  datatype expr = Ident of ident | 
    Number of int | Boolean of bool | 
    UnOp of unop*expr | BinOp of binop*expr*expr | 
    NilList |
    Cond of expr*expr*expr |
    Abs of ident*expr | App of expr*expr
  datatype stmt = Assign of ident*expr 
  datatype pgm = Program of (stmt list)


  (* String represetation of unary operations.  Intended for use by ast2str. *)
  fun unop2str(NEG) = "~"
    | unop2str(NOT) = "!"
    | unop2str(HEAD) = "hd"
    | unop2str(TAIL) = "tl"

  (* String representation of binary operations.  Intended for use by ast2str.
  *)
  fun binop2str(PLUS) = "+"
    | binop2str(SUB) = "-"
    | binop2str(TIMES) = "*"
    | binop2str(DIV) = "/"
    | binop2str(LT) = "<"
    | binop2str(LE) = "<="
    | binop2str(GT) = ">"
    | binop2str(GE) = ">="
    | binop2str(EQ) = "="
    | binop2str(NE) = "!="
    | binop2str(AND) = "&&"
    | binop2str(OR) = "||"
    | binop2str(CONS) = "::"

  (* ast2str : expr -> string
  *  ast2str e is the string representation of ASTs. *)
  fun ast2str(Ident(x)) = x
    | ast2str(Number(n)) = Int.toString(n)
    | ast2str(Boolean(b)) = if b then "true" else "false"
    | ast2str(UnOp(rator, rand)) =
        "(" ^ unop2str(rator) ^ " " ^ ast2str(rand) ^ ")"
    | ast2str(BinOp(rator, rand1, rand2)) =
        "(" ^ ast2str(rand1) ^ " " ^ binop2str(rator) ^ " " ^ ast2str(rand2) ^
        ")"
    | ast2str(NilList) = "[]"
    | ast2str(Cond(a, t, f)) =
        "if " ^ ast2str(a) ^ " then " ^ ast2str(t) ^ " else " ^ ast2str(f) ^
        "endif"
    | ast2str(Abs(v, body)) =
        "(" ^ "fn " ^ v ^ " => " ^ ast2str(body) ^ ")"
    | ast2str(App(rator, rand)) =
        "(" ^ ast2str(rator) ^ ")(" ^ ast2str(rand) ^ ")"
end

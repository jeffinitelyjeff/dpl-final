structure Ast =
struct

  datatype t = Number of int | Neg of t | Plus of t*t | Times of t*t | Spot

  fun ast2str (Number(n)) = Int.toString(n)
    | ast2str (Neg(a)) = "~(" ^ (ast2str a) ^ ")"
    | ast2str (Plus(left, right)) = 
            "(" ^ (ast2str left) ^ " + " ^ (ast2str right) ^ ")"
    | ast2str (Times(left, right)) = 
            "(" ^ (ast2str left) ^ " * " ^ (ast2str right) ^ ")"


end

structure Eval
struct

  structure A = Ast
  type 'a value = 'a

  fun eval_expr expr = case expr of A.Number(n) => n

  fun eval_pgm (hd::tl) = eval_expr hd
    
  fun values2ast v = A.Number(v)

end

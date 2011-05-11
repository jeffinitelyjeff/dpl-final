structure Eval
struct

  structure A = Ast

  type valenv = A.ident -> valexp

  datatype value = Number of int*valenv
                 | Bool of bool*valenv
                 | Abs of A.ident*A.expr*valenv
                 | Nil of valenv
                 | Cons of value*value
  datatype closure = Closure of A.expr*valenv

  fun env_update env x v =
    case env of
      valenv => (fn y => if y=x then v else (env y))
    | _ => raise Fail("Update env called on non-environment")
    end
                           
  val empty_env = fn _ => raise Fail("Accessing an empty environment")
                           
  fun eval_clos (Closure(A.Number(n), _)) = Number(n, empty_env)
    | eval_clos (Closure(A.Boolean(b), _)) = Bool(b, empty_env)
    | eval_clos (Closure(A.UnOp(NEG,e), env)) =

        
                                                            
  fun eval_expr e = eval_clos e empty_env

  fun pgm_xfrm (A.Program(A.Assign(x,e)::[])) = e
    | pgm_xfrm (A.Program(A.Assign(x,e)::tl)) = A.App(A.Abs(x, pgm_xfrm tl), e)
    | pgm_xfrm _ = raise Fail("Not a proper program")
  
  fun eval_pgm p = eval_expr (pgm_xfrm p)

  fun values2ast (Number(n,_)) = A.Number(n)
    | values2ast (Boolean(b,_)) = A.Boolean(b)
    | values2ast (Abs(i,e,env)) = (* FIXME, this needs to actually go in and read
                                 the environment and fill in values. lame. *)
    | values2ast (Nil(_)) = A.NilList
    | values2ast (Cons(v0,v1)) = A.BinOp(CONS, values2ast v0, values2ast v1)
    | values2ast _ = raise Fail("Not a proper value; can't convert to AST")
end
